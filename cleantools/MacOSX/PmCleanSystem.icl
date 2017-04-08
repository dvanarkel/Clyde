implementation module PmCleanSystem

// Modified for Clyde to include Linker arguments and enable PostLink actions

import StdEnv,StdMisc,StdMaybe
from Directory import pd_StringToPath,getFileInfo,createDirectory,::Path,::FileInfo{pi_fileInfo},::PI_FileInfo{isDirectory},::DirError(..)
import UtilStrictLists
import StdPathname
import PmTypes
from PmCompilerOptions import ::CompilerOptions(..),::ListTypes(..),instance == ListTypes
import PmPath
//import PmCallBack
from PmParse import IsTypeSpec,IsImportError20
//from linkargs import ReadLinkErrors,WriteLinkOpts,:: LinkInfo`(..),:: LPathname

//from Platform import TempDir
//tooltempdir =: "/tmp"

import Clyde.Process

:: CompileOrCheckSyntax = SyntaxCheck | Compilation

:: CompilePollCompletedResult = NoFinishedCompiler | UnknownFinishedCompiler | FinishedCompiler !Int !Int
:: CodeGenerateAsmOrCode = AsmGeneration | CodeGeneration

:: CompilingInfo = NoCompiler | CompilerProcess !CompilerProcess

:: CompilerProcess = {
	commands_file_name :: !{#Char},
	results_file_name :: !{#Char},
	compiler_pid :: !Int,
	commands_fd :: !Int,
	results_fd :: !Int
  }

:: CompilerProcessIds :== [CompilerProcess]

::	CompilerMsg
	= 	CompilerOK
	| 	SyntaxError
	| 	Patherror Pathname

::	WindowFun env :== ([String]) -> env -> env

instance == CompileOrCheckSyntax where
	(==) SyntaxCheck SyntaxCheck = True
	(==) SyntaxCheck _ = False
	(==) Compilation Compilation = True
	(==) Compilation _ = False

instance == CodeGenerateAsmOrCode where
	(==) CodeGeneration CodeGeneration = True
	(==) AsmGeneration AsmGeneration = True
	(==) _ _ = False

NoCompilerProcessIds :: CompilerProcessIds
NoCompilerProcessIds = []

standardStaticLibraries :: !Processor !LinkMethod -> List String
standardStaticLibraries _ method
	= Nil

standardObjectFiles :: !Bool !Bool !Processor !Bool -> List String
standardObjectFiles stack_traces profiling _ use_64_bit_processor
	#! startup_file = if stack_traces "_startupTrace.o"
					  (if profiling "_startupProfile.o" "_startup.o")
	= (startup_file :! "_system.o" :! Nil)

getLibs :: ![String] !*Files -> (!(![String],![String]),!*Files)
getLibs [] files = (([],[]),files)
getLibs libs files = abort "getLibs"

InitCompilingInfo :: *CompilingInfo
InitCompilingInfo = NoCompiler

Compile :: !String !Bool !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !ModuleDirAndName !Pathname
	!(List Pathname) !ProjectCompilerOptions !CompilerOptions !Pathname !CompilerProcessIds !*env
	-> (!Pathname,!CompilerMsg,!CompilerProcessIds,!*env)
	| FileEnv env
Compile
	cocl use_compiler_process_ids write_module_times errwin typewin compileOrCheckSyntax mdn path paths project_compiler_options
	co=:{CompilerOptions | listTypes}
	startupdir compiler_process_ids env
	# (cocl,cocl_dir,options1,options2) = get_path_name_and_options2 cocl startupdir

	# temp_dir = temp_dir_path startupdir
	  out_file_name = out_file_path temp_dir 0
	  errors_file_name = errors_file_path temp_dir 0

	# cocl_arguments = compiler_arguments out_file_name errors_file_name compileOrCheckSyntax path paths write_module_times project_compiler_options.pco_memory_profiling project_compiler_options.pco_time_profiling project_compiler_options.pco_link_dynamic co
	  cocl_arguments = add_options_string_to_args 0 options2 cocl_arguments
	  cocl_arguments = add_options_string_to_args 0 options1 cocl_arguments

// so, provided that cocl is an absolute path...
	  result	= accUnsafe ('SP'.callProcess cocl cocl_arguments Nothing)
	| isError result
		#	(err_code,err_msg)	= fromError result
		= abort ("Compile failed: ("+++toString err_code+++") "+++err_msg)
	# exitCode = fromOk result
	# exitcode = if (exitCode==0) 0 1
	# dummy_slot = 0
	# (path,mess,env) =	CompileHandleExitCode exitcode cocl startupdir dummy_slot errwin typewin mdn listTypes env
	= (path,mess,compiler_process_ids,env)

CompilePersistent ::
	!String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !ModuleDirAndName
	!(List Pathname) !ProjectCompilerOptions !CompilerOptions !Pathname !*CompilingInfo !*env
	-> (!*CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env
CompilePersistent cocl write_module_times errwin typewin compileOrCheckSyntax mdn paths project_compiler_options co=:{CompilerOptions | listTypes}
					startupdir compiling_info env
	# (cocl,cocl_dir,options1,options2) = get_path_name_and_options2 cocl startupdir

	# (compiling_info=:CompilerProcess {commands_fd,results_fd}) = start_compiler_if_not_yet_started cocl compiling_info

	# temp_dir = temp_dir_path startupdir
	  out_file_name = out_file_path temp_dir 0
	  errors_file_name = errors_file_path temp_dir 0

	# args = makeCompilerOptionsArguments compileOrCheckSyntax write_module_times project_compiler_options.pco_memory_profiling project_compiler_options.pco_time_profiling project_compiler_options.pco_link_dynamic co
	# args_string = concat_args args+++
		" "+++mdn.mdn_name+++
		" -P "+++"\""+++concatenate_paths paths+++"\""+++
		" -RE "+++"\""+++errors_file_name+++"\""+++
		" -RO "+++"\""+++out_file_name+++"\""
	# args_string = if (size options2==0) args_string (options2+++" "+++args_string)
	# args_string = if (size options1==0) args_string (options1+++" "+++args_string)
	# args_string = "cocl "+++args_string+++"\n"
	  n_chars = size args_string
	# r = write commands_fd args_string n_chars
	| r<>n_chars
		= abort "write_failed"

	# result_string = createArray 6 '\0'
	# r = read results_fd result_string 6;
	| r<=1
		= abort ("read failed "+++toString r)

	# exitcode = parse_result_number 0 result_string
	
	# dummy_slot = 0
	# (path,mess,env) =	CompileHandleExitCode exitcode cocl startupdir dummy_slot errwin typewin mdn listTypes env
	= (compiling_info,(env,path,mess))

parse_result_number i result_string
	# c=result_string.[i]
	| c>='0' && c<='9'
		= parse_result_number2 (i+1) (toInt c-48) result_string
	| c=='-'
		= 0 - parse_result_number (i+1) result_string
		= abort "parse_result_number failed"
  where
	parse_result_number2 i n result_string
		| i<size result_string
			# c=result_string.[i]
			| c>='0' && c<='9'
				= parse_result_number2 (i+1) (n*10+(toInt c-48)) result_string
			| c=='\n'
				= n
				= abort "parse_result_number2 failed"
			= abort "parse_result_number2 failed"

CompileStartCommand ::
	!String !Bool !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Int !ProjectCompilerOptions !CompilerOptions !Pathname !CompilerProcessIds !*env
	-> (!Bool,!CompilerProcessIds,!*env)
	| FileEnv env
CompileStartCommand cocl write_module_times errwin compileOrCheckSyntax path
		paths slot project_compiler_options co startupdir compiler_process_ids ps
	# (cocl,cocl_dir,options1,options2) = get_path_name_and_options2 cocl startupdir

	# ({commands_fd,results_fd},compiler_process_ids)
		= start_a_compiler_if_not_yet_started cocl slot compiler_process_ids

	# temp_dir = temp_dir_path startupdir
	  out_file_name = out_file_path temp_dir slot
	  errors_file_name = errors_file_path temp_dir slot

	# args = makeCompilerOptionsArguments compileOrCheckSyntax write_module_times project_compiler_options.pco_memory_profiling project_compiler_options.pco_time_profiling project_compiler_options.pco_link_dynamic co
	# args_string = concat_args args+++
		" "+++path+++
		" -P "+++concatenate_paths paths+++
		" -RE "+++errors_file_name+++
		" -RO "+++out_file_name
	# args_string = if (size options2==0) args_string (options2+++" "+++args_string)
	# args_string = if (size options1==0) args_string (options1+++" "+++args_string)
	# args_string = "cocl "+++args_string+++"\n"
	  n_chars = size args_string
	# r = write commands_fd args_string n_chars
	| r<>n_chars
		= abort "write_failed"
	= (True,compiler_process_ids,ps)

exit_clean_compiler {compiler_pid,commands_fd,results_fd,commands_file_name,results_file_name} env
	# args_string = "quit\n"
	  n_chars = size args_string
	# r = write commands_fd args_string n_chars
	| r<>n_chars
		= abort "write_failed"
	# result	= accUnsafe (waitForProcess {pid=compiler_pid})
	| isError result
		#	(err_code,err_msg)	= fromError result
		= abort ("Compile failed: ("+++toString err_code+++") "+++err_msg)
	# r=close commands_fd
	| r==(-1)
		= abort "close failed"
	# r=close results_fd
	| r==(-1)
		= abort "close failed"
	| unlink (commands_file_name+++"\0")<>0
		= abort "unlink failed"
	| unlink (results_file_name+++"\0")<>0
		= abort "unlink failed"
	= env

ExitCleanCompiler :: !*(!*CompilingInfo,*env) -> *(!*CompilingInfo,*env)
ExitCleanCompiler (NoCompiler,env)
	= (NoCompiler,env)
ExitCleanCompiler (CompilerProcess compiler,env)
	#! env = exit_clean_compiler compiler env
	= (NoCompiler,env)

QuitCleanCompiler :: !Bool !CompilerProcessIds !*World -> *World
QuitCleanCompiler async compiler_process_ids world
	| async
		= exit_compilers compiler_process_ids world
		= world
  where
	exit_compilers [compiler:compilers] world
		= exit_compilers compilers (exit_clean_compiler compiler world)
	exit_compilers [] world
		= world

CodeGen	::	!String !Bool !(WindowFun *GeneralSt) !CodeGenerateAsmOrCode !Pathname !Pathname !Bool
			!CodeGenOptions !Processor !ApplicationOptions !Pathname !CompilerProcessIds !*GeneralSt
			-> (!Pathname,!Bool,!CompilerProcessIds,!*GeneralSt)
CodeGen cgen used_compiler_process_ids wf genAsmOrCode abc_path obj_path timeprofile cgo tp ao startupdir compiler_process_ids ps
	# (cgen,cgendir,options) = get_path_name_and_options cgen startupdir
	# path_without_suffix = RemoveSuffix abc_path
	# cg_arguments = make_code_generator_arguments genAsmOrCode cgo

	# temp_dir = temp_dir_path startupdir
	  errors_file_name = errors_file_path temp_dir 0
	# stderr_fd = creat (errors_file_name+++"\0") 0777;
	| stderr_fd== (-1)
		= abort "creat failed"

	#	args	= add_options_string_to_args 0 options (cg_arguments++[path_without_suffix])
		res		= accUnsafe ( runProcessWithRedirect cgen args Nothing Nothing Nothing (Just stderr_fd) )
	| isError res
		#	(err_code,err_msg)	= fromError res
		= abort ("Code generation failed: ("+++toString err_code+++") "+++err_msg)
	#	pid		= fromOk res
	#	result	= accUnsafe (waitForProcess pid)//{pid=compiler_pid})
	| isError result
		#	(err_code,err_msg)	= fromError result
		= abort ("Code generation failed: ("+++toString err_code+++") "+++err_msg)
	# exitCode = fromOk result
	# exit_code = if (exitCode==0) 0 1

	# r=close stderr_fd
	| r==(-1)
		= abort "close failed"

	# ((_, errors_not_empty, error_text),ps) = accFiles (readErrorsAndWarnings errors_file_name) ps
	  ps = if errors_not_empty
				(wf (StrictListToList error_text) ps) 
				(if (exit_code <> 0)
					(wf ["Error: Code generator failed for '"+++abc_path+++"' with exit code: "+++toString exit_code,(quoted_string path_without_suffix)] ps)
					ps
					)

	| not CLEAN_VIA_ASM	
		= (obj_path,exit_code==0,compiler_process_ids,ps)
	| exit_code<>0
		= (obj_path,exit_code==0,compiler_process_ids,ps)

	# assembly_path_name = path_without_suffix+++".s"
	# assembler_path_name = "/usr/bin/as"

	#	args	= ["-o",obj_path,assembly_path_name]
		result	= accUnsafe ( callProcess assembler_path_name args Nothing)
	| isError result
		#	(err_code,err_msg)	= fromError result
		= abort ("Code generation failed (as): ("+++toString err_code+++") "+++err_msg)
	#	exitCode	= fromOk result
		exit_code	= if (exitCode==0) 0 1
	| unlink (assembly_path_name+++"\0")<>0
		= abort "deleting the assembler file failed"

	= (obj_path,exit_code==0,compiler_process_ids,ps)

:: StartedCodeGenerator = !{
	scg_abc_path :: !{#Char},
	scg_path_without_suffix :: !{#Char},
	scg_std_error_fd :: !Int,
	scg_errors_file_name :: !{#Char}
  }

start_code_generator ::	!String !(WindowFun *GeneralSt) !Pathname !Int !Bool !CodeGenOptions !Processor !Pathname !*GeneralSt
						-> (!Bool,!Int/*HANDLE*/,!StartedCodeGenerator,!*GeneralSt)
start_code_generator cgen wf abc_path slot timeprofile cgo tp startupdir ps
	# (cgen,cgendir,options) = get_path_name_and_options cgen startupdir
	# path_without_suffix = RemoveSuffix abc_path
	# cg_arguments = make_code_generator_arguments CodeGeneration cgo

	# temp_dir = temp_dir_path startupdir
	  errors_file_name = errors_file_path temp_dir slot
	# stderr_fd = creat (errors_file_name+++"\0") 0777;
	| stderr_fd== (-1)
		= abort "creat failed"

	#	args	= add_options_string_to_args 0 options (cg_arguments++[path_without_suffix])
		res	= accUnsafe (runProcessWithRedirect cgen args Nothing Nothing Nothing (Just stderr_fd))
	| isError res
		#	(err_code,err_msg)	= fromError res
		= abort ("Code generation failed: ("+++toString err_code+++") "+++err_msg)
	#	code_generator_pid		= (fromOk res).pid
	# scg = {scg_abc_path=abc_path,scg_path_without_suffix=path_without_suffix,
			 scg_std_error_fd=stderr_fd,scg_errors_file_name=errors_file_name}
	= (True,code_generator_pid,scg,ps)

wait_for_finished_code_generator :: !{#Int} !*GeneralSt -> (!Int,!Int,!*GeneralSt)
wait_for_finished_code_generator process_ids ps
	#	(process_n,res)	= accUnsafe (waitForAnyChild process_ids)
	| isError res
		= abort "wait_for_finished_code_generator failed"
	= (process_n,fromOk res,ps)

finish_code_generator :: !Int/*HANDLE*/ !StartedCodeGenerator !Int !(WindowFun *GeneralSt) !*GeneralSt -> (!Bool,!*GeneralSt)
finish_code_generator process_handle {scg_abc_path,scg_path_without_suffix,scg_std_error_fd,scg_errors_file_name} exit_code wf ps
	# r=close scg_std_error_fd
	| r==(-1)
		= abort "close failed"
	# ((_, errors_not_empty, error_text),ps) = accFiles (readErrorsAndWarnings scg_errors_file_name) ps
	  ps = if errors_not_empty
				(wf (StrictListToList error_text) ps) 
				(if (exit_code <> 0)
					(wf ["Error: Code generator failed for '" +++ scg_abc_path +++ "' with exit code: "+++toString exit_code,(quoted_string scg_path_without_suffix)] ps)
					ps)
	= (exit_code==0, ps)

from System._Pointer import ::Pointer,packString,derefString
//from System._Posix import getcwd,chdir
import StdDebug
DelayEventLoop :: !.ps -> .ps
DelayEventLoop ps
	= ps

POLLIN :== 1

CompilePollCompleted :: !CompilerProcessIds !*env -> (!CompilePollCompletedResult, !*env) | FileEnv env
CompilePollCompleted compiler_process_ids ps
	# n_processes = length compiler_process_ids
	  fda = createArray (IF_INT_64_OR_32 n_processes (n_processes<<1)) 0
	  fda = fill_fda compiler_process_ids 0 fda
	  r = poll fda n_processes -1
	| r<0
		= abort "poll failed"
	| r==0
		= (NoFinishedCompiler,ps)
		# compiler_id = find_fd_index 0 fda
		# results_fd = (compiler_process_ids !! compiler_id).results_fd
	
		# result_string = createArray 6 '\0'
		# r = read results_fd result_string 6;
		| r<=1
			= abort ("read failed "+++toString r)
		# exit_code = parse_result_number 0 result_string
	
		= (FinishedCompiler compiler_id exit_code,ps)
where
	fill_fda :: !CompilerProcessIds !Int !*{#Int} -> *{#Int}
	fill_fda [{results_fd}:process_ids] i fda
		# fda = IF_INT_64_OR_32
					{fda & [i] = results_fd + (POLLIN<<32)}
					{fda & [i] = results_fd, [i+1] = POLLIN}
		= fill_fda process_ids (i+(IF_INT_64_OR_32 1 2)) fda
	fill_fda [] i fda
		= fda

	find_fd_index i fda
		| i<size fda
			#! r_events = (fda.[IF_INT_64_OR_32 i ((i<<1)+1)] >> (IF_INT_64_OR_32 48 16)) bitand 0xffff;
			| r_events==0
				= find_fd_index (i+1) fda
			| r_events==POLLIN
				= i
				= abort ("polling file descriptors failed with event"+++toString r_events) 

O_RDONLY:==0;
O_WRONLY:==1;
O_RDWR:==2;

start_compiler :: !{#Char} -> *CompilerProcess
start_compiler compiler_file_name
	# commands_file_name = "/tmp/comXXXXXX" +++ "\0" // +++ because modified by mkstemp
	# fd = mkstemp commands_file_name
	| fd== -1
		= abort "mkstemp failed"
	# r = close fd
	| r== -1
		= abort "close failed"
	# r = unlink commands_file_name
	| r== -1
		= abort "unlink failed"
	# r = mkfifo commands_file_name (S_IRUSR bitor S_IWUSR)
	| r== -1
		= abort "mkfifo failed"

	# results_file_name = "/tmp/resXXXXXX" +++ "\0" // +++ because modified by mkstemp
	# fd = mkstemp results_file_name
	| fd== -1
		= abort "mkstemp failed"
	# r = close fd
	| r== -1
		= abort "close failed"
	# r = unlink results_file_name
	| r== -1
		= abort "unlink failed"
	# r = mkfifo results_file_name (S_IRUSR bitor S_IWUSR)
	| r== -1
		= abort "mkfifo failed"

	# cocl_arguments = ["--pipe",commands_file_name,results_file_name]

	#	res		= accUnsafe (runProcess compiler_file_name cocl_arguments Nothing)
	| isError res
		#	(err_code,err_msg)	= fromError res
		= abort ("compiler failed (exec): ("+++toString err_code+++") "+++err_msg)
	#	compiler_pid		= (fromOk res).pid
	# commands_fd = open (commands_file_name+++"\0") O_WRONLY 0
	| commands_fd == -1
		= abort "open failed"
	# results_fd = open (results_file_name+++"\0") O_RDONLY 0
	| results_fd == -1
		= abort "open failed"

	= {	commands_file_name=commands_file_name, results_file_name=results_file_name,
		compiler_pid=compiler_pid, commands_fd=commands_fd, results_fd=results_fd}

start_compiler_if_not_yet_started :: !{#Char} !*CompilingInfo -> *CompilingInfo
start_compiler_if_not_yet_started compiler_file_name NoCompiler
	= CompilerProcess (start_compiler compiler_file_name);
start_compiler_if_not_yet_started compiler_file_name compiling_info
	= compiling_info

start_a_compiler_if_not_yet_started compiler_file_name slot compiler_process_ids
	| slot<length compiler_process_ids
		= (compiler_process_ids !! slot,compiler_process_ids)
		# compiler_process = start_compiler compiler_file_name
		# compiler_process_ids = compiler_process_ids++[compiler_process]
		= (compiler_process,compiler_process_ids)

CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *env) !(WindowFun *env) !ModuleDirAndName
				!ListTypes !*env -> (!Pathname,!CompilerMsg,!*env) | FileEnv env
CompileHandleExitCode exitcode cocl startupdir slot errwin typewin mdn listTypes ps
	# temp_dir = temp_dir_path startupdir
	  out_file_name = out_file_path temp_dir slot
	  errors_file_name = errors_file_path temp_dir slot
	#	((type_text_not_empty,type_text),ps)
			= accFiles (readTypesInfo (listTypes<>NoTypes) out_file_name) ps
		((errors,errors_and_messages_not_empty,errors_and_messages),ps)
			= accFiles (readErrorsAndWarnings errors_file_name) ps
	| exitcode <> 0 && not errors_and_messages_not_empty
	  = ( ""
		, SyntaxError
		, errwin [  "Error: Compiler crashed: "+++cocl
				 : (case errors of CompilerOK -> ["Unable to open Errors file"] ; _ -> [])
				 ] ps
		)
	#	abcpath			= ModuleDirAndNameToABCSystemPathname mdn
		ps				= (if type_text_not_empty (typewin (StrictListToList type_text)) id) ps
		ps				= (if errors_and_messages_not_empty (errwin (StrictListToList errors_and_messages)) id) ps
		errors			= case exitcode of
							0	-> CompilerOK
							_	-> errors
     = (abcpath,errors,ps)
where
	readTypesInfo :: !Bool !Pathname !*Files -> ((!Bool,!(List String)),!*Files)
	readTypesInfo readtypes	path env
		| not readtypes
			= ((False,Nil),env)
		# (opened,file,env) = fopen path FReadText env
		| not opened
			= ((False,Nil),env)
		# (typelist,types_read,file`) = readTypeMsg file
		  (_,env) = fclose file` env
		= ((types_read,typelist),env)

	readTypeMsg :: !*File -> (!List String,!Bool,!*File)
	readTypeMsg file
		#	(string,file)					= freadline file
			(eof,file)						= fend file
		| eof && IsTypeSpec string
			= (strip_newlines string :! Nil,True,file)
		| eof
			= (Nil,False,file)
		#	(typeslist,types_read,file)	= readTypeMsg file
		= (strip_newlines string :! typeslist,types_read,file)

readErrorsAndWarnings :: !Pathname !*Files -> ((!CompilerMsg, !Bool, !(List String)), !*Files)
readErrorsAndWarnings path env
	# (opened,file,env) = fopen path FReadText env
	| not opened
		= ((CompilerOK,False,Nil),env)
	# (errors,errors_and_warnings_read,errlist,file`) = readErrorAndWarningMessages file
	  (_,env) = fclose file` env
	= ((errors,errors_and_warnings_read,errlist),env)
where
	readErrorAndWarningMessages :: !*File -> (!CompilerMsg,!Bool,!List String,!*File)
	readErrorAndWarningMessages file
		#!	(string, file1)					= freadline file
			(eof,file2)						= fend file1
			(is_import_error,path)			= IsImportError20 string
		| eof
			#!	not_empty_or_newline 		= (size string)<>0 && string.[0]<>'\n'
			= (if is_import_error (Patherror path) SyntaxError,not_empty_or_newline,strip_newlines string :! Nil,file2)
		#	(path_error,_,errlist,file3) = readErrorAndWarningMessages file2
		= (if is_import_error (Patherror path) path_error,True,strip_newlines string:!errlist,file3)

strip_newlines :: !{#Char} -> {#Char}
strip_newlines s
	| size s==0
		= s
		# last = dec (size s)
		  char = s.[last]
		| char == '\n' || char == '\r'
			= strip_newlines (s % (0,dec last))
		= s

temp_dir_path startupdir
	= startupdir +++ "/Temp"

out_file_path temp_dir slot
	| slot==0
		= temp_dir +++ "/out"
		= temp_dir +++ "/out"+++toString slot

errors_file_path temp_dir slot
	| slot==0
		= temp_dir +++ "/errors"
		= temp_dir +++ "/errors"+++toString slot

get_path_name_and_options2 ccstring startupdir
	# (ccstring,rem) = splitOptions ccstring
	  (opts,opts`) = splitOptions rem
	  cocl = startupdir +++ "/" +++ ccstring
	  cocldir = RemoveFilename cocl
	= (cocl,cocldir,opts,opts`)

get_path_name_and_options codegen_or_linker startupdir
	# (codegen_or_linker,opts) = splitOptions codegen_or_linker
	| size codegen_or_linker>0 && codegen_or_linker.[0]=='/'
		# codegen_or_linker_dir = RemoveFilename codegen_or_linker
		= (codegen_or_linker,codegen_or_linker_dir,opts)
		# codegen_or_linker = startupdir +++ "/" +++ codegen_or_linker
		# codegen_or_linker_dir = RemoveFilename codegen_or_linker
		= (codegen_or_linker,codegen_or_linker_dir,opts)

splitOptions :: !{#Char} -> (!{#Char},!{#Char})
splitOptions str
	# len_str = size str
	  first_q = FindQuoteChar str len_str 0
	| first_q >= len_str
		= (str,"")
		# first_str = str % (0,dec first_q)
		  last_str = str % (first_q+1, len_str)
		= (first_str,last_str)
where
	FindQuoteChar str len pos	= FindChar ':' str len pos;

	FindChar	:: !Char !.String !.Int !Int -> Int;
	FindChar c line linelen pos
		| pos >= linelen		=  pos;
		| c ==  line.[pos]		=  pos;
								=  FindChar c line linelen (pos+1);

add_options_string_to_args i s args
	# first_i = skip_spaces_and_tabs i s
	| first_i>=size s
		= args
		# end_i = skip_to_space_or_tab (i+1) s
		= [s % (first_i,end_i-1) : add_options_string_to_args end_i s args]
  where
	skip_spaces_and_tabs i s
		| i<size s
			# c=s.[i]
			| c==' ' || c=='\t'
				= skip_spaces_and_tabs (i+1) s
				= i
			= i

	skip_to_space_or_tab i s
		| i<size s
			# c=s.[i]
			| c==' ' || c=='\t'
				= i
				= skip_to_space_or_tab (i+1) s
			= i

compiler_arguments :: !String !String !CompileOrCheckSyntax !Pathname !(List Pathname) !Bool !Bool !Bool !Bool !CompilerOptions -> [String]
compiler_arguments out_file_name errors_file_name compileOrCheckSyntax path paths
					write_module_times projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	# args = makeCompilerOptionsArguments compileOrCheckSyntax write_module_times projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	= args ++ [path,"-P",concatenate_paths paths,"-RE",errors_file_name,"-RO",out_file_name];

concat_args [] = ""
concat_args [arg] = arg
concat_args [arg:args] = arg+++" "+++concat_args args

makeCompilerOptionsArguments :: !CompileOrCheckSyntax !Bool !Bool !Bool !Bool !CompilerOptions -> [String]
makeCompilerOptionsArguments compileOrCheckSyntax write_module_times projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
			{neverMemoryProfile,neverTimeProfile,sa,gw,gc,listTypes,attr,reuseUniqueNodes,fusion}
	= write_module_times_arg ++ checksyntax ++ timeProfileSwitch ++ memoryProfileSwitch ++ dynamicLinkSwitch
		++ strictness ++ warnings ++ comments ++ listtypes ++ show_attr ++ reuse ++ fusion_arg
where
	write_module_times_arg
		| write_module_times
			= ["-wmt"]
			= []
	memoryProfileSwitch
		| (not neverMemoryProfile && projectMemoryProfiling)
		|| projectEagerOrDynamic
			= ["-desc"]
			= []
	timeProfileSwitch
		| not neverTimeProfile && projectTimeProfiling
			= ["-pt"]
			= []
	dynamicLinkSwitch
		| projectEagerOrDynamic
			= ["-exl","-dynamics"]
			= []
	strictness
		| sa
			= []
			= ["-sa"]
	warnings
		| gw
			= []
			= ["-w"]
	comments
		| gc
			= ["-d"]
			= []
	listtypes
		| listTypes == InferredTypes
			= ["-lt"]
		| listTypes == AllTypes
			= ["-lat"]
		| listTypes == StrictExportTypes
			= ["-lset"]
			= []
	show_attr
		| attr
			= []
			= ["-lattr"]
	checksyntax
		| compileOrCheckSyntax == SyntaxCheck
			= ["-c"]
			= []
	reuse
		| reuseUniqueNodes
			= ["-ou"]
			= []
	fusion_arg
		| fusion
			= ["-fusion"]
			= []

import System.Environment, System._Unsafe
CLEAN_VIA_ASM =: True	// accUnsafe clean_via_asm	// not working??

clean_via_asm w
	#!	(r,w)	= getEnvironmentVariable "CLEAN_VIA_ASM" w
		res		= case r of
						Nothing	-> False
						(Just _)-> True
	= (res,w)

make_code_generator_arguments genAsmOrCode {ci,cs}
	= checkindex++checkstack++genasm
where
	checkindex	| ci = ["-ci"]; = []
	checkstack	| cs = ["-os"]; = []
	genasm		| genAsmOrCode == AsmGeneration || CLEAN_VIA_ASM
								= ["-a"]
								= []

concatenate_paths :: (List Pathname) -> String
concatenate_paths ss
	# s = createArray (sSize ss) ':'
	= sUpdate 0 s ss
where
	sSize Nil = 0
	sSize (string :! Nil) = size string
	sSize (string :! rest) = size string + 1 + sSize rest
	
	sUpdate i s Nil = s
	sUpdate i s (string :! Nil)
		# (_,s) = sU (size string) i 0 s string
		= s
	sUpdate i s (string :! rest)
		# (i,s) = sU (size string) i 0 s string
		# i = inc i
		= sUpdate i s rest
	
	sU l i j s h
		| j >= l = (i,s)
		# s = update s i h.[j]
		= sU l (inc i) (inc j) s h

ClearCompilerCache :: .a
ClearCompilerCache = abort "ClearCompilerCache"

ClearCompilerCaches :: .a
ClearCompilerCaches = abort "ClearCompilerCaches"

SendRepeatResult :: .a
SendRepeatResult = abort "SendRepeatResult"

StartCodeGenerator :: .a
StartCodeGenerator = abort "StartCodeGenerator"

import qualified Clyde.Process as SP
import IdeState
/*
 * we can push Execute to the application specific level by implementing it in IdeState...
 * note that it currently is just a simple wrapper around Process.callProcess, which is fine
 * for the usage in Precompile and Postlink phases
 * but _not_ for launching final application ('Run'), where we want Process.runProcess,
 * with varying i/o hookups for out of IDE or in IDE running (pipe or pty redirection) 
*/
Execute` ::	!String !*GeneralSt -> (!Bool,!Int,!*GeneralSt)
Execute` command ps=:{gst_world}
	#!	env			= gst_world
		path		= command
		args		= []
		(res,env)	= 'SP'.callProcess path args mCurrentDirectory env
		env			= {ps & gst_world = env}
	= case res of 
		'SP'.Ok ret		= (True,ret,env)
		'SP'.Error (e,m)
			= (False, e, env)
where
		mCurrentDirectory :: 'SP'.Maybe String
		mCurrentDirectory	= 'SP'.Nothing

S_IRUSR:==0x100
S_IWUSR:==0x080

mkstemp :: !{#Char} -> Int;
mkstemp temp_file_name = code {
	ccall mkstemp "s:I"
}

creat :: !{#Char} !Int -> Int;
creat string0 mode = code {
	ccall creat "sI:I"
}

open :: !{#Char} !Int !Int -> Int;
open path_name flags mode = code {
	ccall open "sII:I"
}

write :: !Int !{#Char} !Int -> Int;
write fd buffer count = code {
	ccall write "Isp:p"
}

read :: !Int !{#Char} !Int -> Int;
read fd buffer count = code {
	ccall read "Isp:p"
}

close :: !Int -> Int;
close fd = code {
	ccall close "I:I"
}

unlink :: !{#Char} -> Int;
unlink file_name = code {
	ccall unlink "s:I"
}

mkfifo :: !{#Char} !Int -> Int;
mkfifo name flags = code {
	ccall mkfifo "sI:I"
}

poll :: !{#Int} !Int !Int -> Int;
poll fds nfds timeout = code {
	ccall poll "AII:I"
}
