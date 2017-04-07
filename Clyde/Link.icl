implementation module Clyde.Link

import StdEnv
import System.FilePath
import Clyde.Process

from IdeState		import :: GeneralSt(..), :: General, getPrefs, :: Prefs(..), instance FileEnv GeneralSt, instance FileSystem GeneralSt
from PmCompilerOptions	import :: CompilerOptions
from PmProject	import :: Project
from PmTypes		import :: ApplicationOptions(..), :: Output(..), :: Processor

from PmProject	import PR_ExecUpToDate

from PmFileInfo	import :: FileInfoCache, YoungestObj
from UtilIO		import FModified
//from UtilDate		import :: DATE(..), Older_Date, NoDate
import UtilDate

from errwin		import updateErrorWindow
from messwin		import showInfo, :: InfoMessage(..)

Link ::	!String !Path !ApplicationOptions
		!Path ![!Path!] ![!Path!] ![!Path!] !Bool !Bool !Bool !Bool !Bool !String
		!Bool !String !Path !String !Processor !Bool !*GeneralSt
		 -> (!*GeneralSt,!Bool)
Link linker path
		applicationOptions
		optionspathname library_file_names object_file_names static_libraries static gen_relocs gen_symbol_table gen_linkmap
		link_resources resource_path gen_dll dll_syms startupdir dynlstr _ _ ps
	# (linker,linkerdir,options)= get_path_name_and_options linker startupdir
	# optdirpath				= takeDirectory optionspathname

	#	(errs,ps)				= ensureDirectory optdirpath ps
	| not (isEmpty errs)
		= (updateErrorWindow errs ps, False)

	# (options_file_ok,ps)		= accFiles (write_options_file optionspathname applicationOptions) ps
	| not options_file_ok
		= (updateErrorWindow ["Linker error: Could not write the options object file: "+++optionspathname] ps,False)

	# is_gcc	= linker == "/usr/bin/gcc"
/*
	# linkopts	=
		{ exe_path					= path
		, res_path					= resource_path
		, open_console				= o <> NoConsole
		, static_link				= static
		, gen_relocs				= gen_relocs
		, gen_symbol_table          = gen_symbol_table
		, gen_linkmap				= gen_linkmap
		, link_resources			= link_resources
		, object_paths				= optionspathname :! (RemoveDup object_file_names)
	  	, dynamic_libs				= RemoveDup library_file_names
	  	, static_libs				= RemoveDup static_libraries
	  	, stack_size				= ss
		, gen_dll					= gen_dll
		, dll_names					= dll_syms
		, dynamics_path				= startupdir +++. DirSeparatorString +++. dynlstr
		, lib_name_obj_path 		= MakeFullPathname tooltempdir "lib_name.o"
	  	}
	# linkoptspath = MakeFullPathname tooltempdir "linkopts"
	# linkerrspath = MakeFullPathname tooltempdir "linkerrs"
*/
	# (err,ps)		= //if is_gcc
						(Nothing,ps)
						//(accFiles (WriteLinkOpts linkoptspath linkopts) ps)
	| isJust err
		= (updateErrorWindow (fromJust err) ps,False)

	# linker_args = //if is_gcc
						(add_options_string_to_args 0 options ["-o", path: [optionspathname : strictListToList (removeDup object_file_names)]++strictListToList library_file_names])
					//	["-I",linkoptspath,"-O",linkerrspath]

	#	(res,ps)	= accWorld (runProcess linker linker_args (Just (optdirpath+++"/.."))) ps
	| isError res
		#	(err_code,err_msg)	= fromError res
		= abort ("Linking failed (exec): ("+++toString err_code+++") "+++err_msg)
	#	ld_pid		= (fromOk res)
		(res,ps)	= accWorld (waitForProcess ld_pid) ps
	| isError res
		#	(err_code,err_msg)	= fromError res
		= abort ("Linking failed (wait): ("+++toString err_code+++") "+++err_msg)
	# result = fromOk res
	# link_ok = result>=0
	//Read link errors
	# ((err,link_errors),ps) = //if is_gcc
								((Nothing,[]),ps)
								//(accFiles (ReadLinkErrors linkerrspath) ps)
	| isJust err
		= (updateErrorWindow (fromJust err) ps,False)
	# (errtext,errlines) = (link_errors, length link_errors)
	| errlines<>0
		= (updateErrorWindow errtext ps,link_ok)
	=  (ps,link_ok)

ApplicationOptionsToFlags :: !ApplicationOptions -> Int
ApplicationOptionsToFlags {sgc,pss,marking_collection,set,o,memoryProfiling,write_stderr_to_file,disable_rts_flags}
	= showgc+printstacksize+showexectime+cons+marking_collection_mask+memory_profiling_mask+write_stderr_to_file_mask+disable_rts_flags_mask
where
	showgc					
		| sgc	= 2
				= 0
	printstacksize			
		| pss	= 4
				= 0
	showexectime 
		| set	= 8 
				= 0
	write_stderr_to_file_mask
		| write_stderr_to_file	= 128 
								= 0
	marking_collection_mask 
		| marking_collection 	= 64  
								= 0	
	memory_profiling_mask
		| memoryProfiling	= 32  
							= 0
	cons
		= case o of
			BasicValuesOnly		-> 1
			ShowConstructors	-> 0
			NoReturnType		-> 16
			NoConsole			-> 16

	disable_rts_flags_mask
		| disable_rts_flags	= 8192
							= 0

(FWI) infixl
(FWI) f i :== fwritei i f

write_options_file :: !{#.Char} ! ApplicationOptions !*Files -> *(!Bool,!*Files)
write_options_file options_file_name applicationOptions files
	#	flags						= ApplicationOptionsToFlags applicationOptions
		initial_heap_size			= applicationOptions.initial_heap_size
		heap_size					= applicationOptions.hs
		stack_size					= applicationOptions.ss
		heap_size_multiple			= applicationOptions.heap_size_multiple
	# (opened,file,files) 
		= fopen options_file_name FWriteData files
	| not opened
		= (False,files)
	#! file = file FWI
			0xfeedfacf FWI 0x01000007 FWI 0x00000003 FWI 0x00000001 FWI
			0x00000003 FWI 0x00000150 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000019 FWI 0x000000e8 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000028 FWI 0x00000000 FWI 0x00000170 FWI 0x00000000 FWI
			0x00000028 FWI 0x00000000 FWI 0x00000007 FWI 0x00000007 FWI
			0x00000002 FWI 0x00000000 FWI 0x65745f5f FWI 0x00007478 FWI
			0x00000000 FWI 0x00000000 FWI 0x45545f5f FWI 0x00005458 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000170 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x80000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x61645f5f FWI 0x00006174 FWI
			0x00000000 FWI 0x00000000 FWI 0x41445f5f FWI 0x00004154 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000028 FWI 0x00000000 FWI 0x00000170 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000002 FWI 0x00000018 FWI
			0x00000198 FWI 0x00000005 FWI 0x000001e8 FWI 0x0000004c FWI
			0x0000000b FWI 0x00000050 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000005 FWI 0x00000005 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			0x00000000 FWI 0x00000000 FWI 0x00000000 FWI 0x00000000 FWI
			flags FWI 0x00000000 FWI initial_heap_size FWI 0x00000000 FWI
			heap_size FWI 0x00000000 FWI stack_size FWI 0x00000000 FWI
			heap_size_multiple FWI 0x00000000 FWI 0x00000026 FWI 0x0000020f FWI
			0x00000018 FWI 0x00000000 FWI 0x00000001 FWI 0x0000020f FWI
			0x00000000 FWI 0x00000000 FWI 0x0000001b FWI 0x0000020f FWI
			0x00000010 FWI 0x00000000 FWI 0x00000035 FWI 0x0000020f FWI
			0x00000020 FWI 0x00000000 FWI 0x00000008 FWI 0x0000020f FWI
			0x00000008 FWI 0x00000000 FWI 0x6c665f00 FWI 0x00736761 FWI
			0x696e695f FWI 0x6c616974 FWI 0x6165685f FWI 0x69735f70 FWI
			0x5f00657a FWI 0x70616568 FWI 0x7a69735f FWI 0x615f0065 FWI
			0x74735f62 FWI 0x5f6b6361 FWI 0x657a6973 FWI 0x65685f00 FWI
			0x735f7061 FWI 0x5f657a69 FWI 0x746c756d FWI 0x656c7069 FWI
			0x00000000
	# (close_ok,files) 
		= fclose file files
	= (close_ok,files)

//---

get_path_name_and_options linker startupdir
	#	(linker,opts)	= splitOptions linker
	| size linker > 0 && linker.[0] == '/'
		# linker_dir	= takeDirectory linker
		= (linker,linker_dir,opts)
	#	linker			= startupdir </> linker
		linker_dir		= takeDirectory linker
	= (linker,linker_dir,opts)

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
		| i < size s
			# c	= s.[i]
			| c==' ' || c=='\t'
				= skip_spaces_and_tabs (i+1) s
			= i
		= i

	skip_to_space_or_tab i s
		| i < size s
			# c	= s.[i]
			| c==' ' || c=='\t'
				= i
			= skip_to_space_or_tab (i+1) s
		= i

MakeOptionsName :: !.String !Processor -> String
MakeOptionsName path processor
	# (path`,name)	= splitFileName path
	= path` </> "Clean System Files" </> "_"+++dropExtension name+++"_options.o"

CheckObjsOutOfDate :: !Bool !Path ![!String!] !*GeneralSt -> (!Bool,!*GeneralSt)
CheckObjsOutOfDate gen execpath objs ps
	#	execname			= takeFileName execpath
	| gen
		#	ps				= verboseInfo
								(Level3 ["[" +++ execname +++ ",]: out of date. Linking new executable."])
								ps
		= (True,ps)
	#	(date, ps)			= accFiles (FModified execpath) ps
	| not date.exists
		#	ps				= verboseInfo
								(Level3 ["[" +++ execname +++ ",]: does not exist. Linking new executable."])
								ps
		= (True,ps)
	# (ood,ps) = accFiles (check date objs) ps
	| ood
		#	ps				= verboseInfo
								(Level3 ["[" +++ execname +++ ",]: is older than object files. Linking new executable."])
								ps
		= (True,ps)
	= (False,ps)
where
	check date [!!] files			= (False,files)
	check date [!hd : tl!] files
		#	(objDate, files)		= FModified hd files
		| Older_Date date objDate	= (True,files)
		= check date tl files

CheckExecOutOfDate :: !Bool !Path !FileInfoCache !Project !*GeneralSt -> *(!Bool,!*GeneralSt)
CheckExecOutOfDate gen execpath fileinfo project ps
	| gen
		= (True,ps)
	#	execname			= takeFileName execpath
	| not (PR_ExecUpToDate project)
		#	ps				= verboseInfo
								(Level3 ["'" +++ execname +++ "' was linked with different application options"])
								ps
		= (True,ps)
	#	(date, ps)			= accFiles (FModified execpath) ps
	#	youngest			= YoungestObj NoDate fileinfo
	#	link				= youngest.exists && (not date.exists || Older_Date date youngest)
	| link
		#	ps				= verboseInfo
								(if date.exists
									(Level3 ["[" +++ execname +++ ",]: is older than object files. Linking new executable."])
									(Level3 ["[" +++ execname +++ ",]: does not exist. Linking new executable."])
								) ps
		= (True,ps)
	= (False,ps)

verboseInfo vinfo ps
	#	({be_verbose},ps)	= getPrefs ps
	| not be_verbose
		= ps
	= showInfo vinfo ps

strictListToList :: ![!.a!] -> [.a]
strictListToList [!!]
	=	[]
strictListToList [!h : t!]
	=	[h : strictListToList t]

removeDup :: !.[!a!] -> .[!a!] | Eq a
removeDup [!x:xs!] = [!x:removeDup (Filter ((<>) x) xs)!]
removeDup _      = [!!]

Filter :: !(s -> Bool) ![!s!] -> [!s!]
Filter _ [!!]
	=	[!!]
Filter p [!h : t!]
	| p h
		=	[!h : Filter p t!]
	// otherwise
		=	Filter p t

//--

/*
from Directory	import pd_StringToPath,getFileInfo,createDirectory,::Path,::FileInfo{pi_fileInfo},::PI_FileInfo{isDirectory},::DirError(..)

ensureDirectory path ps
	# ((ok,pd_path),ps)	= pd_StringToPath path ps
	| not ok
		= (["Linker error: Unable to understand path: "+++path],ps)
	# ((err,{pi_fileInfo={isDirectory}}),ps) = getFileInfo pd_path ps
	  (err,ps) = if (case err of DoesntExist -> True; _ -> not isDirectory)
					(createDirectory pd_path ps)
					(err,ps)
	| case err of NoDirError -> False; _ -> True
		= (["Linker error: Unable to access or create: "+++path],ps)
	= ([],ps)
*/
import System.File, System.Directory

ensureDirectory path ps
	#	(r, ps)		= accWorld (getFileInfo path) ps
	| isError r
		#	(r,ps)	= createDirectory path ps
		| isError r
			= (["Linker error: Unable to access or create: "+++path],ps)
		= ([],ps)
	| not (fromOk r).directory
		= (["Linker error: Is a file, not a directory: "+++path],ps)
	= ([],ps)

accWorld f ps=:{gst_world}
	#	(r,gst_world)	= f gst_world
		ps				= {ps & gst_world = gst_world}
	= (r,ps)
	
/*
     The lstat() and stat() system calls will fail if:

     [EACCES]           Search permission is denied for a component of the path prefix.

     [EFAULT]           Sb or name points to an invalid address.

     [EIO]              An I/O error occurs while reading from or writing to the file system.

     [ELOOP]            Too many symbolic links are encountered in translating the pathname.  This is taken to be
                        indicative of a looping symbolic link.

     [ENAMETOOLONG]     A component of a pathname exceeds {NAME_MAX} characters, or an entire path name exceeds
                        {PATH_MAX} characters.

     [ENOENT]           The named file does not exist.

     [ENOTDIR]          A component of the path prefix is not a directory.

     The fstat(), lstat(), and stat() system calls will fail if:

     [EOVERFLOW]        The file size in bytes or the number of blocks allocated to the file or the file serial
                        number cannot be represented correctly in the structure pointed to by buf.

     In addition to the errors returned by the stat() and lstat(), fstatat() may fail if:

     [EBADF]            The path argument does not specify an absolute path and the fd argument is neither
                        AT_FDCWD nor a valid file descriptor open for searching.

     [EINVAL]           The value of the flag argument is not valid.

     [ENOTDIR]          The path argument is not an absolute path and fd is neither AT_FDCWD nor a file descrip-
                        tor associated with a directory.
*/
