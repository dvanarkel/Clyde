implementation module PmDriver

import StdArray,StdBool,StdList,StdMisc,StdEnum,StdStrictLists
from StdOverloadedList import Foldr,++|,Hd,Any
import UtilNewlinesFile, UtilIO

import IdeState

from typeatt import update_type_window
from errwin  import updateErrorWindow
from messwin import showInfo, :: InfoMessage(..)
from projwin import pm_update_project_window

import PmCallBack

import PmCleanSystem,PmPath,PmProject
from PmDialogues import doPathsDialog
import PmAbcMagic,PmFileInfo,PmDirCache

import Platform
from StdLibMisc import :: Date{..}, :: Time{..}

verboseInfo verbose info ps :== verbi verbose info ps
where
	verbi verbose info ps
		| not verbose && level3 info
			= ps
			= showInfo info ps

	level3 (Level3 _) = True
	level3 _ = False

getFICache` ps
	# (_,ps)	= getFICache ps
	# fi		= FI_EmptyCache
	= (fi,ps)

/*--- TO DO:

system module dependancy analysis is possible...

	ie. when you encounter a system module that must be recompiled then check in done list
	and remove those that depend on this system module and put them back into the todo list

should also be possible to detect cycles... -> generate warning dialogue...

---*/

System			:== "_system"

//--- project manager routines

:: SetMadeProjectFun :== Bool -> Bool -> Project -> GeneralSt -> GeneralSt

//	Compile /Check Syntax of the designated module
CompileProjectModule ::	!CompileOrCheckSyntax !Pathname !Project !SetMadeProjectFun !*GeneralSt -> *GeneralSt
CompileProjectModule compileOrCheckSyntax imp_pathname project setproject ps
	# ps					= ClearCompilerCache` ps
	# (srcpaths,ps)			= get_project_and_environment_paths project ps
	# (mdn,hierarchical_imp_pathname) = determine_dir_and_filename imp_pathname srcpaths
	#! (abccache,ps)		= getABCCache ps
	#! (fileinfo,ps)		= getFICache` ps
	#! ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	#! ps					= HandleDCErrors be_verbose errs warns ps
	#! (fileinfo,abccache,project,ok,newpaths,_,_,_,ps)
							= CompileTheProjectModule compileOrCheckSyntax mdn hierarchical_imp_pathname fileinfo abccache project dircache ps
	# ps					= setABCCache abccache ps
	# ps					= setFICache fileinfo ps
	= setproject ok newpaths project ps

GenAsmProjectModule :: !.Pathname !Project !SetMadeProjectFun !*GeneralSt -> *GeneralSt
GenAsmProjectModule imp_pathname project setproject ps
	# ps					= ClearCompilerCache` ps
	# (srcpaths,ps)			= get_project_and_environment_paths project ps
	# (mdn,hierarchical_imp_pathname) = determine_dir_and_filename imp_pathname srcpaths
	# (abccache,ps)			= getABCCache ps
	#! (fileinfo,ps)		= getFICache` ps
	# ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	# ps					= HandleDCErrors be_verbose errs warns ps
	# (fileinfo,abccache,project,ok,newpaths,abcpath,_,_,ps)
							= CompileTheProjectModule Compilation mdn hierarchical_imp_pathname fileinfo abccache project dircache ps
	| not ok || newpaths
		# ps				= setABCCache abccache ps
		# ps				= setFICache fileinfo ps
		= setproject True False project ps
	# (ps,abccache,fileinfo,project,ok,_)
							= GenCodeTheProjectModule True False AsmGeneration mdn abcpath abccache fileinfo project ps
	# ps					= setABCCache abccache ps
	# ps					= setFICache fileinfo ps
	= setproject True ok project ps

:: CleanupCont :== Pathname Bool Bool *GeneralSt -> *GeneralSt

:: *DriverCompilingInfo
	= Sync
	| AsyncWin ![CurrentlyCompiled] !AsyncWinCompilingInfo
	| Async	![CurrentlyCompiled] !AsyncCompilingInfo
	| Pers	!*CompilingInfo

::	AsyncWinCompilingInfo = {
		win_max_n_processes :: !Int,
		win_compiler_process_ids :: !CompilerProcessIds
	};

::	AsyncCompilingInfo = {
		max_n_processes :: !Int,
		compiler_process_ids :: !CompilerProcessIds,
		unknown_finished_processors :: !UnknownFinishedProcessors
	};

:: *DriverState
	= DInit !Bool !Project !MTPContinuation
	| DComp !Bool !*DirCache !DriverCompilingInfo ![!ModuleDirAndName] !DriverStateRecord
	| DQuitCompilers ![!ModuleDirAndName] !DriverCompilingInfo !DriverStateRecord
	| DGene ![!ModuleDirAndName] !DriverCodeGenerationInfo !DriverStateRecord
	| DLink !DriverStateRecord
	| DDone

:: *DriverStateRecord =
	{ project	:: !Project
	, continue	:: !MTPContinuation
	, fileinfo	:: !FileInfoCache
	, abccache	:: !*ABCCache
	, libsinfo	:: !StaticLibInfo
	, ok		:: !Bool
	, newpaths	:: !Bool
	, modpaths	:: ![!ModuleDirAndName]
	}

BringProjectUptoDate :: !Bool CleanupCont !*GeneralSt -> *GeneralSt
BringProjectUptoDate force continuation ps
	#  (project,ps)		= getProject ps

	#	ps				= PrecompileFase project ps

	#	ps				= showInfo (Level1 "Bring up to date...") ps
	#	ps				= ClearCompilerCache` ps
		ini_step		= DInit force project cleanup
	= start ini_step step ps
where
	PrecompileFase project ps
		# (precompile,project)	= PR_GetPrecompile project
		| isJust precompile
			# ps				= showInfo (Level1 "Precompile...") ps
			# (ok,ec,ps)		= Execute` (fromJust precompile) ps
			// error handling???
			= ps
		= ps

	PostlinkFase ok project ps
		| not ok
			= ps
		# (postlink,project)	= PR_GetPostlink project
		| isJust postlink
			# (Just post_link)	= postlink
			# (prj_path,ps)		= getProjectFilePath ps
			# prj_dir_path  = PR_GetRootDir project
			# (app_path,ps)		= getStup ps
			# post_link			= fulPath app_path prj_dir_path post_link
			# ps				= showInfo (Level1 "Postlink...") ps
			# (ok,ec,ps)		= Execute` ("\""+++post_link+++"\" \""+++prj_path+++"\"") ps
			= ps
		= ps

	cleanup :: !Bool !Bool !Bool !FileInfoCache !StaticLibInfo ![!ModuleDirAndName] !Project !Bool (!*ABCCache,!GeneralSt) -> *(!*DriverState,!*GeneralSt)
	cleanup ok newpaths linked fileinfo libsinfo modpaths project intr (abccache,ps)
		| newpaths && not intr		// if paths have changed -> try again
			# ps			= showInfo (Level1 "Paths have changed: remaking.") ps
			# ps			= ClearCompilerCache` ps
			= MakeTheProject False fileinfo libsinfo abccache project cleanup` ps

		# ps				= PostlinkFase ok project ps
		
		# ps				= showInfo (Level1 "Finished making.") ps
		# ps				= setProject project ps
		# ps				= setABCCache abccache ps
		# ps				= setFICache fileinfo ps
		# ps				= pm_update_project_window ps
		# path				= PR_GetExecPath project
		= stop (DDone,continuation path linked ok ps)
	
	cleanup` :: MTPContinuation
	cleanup` = cleanup

//-- Private stuff

get_project_and_environment_paths :: Project *GeneralSt -> *(!List String,!*GeneralSt)
get_project_and_environment_paths project ps
	# (syspaths,ps)			= getCurrentPaths ps
	# prjpaths				= PR_GetPaths project
	= (AppendLists prjpaths syspaths,ps)

:: MTPContinuation :== Bool Bool Bool FileInfoCache StaticLibInfo [!ModuleDirAndName] Project Bool *(*ABCCache,GeneralSt) -> *(*DriverState,*GeneralSt)

MakeTheProject :: !Bool !FileInfoCache !StaticLibInfo !*ABCCache !Project !MTPContinuation !GeneralSt -> (!*DriverState,!*GeneralSt)
MakeTheProject force fileinfo libsinfo abccache project continue ps
	# (srcpaths,ps)			= get_project_and_environment_paths project ps
	# ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	# ps					= HandleDCErrors be_verbose errs warns ps
	# (root_mdn,project)	= PR_GetRootModuleDirAndName project
	# (env_static_libs,ps)	= getCurrentSlibs ps
	# sfiles				= StrictListToList (Concat (SL_Libs libsinfo) env_static_libs)
	# (err,ps)				= check_exists sfiles ps
	| isJust err
		# line				= Level3 ["Error: Unable to find static library: '" +++ fromJust err +++ "'."]
		# ps				= showInfo line ps
		= continue False False False fileinfo libsinfo [!] project False (abccache, ps)
	# ((errs,slibs),ps)		= accFiles (getLibs sfiles) ps
	| not (isEmpty errs)
		# line				= Level3 ["Error: Failed reading static libraries: '" :errs]
		# ps				= showInfo line ps
		= continue False False False fileinfo libsinfo [!] project False (abccache, ps)
	# slibs					= ListToStrictList slibs
	# libsinfo				= SL_SetDcls slibs libsinfo
	# ps					= showInfo (Level1 "Compiling...") ps
	# rest					= [!root_mdn]
	# (method,ps)			= getCurrentMeth ps
	# (compinfo,ps) = case method of
				CompileSync			-> (Sync,ps)
				(CompileAsync cmax)	-> PlatformDependant
										(AsyncWin [] {win_max_n_processes=cmax,win_compiler_process_ids=NoCompilerProcessIds},ps)								// win
										(let (compiler_process_ids,ps2) = getCompilerProcessIds ps
										 in  (Async [] {max_n_processes=cmax,compiler_process_ids=compiler_process_ids,unknown_finished_processors=NoUnknownFinishedProcessors},ps2)	// mac
										)
				CompilePers			-> (Pers InitCompilingInfo,ps)
	# ds = 
		{ project	= project
		, continue	= continue
		, fileinfo	= fileinfo
		, abccache	= abccache
		, libsinfo	= libsinfo
		, ok		= True
		, newpaths	= False
		, modpaths	= [!]
		}
	= step False (DComp force dircache compinfo rest ds) ps
where
	check_exists [] ps = (Nothing,ps)
	check_exists [file:rest] ps
		# (ok,ps) = accFiles (FExists file) ps
		| ok = check_exists rest ps
		= (Just file,ps)

:: CurrentlyCompiled =
	{ iclModule	:: !ModuleDirAndName
	, options	:: CompilerOptions
	, slot		:: !Int
	}

:: *DriverCodeGenerationInfo
	= SyncCodeGeneration
	| ASyncCodeGeneration ![CodeGeneratorProcessNAndPaths] !AsyncCompilingInfo
	| ASyncCodeGenerationWin ![WinCodeGeneratorProcess] /*max_n_processes*/!Int

:: CodeGeneratorProcessNAndPaths
	= { cgp_process_n :: !Int, cgp_module_name :: !Modulename, cgp_obj_path :: !Pathname }

:: WinCodeGeneratorProcess = {
	wcgp_process_n :: !Int,
	wcgp_process_handle :: !Int,
	wcgp_scg :: !StartedCodeGenerator,
	wcgp_module_name :: !Modulename,
	wcgp_obj_path :: !Pathname
   }

module_occurs :: !String ![!ModuleDirAndName] -> Bool
module_occurs s [|x:xs] = x.mdn_name == s || module_occurs s xs
module_occurs s [!] =  False

get_neverTimeProfile_option :: !{#Char} Project !*GeneralSt -> (!Bool,!*GeneralSt)
get_neverTimeProfile_option module_name project ps
	= case (PR_GetModuleInfo module_name project) of
		Just modinfo
			-> (modinfo.compilerOptions.neverTimeProfile,ps)
		_
			# (prefs,ps) = getPrefs ps
			  defaultCO = prefs.compopts
			-> (defaultCO.neverTimeProfile,ps)

step :: !Bool !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
step intr (DInit force project setproject) ps
//	# ps				= showInfo (Level1 "Make the project...") ps
	# libsinfo			= PR_GetStaticLibsInfo project
	# (abccache,ps)		= getABCCache ps
	# (fileinfo,ps)		= getFICache` ps
	= MakeTheProject force fileinfo libsinfo abccache project setproject ps

step True (DComp force dircache compinfo rest ds) ps
	# ds = {ds & ok = False}
	# (modpaths,ds) = ds!modpaths
	= case compinfo of
		Pers inf
			#! ds = DGene modpaths SyncCodeGeneration ds
			// compile phase finished: kill clean compiler
			# (_,ps) = ExitCleanCompiler (inf,ps)
			-> step True ds ps
		AsyncWin _ _
			#! ds = DQuitCompilers modpaths compinfo ds
			-> step True ds ps
		_
			#! ds = DGene modpaths SyncCodeGeneration ds
			// need async cocl shootdown as well..
			-> step True ds ps

step intr (DComp force dircache Sync [!] ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetDirAndModulenames project
	# ds					= {ds & modpaths = modpaths, project = project}
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths SyncCodeGeneration ds) ps

step intr (DComp force dircache Sync [!next : rest] ds) ps
	// compile phase: check module 'next'
	| module_occurs next.mdn_name ds.modpaths
		// if already done then skip
		= step intr (DComp force dircache Sync rest ds) ps
	| isProjLibraryModule next.mdn_name ds.libsinfo
		// instead of testing explicitly put libmodules in done <= conflicts with other administration
		= step intr (DComp force dircache Sync rest ds) ps
	# (ps,dircache,ok,newpaths`,rest,compinfo,ds,_)
							= UpdateDependencies force next rest Sync dircache ds ps
	# ds	= {ds & newpaths = ds.newpaths || newpaths`, ok = ok}
	| not ok
		# (paths,ds)		= ds!modpaths
		= step intr (DGene paths SyncCodeGeneration ds) ps
	# ds & modpaths = [!next : ds.modpaths]
	= cont (DComp force dircache compinfo rest ds,ps)

step intr (DComp force dircache (Pers inf) [!] ds) ps
	// compile phase finished: kill clean compiler
	# (_,ps)				= ExitCleanCompiler (inf,ps)
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetDirAndModulenames project
	# ds					= {ds & modpaths = modpaths, project = project}
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths SyncCodeGeneration ds) ps

step intr (DComp force dircache compinfo=:(Pers _) [!next:rest] ds) ps
	// compile phase: check module 'next'
	| module_occurs next.mdn_name ds.modpaths
		// if already done then skip
		= step intr (DComp force dircache compinfo rest ds) ps
	| isProjLibraryModule next.mdn_name ds.libsinfo
		// instead of testing explicitly put libmodules in done <= conflicts with other administration
		= step intr (DComp force dircache compinfo rest ds) ps
	# (ps,dircache,ok,newpaths`,rest,compinfo,ds,_)
							= UpdateDependencies force next rest compinfo dircache ds ps
	# ds	= {ds & newpaths = ds.newpaths || newpaths`, ok = ok}
	| not ok
		# (Pers inf)		= compinfo
		# (_,ps)			= ExitCleanCompiler (inf,ps)
		# (paths,ds)		= ds!modpaths
		= step intr (DGene paths SyncCodeGeneration ds) ps
	# ds & modpaths = [!next : ds.modpaths]
	= cont (DComp force dircache compinfo rest ds,ps)

step intr (DComp force dircache (Async [] async_compiling_info=:{max_n_processes,compiler_process_ids,unknown_finished_processors=NoUnknownFinishedProcessors}) [!] ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetDirAndModulenames project
	# ds					= {ds & modpaths = modpaths, project = project}
	# (os_error,ps)			= ClearCompilerCaches compiler_process_ids ps;
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths (ASyncCodeGeneration [] async_compiling_info) ds) ps

step intr (DComp force dircache (AsyncWin [] {win_compiler_process_ids,win_max_n_processes}) [!] ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetDirAndModulenames project
	# ds					= {ds & modpaths = modpaths, project = project}
	# ps = app_world_instead_of_ps (QuitCleanCompiler True win_compiler_process_ids) ps;
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths (IF_WINDOWS (ASyncCodeGenerationWin [] win_max_n_processes) SyncCodeGeneration) ds) ps

step intr state=:(DComp force _ (Async _ _) _ _) ps
	# (state, ps)			= check_completed state ps
	# (state, ps)			= start_compilations state ps
	= cont (state, ps)
	where
		check_completed :: !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		check_completed state=:(DComp force _ (Async current=:[_:_] {max_n_processes,compiler_process_ids}) _ _)  ps
			= case (CompilePollCompleted compiler_process_ids ps) of
				(NoFinishedCompiler,ps)
					-> check_unknow_processors_are_known state ps
				(UnknownFinishedCompiler,ps)
					-> case state of
						DComp force dircache (Async current {max_n_processes,compiler_process_ids,unknown_finished_processors}) todo ds
							# unknown_finished_processors = add_unknown_finished_processor unknown_finished_processors
							# state = DComp force dircache (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) todo ds
							-> check_completed state ps
				(FinishedCompiler completedSlot exitcode,ps)
					#! (state,ps) = process_completed completedSlot exitcode state ps
					-> check_completed state ps
		check_completed state ps
			= check_unknow_processors_are_known state ps

		check_unknow_processors_are_known (DComp force dircache (Async current {max_n_processes,compiler_process_ids,unknown_finished_processors=UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors}) todo ds) ps
			| n_unknown_finished_processors+length known_finished_processors>=max_n_processes
				# state = DComp force dircache (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=NoUnknownFinishedProcessors}) todo ds
				# (state,ps) = handle_completed_processes 0 state ps
					with
						handle_completed_processes process_n state ps
							| process_n>=max_n_processes
								= (state,ps)
							| isMember process_n known_finished_processors
								= handle_completed_processes (process_n+1) state ps
								# (_,ps) = SendRepeatResult process_n ps
								/*
								# exitcode = 1
								#! (state,ps) = process_completed process_n exitcode state ps
								*/
								= handle_completed_processes (process_n+1) state ps								
				= (state, ps)
		check_unknow_processors_are_known state ps
			=	(state, ps)

		process_completed :: !Int !Int !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		process_completed completedSlot exitcode (DComp force dircache (Async current {max_n_processes,compiler_process_ids,unknown_finished_processors}) todo ds) ps
			# (completed, current)			= removeFromCurrent completedSlot current
			# unknown_finished_processors	= remove_from_unknown_finished_processors completedSlot unknown_finished_processors
			# (startupdir,ps)				= getStup ps
			# (interact, ps)				= getInteract ps
			# typewin = update_type_window interact completed.iclModule.mdn_name
			# ccstring						= "dummy ccstring for now.."
			# (abcpath,res,ps)				= CompileHandleExitCode exitcode ccstring startupdir completedSlot updateErrorWindow typewin 
												completed.iclModule completed.options.listTypes ps // types param
			# (_,(fileinfo,abccache,project,ok,newpaths`,_,deps,dircache,ps))
											= ProcessCompilerMsg Nothing Compilation completed.options completed.iclModule abcpath res ds.fileinfo dircache ds.abccache ds.project ps
			# ds							= {ds & newpaths = ds.newpaths || newpaths`, fileinfo = fileinfo, abccache = abccache, project = project, ok = ok}
			| ok
				# ds & modpaths = [!completed.iclModule : ds.modpaths]
				  todo = deps++|todo
				= (DComp force dircache (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) todo ds, ps)
			// not ok
				# (os_error,ps) = ClearCompilerCaches compiler_process_ids ps;
				# (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)

		start_compilations :: !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		start_compilations state=:(DComp force dircache (Async current {max_n_processes,compiler_process_ids,unknown_finished_processors}) [!next : rest] ds) ps
			// all threads used?
			| length current >= max_n_processes
				# ps = DelayEventLoop ps;			
				= (state, ps)
			// compile phase: check module 'next'
			| module_occurs next.mdn_name ds.modpaths || currently_compiled next.mdn_name current
				= start_compilations (DComp force dircache (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) rest ds) ps
			| isProjLibraryModule next.mdn_name ds.libsinfo
				// instead of testing explicitly put libmodules in done <= conflicts with other administration
				= (DComp force dircache (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) rest ds, ps)
			# (ps,dircache,ok,_,rest,compinfo,ds,_)
				= UpdateDependencies force next rest (Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) dircache ds ps
			# ds = {ds & ok = ok}
			| not ok
				# (os_error,ps) = ClearCompilerCaches compiler_process_ids ps;
				#! (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)
			= start_compilations (DComp force dircache compinfo rest ds) ps
		start_compilations state ps
			# ps = DelayEventLoop ps
			= (state, ps)

step intr state=:(DComp force dircache compinfo=:(AsyncWin _ _) rest ds) ps
	# (state, ps) = check_completed state ps
	# (state, ps) = start_compilations state ps
	= cont (state, ps)
	where
		check_completed :: !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		check_completed state=:(DComp _ _ (AsyncWin current=:[_:_] {win_compiler_process_ids}) _ _)  ps
			=	case (CompilePollCompleted win_compiler_process_ids ps) of
					(NoFinishedCompiler, ps)
						-> (state, ps)
					(FinishedCompiler completedSlot exitcode, ps)
						#! (state,ps) = process_completed completedSlot exitcode state ps
						-> IF_BATCHBUILD_OR_IDE
								(state,ps)
								(check_completed state ps)
					(UnknownFinishedCompiler,ps)
						-> (state, ps)	// -> doesn't occur on win
		check_completed state ps
			=	(state, ps)

		process_completed :: !Int !Int !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		process_completed completedSlot exitcode (DComp force dircache (AsyncWin current {win_max_n_processes,win_compiler_process_ids}) todo ds) ps
			# (completed, current)	= removeFromCurrent completedSlot current
			# (startupdir,ps)		= getStup ps
			# (interact, ps)		= getInteract ps
			# typewin				= update_type_window interact completed.iclModule.mdn_name
			# ccstring				= "dummy ccstring for now.."
			# (abcpath,res,ps)		= CompileHandleExitCode exitcode ccstring startupdir completedSlot updateErrorWindow typewin 
										completed.iclModule completed.options.listTypes ps // types param
			# (_,(fileinfo,abccache,project,ok,newpaths`,_,deps,dircache,ps))
				= ProcessCompilerMsg Nothing Compilation completed.options completed.iclModule abcpath res ds.fileinfo dircache ds.abccache ds.project ps
			# ds					= {ds & newpaths = ds.newpaths || newpaths`, fileinfo = fileinfo, abccache = abccache, project = project, ok = ok}
			| ok
				# ds & modpaths = [!completed.iclModule : ds.modpaths]
				  todo = deps++|todo
				= (DComp force dircache (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) todo ds, ps)
			// not ok
				# (paths,ds)	= ds!modpaths
				= (DQuitCompilers paths (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) ds,ps)

		start_compilations :: !*DriverState !*GeneralSt -> (!*DriverState,!*GeneralSt)
		start_compilations state=:(DComp force dircache (AsyncWin current {win_max_n_processes,win_compiler_process_ids}) [!next : rest] ds) ps
			| length current >= win_max_n_processes
				# ps = DelayEventLoop ps;			
				= (state, ps)
			// compile phase: check module 'next'
			| module_occurs next.mdn_name ds.modpaths || currently_compiled next.mdn_name current
				= start_compilations (DComp force dircache (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) rest ds) ps
			| isProjLibraryModule next.mdn_name ds.libsinfo
				// instead of testing explicitly put libmodules in done <= conflicts with other administration
				= (DComp force dircache (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) rest ds, ps)
			# (ps,dircache,ok,_,rest,compinfo,ds,_)
				= UpdateDependencies force next rest (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) dircache ds ps
			# ds = {ds & ok = ok}
			| not ok
				#! (paths,ds)	= ds!modpaths
				= (DQuitCompilers paths (AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}) ds,ps)
			= start_compilations (DComp force dircache compinfo rest ds) ps
		start_compilations state=:(DComp force dircache (AsyncWin [] _) [!] ds) ps
			= (state, ps)
		start_compilations state ps
			# ps = DelayEventLoop ps;
			= (state, ps)

step intr (DQuitCompilers modpaths (AsyncWin [] {win_compiler_process_ids}) ds) ps
	# ps = app_world_instead_of_ps (QuitCleanCompiler True win_compiler_process_ids) ps
	= step intr (DGene modpaths SyncCodeGeneration ds) ps
step intr state=:(DQuitCompilers modpaths (AsyncWin current async_win_compiling_info=:{win_compiler_process_ids}) ds) ps
	= case (CompilePollCompleted win_compiler_process_ids ps) of
		(NoFinishedCompiler, ps)
			-> cont (state, ps)
		(FinishedCompiler completedSlot exitcode, ps)
			# (completed, current) = removeFromCurrent completedSlot current
			-> cont (DQuitCompilers modpaths (AsyncWin current async_win_compiling_info) ds, ps)
		(UnknownFinishedCompiler,ps)
			-> cont (state, ps)	// -> doesn't occur on win

step intr (DGene [!] SyncCodeGeneration ds) ps
	#! ps	= showInfo (Level1 "Linking...") ps
	= step intr (DLink ds) ps

step intr (DGene [!] (ASyncCodeGeneration [] {unknown_finished_processors=NoUnknownFinishedProcessors,compiler_process_ids}) ds) ps
	# ps = setCompilerProcessIds compiler_process_ids ps
	#! ps	= showInfo (Level1 "Linking...") ps
	= step intr (DLink ds) ps

step intr (DGene [!] (ASyncCodeGenerationWin [] _) ds) ps
	#! ps	= showInfo (Level1 "Linking...") ps
	= step intr (DLink ds) ps

step intr (DGene [!mdn:rest] SyncCodeGeneration ds) ps
	| not ds.ok || intr
		# ds = {ds & ok = False}
		= step intr (DLink ds) ps
	# (abccache,fileinfo,gen,abcpath,ps)	= check_object_file_out_of_date mdn False ds.abccache ds.fileinfo ds.project ps
	# (ps,abccache,fileinfo,project,ok,_)	= GenCodeTheProjectModule gen False CodeGeneration mdn abcpath abccache fileinfo ds.project ps
	# ds = {ds & abccache = abccache, fileinfo = fileinfo, project = project, ok = ok}
	| not ok
		= step intr (DLink ds) ps
	= cont (DGene rest SyncCodeGeneration ds, ps)

step intr (DGene paths (ASyncCodeGeneration busy_processes {max_n_processes,compiler_process_ids,unknown_finished_processors}) ds) ps
	# (ok,busy_processes,unknown_finished_processors,project,fileinfo,ps)
		= handle_finished_code_generators busy_processes unknown_finished_processors ds.project ds.fileinfo ps
		with
			handle_finished_code_generators busy_processes=:[_:_] unknown_finished_processors project fileinfo ps
				= case (CompilePollCompleted compiler_process_ids ps) of
					(NoFinishedCompiler, ps)
						-> check_unknow_processors_are_known busy_processes unknown_finished_processors project fileinfo ps
					(UnknownFinishedCompiler,ps)
						# unknown_finished_processors = add_unknown_finished_processor unknown_finished_processors
						-> (True,busy_processes,unknown_finished_processors,project,fileinfo,ps)
					(FinishedCompiler finished_cg_slot_n exit_code, ps)
//						# ps  = trace ("code generator finished "+++toString finished_cg_slot_n+++" "+++toString exit_code+++"\n") ps
//						# ps  = trace ("f "+++toString finished_cg_slot_n+++" "+++toString exit_code+++" ") ps

						# unknown_finished_processors = remove_from_unknown_finished_processors finished_cg_slot_n unknown_finished_processors
						# (module_name,obj_path,busy_processes) = get_paths_and_remove_process_from_list finished_cg_slot_n busy_processes
							with
								get_paths_and_remove_process_from_list finished_cg_slot_n [busy_process=:{cgp_process_n,cgp_module_name,cgp_obj_path} : rest]
									| finished_cg_slot_n==cgp_process_n
										= (cgp_module_name,cgp_obj_path,rest)
										# (module_name,obj_path,rest) = get_paths_and_remove_process_from_list finished_cg_slot_n rest
										= (module_name,obj_path,[busy_process:rest])
								get_paths_and_remove_process_from_list finished_cg_slot_n []
									= abort "driver.icl: unknown code generator id"
						| exit_code==0
							# (fileinfo,ps) = accFiles (FI_UpdateObjDate module_name obj_path fileinfo) ps
							# project = PR_SetCodeGenerated module_name project
							-> handle_finished_code_generators busy_processes unknown_finished_processors project fileinfo ps
							-> (False,busy_processes,unknown_finished_processors,project,fileinfo,ps)
			handle_finished_code_generators [] unknown_finished_processors project fileinfo ps
				= check_unknow_processors_are_known [] unknown_finished_processors project fileinfo ps

			check_unknow_processors_are_known busy_processes (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors) project fileinfo ps
				| n_unknown_finished_processors+length known_finished_processors>=max_n_processes
					# (busy_processes,project,fileinfo,ps) = handle_completed_processes 0 busy_processes project fileinfo ps
						with
							handle_completed_processes process_n busy_processes project fileinfo ps
								| process_n>=max_n_processes
									= (busy_processes,project,fileinfo,ps)
								| isMember process_n known_finished_processors
									= handle_completed_processes (process_n+1) busy_processes project fileinfo ps
									# (_,ps) = SendRepeatResult process_n ps
									/*
									# unknown_finished_processors = remove_from_unknown_finished_processors finished_cg_slot_n unknown_finished_processors
									# (abc_path,obj_path,busy_processes) = get_paths_and_remove_process_from_list finished_cg_slot_n busy_processes
										with
											get_paths_and_remove_process_from_list finished_cg_slot_n [busy_process=:(slot,abc_path,obj_path) : rest]
												| process_n==slot
													= (abc_path,obj_path,rest)
													# (abc_path,obj_path,rest) = get_paths_and_remove_process_from_list finished_cg_slot_n rest
													= (abc_path,obj_path,[busy_process:rest])
											get_paths_and_remove_process_from_list finished_cg_slot_n []
												= abort "driver.icl: unknown code generator id"
									# (fileinfo,ps) = accFiles (FI_UpdateObjDate abc_path obj_path fileinfo) ps
									# project = PR_SetCodeGenerated (GetModuleName abc_path) project
									*/
									= handle_completed_processes (process_n+1) busy_processes project fileinfo ps								
					= (True,busy_processes,NoUnknownFinishedProcessors,project,fileinfo,ps)
			check_unknow_processors_are_known busy_processes unknown_finished_processors project fileinfo ps
				= (True,busy_processes,unknown_finished_processors,project,fileinfo,ps)
	# ds = {ds & fileinfo = fileinfo, project = project, ok = ds.ok && ok && not intr}
	| not ds.ok
		= cont (DGene paths (ASyncCodeGeneration busy_processes {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) ds,ps)
	| length busy_processes>=max_n_processes || (case paths of [!] -> True ; _ -> False)
		# ps = DelayEventLoop ps
		= cont (DGene paths (ASyncCodeGeneration busy_processes {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) ds,ps)
	# (ok,paths,busy_processes,fileinfo,abccache,ps) = start_code_generators paths busy_processes ds.fileinfo ds.abccache ps
		with
			start_code_generators :: [!ModuleDirAndName] [CodeGeneratorProcessNAndPaths] FileInfoCache *ABCCache !*GeneralSt -> *(.Bool,[!ModuleDirAndName],[CodeGeneratorProcessNAndPaths],FileInfoCache,*ABCCache,!*GeneralSt)
			start_code_generators paths=:[!mdn : rest] busy_processes fileinfo abccache ps
				| length busy_processes>=max_n_processes
		 			# ps = DelayEventLoop ps
					= (True,paths,busy_processes,fileinfo,abccache,ps)
				# (abccache,fileinfo,gen,abc_path,ps)
									= check_object_file_out_of_date mdn False abccache fileinfo project ps
				# cgo				= PR_GetCodeGenOptions project
				# (proc,ps)			= getCurrentProc ps
				# ((info,abccache,fileinfo), ps)
									= FI_GetFileInfo proc mdn abccache fileinfo ps
				| not gen
					= start_code_generators rest busy_processes fileinfo abccache ps
				# module_name		= mdn.mdn_name
				# ps				= showInfo (Level2 (
													(foldl (+++) ("Generating code for " +++ module_name)
															[" "+++cgp_module_name \\ {cgp_module_name}<-busy_processes])
													)) ps
				# (startupdir,ps)	= getStup ps
				  (cgen,ps)			= getCurrentCgen ps
				  (neverTimeProfile,ps) = get_neverTimeProfile_option module_name project ps
				  ao				= PR_GetApplicationOptions project
				  timeprofile		= ao.profiling && (not neverTimeProfile)
				# free_slot			= hd (removeMembers [0..max_n_processes-1] [cgp_process_n \\ {cgp_process_n} <- busy_processes])

				# (res,obj_path,compiler_process_ids,ps) = StartCodeGenerator cgen updateErrorWindow CodeGeneration abc_path free_slot timeprofile cgo proc ao startupdir compiler_process_ids ps
				| not res
					= (False,rest,busy_processes,fileinfo,abccache,ps)
				# busy_processes	= [{cgp_process_n=free_slot,cgp_module_name=module_name,cgp_obj_path=obj_path}:busy_processes]
				= start_code_generators rest busy_processes fileinfo abccache ps
			start_code_generators [!] busy_processes fileinfo abccache ps
				# ps = DelayEventLoop ps
				= (True,[!],busy_processes,fileinfo,abccache,ps)
	# ds = {ds & fileinfo = fileinfo, abccache = abccache, ok = ok}
	= cont (DGene paths (ASyncCodeGeneration busy_processes {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}) ds, ps)

step intr (DGene paths (ASyncCodeGenerationWin busy_processes win_max_n_processes) ds) ps
	# (ok,busy_processes,project,fileinfo,ps)
		= handle_finished_code_generators busy_processes ds.project ds.fileinfo ps
		with
			handle_finished_code_generators busy_processes=:[|_:_] project fileinfo ps
				# process_handles = {wcgp_process_handle \\ {wcgp_process_handle} <- busy_processes}
				# (i,exit_code,ps) = wait_for_finished_code_generator process_handles ps
				| i<0
					= (False,busy_processes,project,fileinfo,ps)
					# (finished_process,busy_processes) = remove_finished_process_from_list i busy_processes
						with
							remove_finished_process_from_list i [p:ps]
								| i==0
									= (p,ps)
									# (fp,ps) = remove_finished_process_from_list (i-1) ps;
									= (fp,[p:ps])
					# (ok,ps) = finish_code_generator finished_process.wcgp_process_handle finished_process.wcgp_scg exit_code updateErrorWindow ps
					| ok
						# module_name = finished_process.wcgp_module_name
						  obj_path = finished_process.wcgp_obj_path
						# (fileinfo,ps) = accFiles (FI_UpdateObjDate module_name obj_path fileinfo) ps
						# project = PR_SetCodeGenerated module_name project
						= (True,busy_processes,project,fileinfo,ps)
						= (False,busy_processes,project,fileinfo,ps)
			handle_finished_code_generators [|] project fileinfo ps
				= (True,[],project,fileinfo,ps)
	# ds = {ds & fileinfo = fileinfo, project = project, ok = ds.ok && ok && not intr}
	| not ds.ok
		= cont (DGene paths (ASyncCodeGenerationWin busy_processes win_max_n_processes) ds,ps)
	| length busy_processes>=win_max_n_processes || (case paths of [!] -> True ; _ -> False)
		= cont (DGene paths (ASyncCodeGenerationWin busy_processes win_max_n_processes) ds,ps)
	# (ok,paths,busy_processes,fileinfo,abccache,ps) = start_code_generators paths busy_processes ds.fileinfo ds.abccache ps
		with
			start_code_generators :: [!ModuleDirAndName] [WinCodeGeneratorProcess] FileInfoCache *ABCCache !*GeneralSt
						   -> *(Bool,[!ModuleDirAndName],[WinCodeGeneratorProcess],FileInfoCache,*ABCCache,!*GeneralSt )
			start_code_generators paths=:[!mdn:rest] busy_processes fileinfo abccache ps
				| length busy_processes>=win_max_n_processes
					= (True,paths,busy_processes,fileinfo,abccache,ps)
				# (abccache,fileinfo,gen,abc_path,ps)
									= check_object_file_out_of_date mdn False abccache fileinfo project ps
				# (proc,ps)			= getCurrentProc ps
				# ((_,abccache,fileinfo), ps) = FI_GetFileInfo proc mdn abccache fileinfo ps
				| not gen
					= start_code_generators rest busy_processes fileinfo abccache ps
				# module_name		= mdn.mdn_name
				# ps				= showInfo (Level2 (
													(foldl (+++) ("Generating code for "+++ module_name)
															[" "+++wcgp_module_name \\ {wcgp_module_name}<-busy_processes])
													)) ps
				# (startupdir,ps)	= getStup ps
				  (cgen,ps)			= getCurrentCgen ps
				  (neverTimeProfile,ps) = get_neverTimeProfile_option module_name project ps
				  ao				= PR_GetApplicationOptions project
				  timeprofile		= ao.profiling && (not neverTimeProfile)
				  cgo				= PR_GetCodeGenOptions project
				# free_slot			= hd (removeMembers [0..win_max_n_processes-1] [wcgp_process_n \\ {wcgp_process_n} <- busy_processes])

				# (res,process_handle,scg,ps)
					= start_code_generator cgen updateErrorWindow abc_path free_slot timeprofile cgo proc startupdir ps
				| not res
					= (False,rest,busy_processes,fileinfo,abccache,ps)
				# obj_path = ModuleDirAndNameToObjSystemPathname proc mdn
				# new_process = {wcgp_process_n=free_slot,wcgp_process_handle=process_handle,wcgp_scg=scg,wcgp_module_name=module_name,wcgp_obj_path=obj_path}
				# busy_processes = [new_process:busy_processes]
				= start_code_generators rest busy_processes fileinfo abccache ps
			start_code_generators [!] busy_processes fileinfo abccache ps
				= (True,[!],busy_processes,fileinfo,abccache,ps)
	# ds = {ds & fileinfo = fileinfo, abccache = abccache, ok = ok}
	= cont (DGene paths (ASyncCodeGenerationWin busy_processes win_max_n_processes) ds, ps)

step intr (DLink ds=:{ok, newpaths, fileinfo, libsinfo, modpaths, abccache, project, continue}) ps
	//	Check whether executable is out of date and relink it	if required.
	| intr || not ok
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	# lo						= PR_GetLinkOptions project
	# (prj_path,ps)				= getProjectFilePath ps
	# (app_path,ps)				= getStup ps
	# (srcpaths,ps)				= get_project_and_environment_paths project ps
	// set up dircache for 'Clean System Files'
	# ((errs,warns,abcPathsCache),ps)	= accFiles (DC_Setup (Map MakeSystemPathname srcpaths)) ps
	// need to handle this differently? Now barfs on paths without Clean System File subdirs
	// maybe use variant DC_Setup which ignores nonexistent CSF-dirs...
	# ({be_verbose},ps)			= getPrefs ps
	# ps						= HandleDCErrors be_verbose errs warns ps
	# system_abc				= MakeABCPathname System
	# (ok,full_sys0,_,abcPathsCache) = DC_Search system_abc abcPathsCache
	# full_sys					= full_sys0 +++ DirSeparatorString +++ system_abc
	# system_mdn 				= {mdn_dir=full_sys0,mdn_name=System}
	
	# ao						= PR_GetApplicationOptions project
	// possibly patch _system to correct profiling settings...

	# (tp,ps)					= getCurrentProc ps
	# ((modinfo,abccache,fileinfo),ps)
								= FI_GetFileInfo tp system_mdn abccache fileinfo ps
	
	# wantstp					= ao.profiling //&& (not co.neverTimeProfile)
	# compile					= /*mp <> info.abcOptions.abcMemoryProfile ||*/ wantstp <> modinfo.abcOptions.abcTimeProfile
	# lines						= if (be_verbose && compile)
									(Level3 ["["+++system_abc+++",]: compiled with different options"])
									(Level3 [])
	# ps						= verboseInfo be_verbose lines ps
	# (version,ps)				= getCurrentVers ps
	# (patched, ps)				= accFiles (PatchSystemABC version compile full_sys /*ao.memoryProfiling*/ wantstp) ps
	| not patched
		# line					= Level3 ["Error: ["+++system_abc+++",]: could not be patched."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	# ((abcdate,fileinfo), ps)	= accFiles (FI_UpdateAbcDate System full_sys wantstp fileinfo) ps
	# (abccache,fileinfo,genabc,abcpath,ps)
								// check _system module out of date
								= check_object_file_out_of_date system_mdn True abccache fileinfo project ps
	# (ps,abccache,fileinfo,project,ok,system_obj_path)
								// if out of date regenerate
								= GenCodeTheProjectModule genabc True CodeGeneration system_mdn abcpath abccache fileinfo project ps
	# (sys_date, ps)			= accFiles (FModified full_sys) ps
	# (abcPathsCache,ps)		= case genabc of
									True
										# sys_obj					= full_sys0 +++ DirSeparatorString +++ (MakeObjPathname tp System)
										  (sys_obj_date,ps)			= accFiles (FModified sys_obj) ps
										  sys_obj_date_time			= DATEtoDateTime sys_obj_date
										-> (DC_Update ((MakeObjPathname tp System),full_sys0,sys_obj_date_time) abcPathsCache,ps)
												// need to check if line above actually works now...
									False
										-> (abcPathsCache,ps)
	| not ok
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	# ((ok,_,_,_,sys_objs,sys_libs,abccache),ps)
								= accFiles (ParseABCDependencies` abcpath sys_date abccache) ps
	| not ok
		# line					= Level3 ["Error: ["+++system_abc+++",]: could not be analysed."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	# execpath					= PR_GetExecPath project
	# prj_path` = PR_GetRootDir project
	# execpath					= fulPath app_path prj_path` execpath
	# ps						= showInfo (Level2 ("Linking '" +++ RemovePath execpath +++ "'")) ps
	
	# (use_64_bit_processor,ps) = getCurrent64BitProcessor ps

	// runtime objects and dynamic libs
	# stdl						= Concat sys_libs (standardStaticLibraries tp lo.method)
	# stdo						= Concat sys_objs (standardObjectFiles ao.stack_traces ao.profiling tp use_64_bit_processor)
	# (stdoOk,ofiles,abcPathsCache)
								= case ao.standard_rte of
									True	-> GetPathNames stdo Nil abcPathsCache
									False	-> (True,Nil,abcPathsCache)
	| not stdoOk
		# line					= Level3 ["Link error: File: '" +++ (Head ofiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	# (stdlOk,lfiles,abcPathsCache)
								= case ao.standard_rte of
									True	-> GetPathNames stdl Nil abcPathsCache
									False	-> (True,Nil,abcPathsCache)
	| not stdlOk
		# line					= Level3 ["Link error: File: '" +++ (Head lfiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	// environment objects and dynamic libs
	# (deflibs,ps)				= getCurrentDlibs ps
	# (defobjs,ps)				= getCurrentObjts ps
	# ofiles					= Concat defobjs ofiles
	# lfiles					= Concat deflibs lfiles

	// clean modules
	# (clmodpaths,fileinfo)		= FI_GetCleanModules system_obj_path libsinfo fileinfo
	// why couldn't we use ds.modpaths above??? No we need to do trickery to ensure main module is first!
	# (root_mdn,project)		= PR_GetRootModuleDirAndName project
	# rootpath					= ModuleDirAndNameToObjSystemPathname tp root_mdn
	# clmodpaths				= RemoveStringFromList rootpath clmodpaths
	# ofiles` = ofiles
	# ofiles					= rootpath :! ofiles
	
	# ofiles					= Reverse2 clmodpaths ofiles

	// module imported objects and dynamic libs
	# abcLinkInfo				= PR_GetABCLinkInfo project
	# linkObjFileNames			= Map (append_object_file_extension_if_dot_at_end tp use_64_bit_processor) abcLinkInfo.linkObjFileNames
	# (objPathsOk,ofiles,abcPathsCache)
								= GetPathNames linkObjFileNames ofiles abcPathsCache
	# (_,ofiles`,abcPathsCache) = GetPathNames /*abcLinkInfo.*/linkObjFileNames ofiles` abcPathsCache
	# (libPathsOk,lfiles,abcPathsCache)
								= GetPathNames abcLinkInfo.linkLibraryNames lfiles abcPathsCache
	| not objPathsOk
		# line					= Level3 ["Link error: File: '" +++ (Head ofiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	| not libPathsOk
		# line					= Level3 ["Link error: File: '" +++ (Head lfiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	// project objects and dynamic libs
	# extraObjectModules		= lo.extraObjectModules
	# extraObjectModules		= Map (append_object_file_extension_if_dot_at_end tp use_64_bit_processor) extraObjectModules
	# ofiles					= Concat extraObjectModules ofiles
	# ofiles`					= Concat extraObjectModules ofiles`
	# lfiles					= Concat lo.libraries lfiles

	# (env_static_libs,ps)		= getCurrentSlibs ps
	#! sfiles					= Concat (SL_Libs libsinfo) env_static_libs		// only if really used?
	#! ofiles					= Reverse ofiles
	#! lfiles					= Reverse lfiles

//*	
	// .exe or .dat older than module.o
	// martijn also wants comparison with other libs and objs
	// !!! fails to check if console type options have changed...
	// ie. tries with old-fashioned PR_ExecUptoDate...
	// can't really fix this...
//	# (genabc,ps)				= CheckObjsOutOfDate genabc ofiles ps
//	# (genabc,ps) 				= CheckLibsOutOfDate genabc lfiles ps
	# (ood,ps)					= case lo.method of
									LM_Static  
										# (ood,ps) = CheckObjsOutOfDate genabc execpath ofiles` ps
										| ood -> (ood,ps)
										-> CheckExecOutOfDate genabc execpath fileinfo project ps
									LM_Dynamic
										-> CheckExecOutOfDate genabc execpath fileinfo project ps
	| not ood
		= continue True False False fileinfo libsinfo modpaths project intr (abccache, ps)
//*/
	# (_,ps)					= accFiles (SaveProjectFile prj_path project app_path) ps
	# (linkstr,ps)				= getCurrentLink ps
	# (startupdir,ps)			= getStup ps
		
// want to wait with .exe out of date checks till here...
// ao ok
// lfiles ok
// ofiles ok
// sfiles ok
// so: chache .exe date with used ao and lfiles,ofiles,sfiles dates...
// can imporve now by Younger checking all objects

	# optionspath				= MakeOptionsName prj_path tp
	# (dynlstr,ps)				= getCurrentDynl ps
	# (ps, ok)					= foldl linkfun (ps,True) (lSplit '|' linkstr)
	  with
		linkfun (ps,ok) linkstr
			| ok
				= Link (ltrim linkstr) updateErrorWindow execpath ao
									optionspath lfiles ofiles sfiles
									(lo.method == LM_Static)
									lo.generate_relocations
									lo.generate_symbol_table
									lo.generate_link_map
									lo.link_resources
									(fulPath app_path prj_path` lo.resource_source)
									lo.generate_dll
									(fulPath app_path prj_path` lo.dll_export_list_name)
									startupdir dynlstr tp /*lo.add_carb_resource*/ use_64_bit_processor ps
				= (ps,ok)
	# project					= if ok (PR_SetLinked project) project
	= continue ok False ok fileinfo libsinfo modpaths project intr (abccache, ps)
where
	DATEtoDateTime :: !DATE -> DateTime
	DATEtoDateTime {DATE | yy,mm,dd,h,m,s}
		= ({year=yy,month=mm,day=dd,dayNr=0},{hours=h,minutes=m,seconds=s})

	ltrim :: !String -> String
	ltrim s
		| non_space_index == 0
						= s
						= s % (non_space_index,size_s-1)
	where
		size_s			= size s
		non_space_index	= non_space_left 0
		
		non_space_left :: !Int -> Int
		non_space_left i
			| i < size_s && isSpace s.[i]
				= non_space_left (i+1)
				= i

	lSplit :: !.Char !String -> [String]
	lSplit c s = lsplit c s (dec (size s)) []
	where
		lsplit c s i l
			| i < 0		= l
			# i` = findPos` c s i
			= lsplit c s (dec i`) [s % (i`+1,i):l]

	findPos` c s i
		| i < 0 = ~1
		| s.[i] == c = i
		= findPos` c s (dec i)

step intr DDone ps
	= stop (DDone,ps)

currently_compiled :: String [CurrentlyCompiled] -> Bool
currently_compiled next current
	= Any (\c -> c.iclModule.mdn_name == next) current

removeFromCurrent :: Int [CurrentlyCompiled] -> (CurrentlyCompiled, [CurrentlyCompiled])
removeFromCurrent _ []
	=	abort "driver.icl: unknown threadId"
removeFromCurrent completedSlot [current=:{slot} : rest]
	| completedSlot == slot
		=	(current, rest)
	// otherwise
		# (completed, rest) = removeFromCurrent completedSlot rest
		= (completed, [current : rest])

append_object_file_extension_if_dot_at_end tp use_64_bit_processor s
	| s.[size s - 1] == '.'
		| use_64_bit_processor && DirSeparator=='\\'
			= s+++"obj"
			= MakeObjPathname tp s
		= s

//-- Compile Phase...

:: UnknownFinishedProcessors = NoUnknownFinishedProcessors | UnknownFinishedProcessors !Int ![Int]

add_unknown_finished_processor NoUnknownFinishedProcessors
	= UnknownFinishedProcessors 1 []
add_unknown_finished_processor (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors)
	= UnknownFinishedProcessors (n_unknown_finished_processors+1) known_finished_processors							

remove_from_unknown_finished_processors completedSlot (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors)
	| not (isMember completedSlot known_finished_processors)
		= UnknownFinishedProcessors n_unknown_finished_processors [completedSlot:known_finished_processors]
remove_from_unknown_finished_processors completedSlot unknown_finished_processors
	= unknown_finished_processors

compiling_info :: !DriverCompilingInfo -> (String,DriverCompilingInfo)
compiling_info info=:(AsyncWin current _)
	=	(compiling_info_async current,info)
compiling_info info=:(Async current _)
	= (compiling_info_async current,info);
compiling_info info
	= ("",info)

compiling_info_async []
	= ""
compiling_info_async current
	=	foldl (\s c -> s +++ " "+++c.iclModule.mdn_name) "Compiling:" current

AccTuple4 f s1 :== let (e1,e2,e3,s2) = f s1 in ((e1,e2,e3),s2)

//	Scan modified modules and update the dependencies (recompile if necessary).
UpdateDependencies :: !Bool !ModuleDirAndName ![!ModuleDirAndName] !DriverCompilingInfo !*DirCache !DriverStateRecord !*GeneralSt
	-> (*GeneralSt,*DirCache,Bool,Bool,[!ModuleDirAndName],DriverCompilingInfo,DriverStateRecord,Bool)
UpdateDependencies force mdn rest compinfo dircache ds ps`
	# module_name				= mdn.mdn_name
	  co						= case PR_GetModuleInfo module_name project of
								  Just modinfo	-> modinfo.compilerOptions
								  _				-> defaultCO

	# (version,ps)				= getCurrentVers ps					// lift to DriverState
	# (cinf,compinfo)			= compiling_info compinfo
	# line						= Level2 ((if verbose ("Analyzing \'" +++  module_name +++ "\'. ") ("")) +++ cinf)
	# ps						= verboseInfo verbose line ps
	# (proc,ps)					= getCurrentProc ps
	# ((info,abccache,fileinfo),ps)
								= FI_GetFileInfo proc mdn ds.abccache ds.fileinfo ps
	# ds						= {ds & abccache = abccache, fileinfo = fileinfo}
	# abcexists					= info.abcdate.exists
	| not abcexists
		# lines					= Level3 ["["+++module_name+++".icl,]: no abc file"]
		#! ps					= showInfo lines ps
		= compile_module compinfo mdn rest co dircache ds ps
	| info.sys	// system module
		# wrongVersion			= info.version <> version
		| wrongVersion
		// add automatic recompilation if system module...
		// check with John what compiler flags we should pass...
			# line				= Level3 ["Error: System file: '" +++ module_name +++ "' has incorrect abc version."]
			# ps				= showInfo line ps
			= (ps, dircache, False, False, rest, compinfo, ds, False)
		# wantstp				= tp && (not co.neverTimeProfile)
		# compile				= /*mp <> info.abcOptions.abcMemoryProfile ||*/ wantstp <> info.abcOptions.abcTimeProfile
		# lines					= (Level3 (if (verbose && compile) ["["+++module_name+++".abc,]: System module compiled with different options"] []))
		# ps					= verboseInfo verbose lines ps
		| compile
			# abcPath = ModuleDirAndNameToABCSystemPathname mdn
			# (patched, ps)		= accFiles (PatchSystemABC version True abcPath /*ao.memoryProfiling*/ wantstp) ps
			| patched
				# ((abcdate,fileinfo), ps)
								= accFiles (FI_UpdateAbcDate module_name abcPath wantstp ds.fileinfo) ps
				# ds = {ds & fileinfo = fileinfo}
				# ((ok,mods,_,_,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath abcdate ds.abccache) ps
				# ds = {ds & abccache = abccache}
				| not ok
					# line		= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
					// can actually only occur when failed to open .abc file...
					# ps		= showInfo line ps
					= (ps,dircache,False,False,rest,compinfo,ds,False)
				# ((ok,paths,dircache),ps) = accFiles (AccTuple4 (LookupModulePaths mods dircache)) ps
				| not ok
					# line		= Level3 ["Error: '" +++ (Hd paths).mdn_name +++ "' not found."]
					# ps		= showInfo line ps
					= (ps,dircache,False,False,rest,compinfo,ds,False)
				#! rest			= rest++|paths
				#! project		= PR_AddABCInfo mdn objs libs defaultCO ds.project
				   ds = {ds & project = project, modpaths = [!mdn : ds.modpaths]}
				=	(ps, dircache, True, False, rest, compinfo, ds, True)
			# line				= Level3 ["Error: System file: '" +++ module_name +++ "' could not be patched."]
			# ps				= showInfo line ps
			=	(ps, dircache, False, False, rest, compinfo, ds, False)
		# ((ok,mods,_,_,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath info.abcdate ds.abccache) ps
		# ds = {ds & abccache = abccache}
		| not ok
			# line				= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		# ((ok,paths,dircache),ps) = accFiles (AccTuple4 (LookupModulePaths mods dircache)) ps
		| not ok
			# line				= Level3 ["Error: '" +++ (Hd paths).mdn_name +++ "' not found."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		#! rest					= paths++|rest
		#! project				= PR_AddABCInfo mdn objs libs defaultCO ds.project
		   ds = {ds & project = project, modpaths = [!mdn : ds.modpaths]}
		=	(ps, dircache, True, False, rest, compinfo, ds, False)

	// normal module
	| force
		# lines					= Level3 ["["+++module_name+++".icl,]: force compile"]
		#! ps					= showInfo lines ps
		= compile_module compinfo mdn rest co dircache ds ps
	# ((ok,mods,xxx_md,xxx_dd,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath info.abcdate ds.abccache) ps
	# ds = {ds & abccache = abccache}
	| not ok
		# line					= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
		# ps					= showInfo line ps
		= (ps,dircache,False,False,rest,compinfo,ds,False)
	# ((okA,whyA,dircache),ps)	= accFiles (AccTuple4 (check_dependant_dates module_name mods xxx_md xxx_dd dircache)) ps
	# (use_64_bit_processor,ps) = getCurrent64BitProcessor ps

	# project_compiler_options = {	pco_memory_profiling=ao.memoryProfiling,
									pco_time_profiling=ao.profiling,
									pco_desc_exl=ao.desc_exl,
									pco_dynamics=ao.dynamics,
									pco_link_dynamic=link_dynamic}
	# (okC,whyC)				= check_module_options module_name info co project_compiler_options use_64_bit_processor version
	
	| okA && okC
		# ((ok,paths,dircache),ps) = accFiles (AccTuple4 (LookupModulePaths mods dircache)) ps
		| not ok
			# line				= Level3 ["Error: '" +++ (Hd paths).mdn_name +++ "' not found."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		#! rest					= paths++|rest
		#! project				= PR_AddABCInfo mdn objs libs co ds.project
		   ds = {ds & project = project, modpaths = [!mdn : ds.modpaths]}
		= (ps,dircache,True,False,rest,compinfo,ds,False)
		
	# lines						= Level3 [if okC whyA whyC]
	#! ps						= showInfo lines ps
	= compile_module compinfo mdn rest co dircache ds ps
where
	(prefs,ps) 			= getPrefs ps`				// lift to DriverState

	project = ds.project
	ao					= PR_GetApplicationOptions project
	mp					= ao.memoryProfiling
	tp					= ao.profiling
	lo					= PR_GetLinkOptions project
	link_dynamic		= case lo.method of LM_Static -> False; LM_Dynamic -> True				
	verbose				= prefs.be_verbose
	defaultCO			= prefs.compopts

compile_module Sync mdn rest co dircache ds ps
	= UpdateSyncDependencies mdn rest co dircache ds ps
compile_module (Async current async_compiling_info) mdn rest co dircache ds ps
	= UpdateAsyncDependencies current async_compiling_info mdn rest co dircache ds ps
compile_module (AsyncWin current win_compiling_info) mdn rest co dircache ds ps
	= UpdateAsyncDependenciesWin current win_compiling_info mdn rest co dircache ds ps
compile_module (Pers info) mdn rest co dircache ds ps
	= UpdatePersDependencies mdn info rest co dircache ds ps

UpdateSyncDependencies mdn rest co dircache ds ps
	# hierarchical_imp_pathname = MakeFullPathname mdn.mdn_dir (mdn.mdn_name +++ ".icl")
	# (fileinfo,abccache,project,ok,newpaths,_,deps,dircache,ps)
							= CompileTheProjectModule Compilation mdn hierarchical_imp_pathname ds.fileinfo ds.abccache ds.project dircache ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache, project = project}
	= (ps,dircache,ok,newpaths,deps++|rest,Sync,ds,ok)

UpdateAsyncDependencies current {max_n_processes,compiler_process_ids,unknown_finished_processors} mdn rest co dircache ds ps
	# free_slot = get_free_slot current
	# (compileStarted, fileinfo, dircache, abccache,compiler_process_ids,ps)
		= CompileTheProjectModuleStart Compilation mdn free_slot ds.fileinfo dircache ds.abccache ds.project compiler_process_ids ps
	# ds = {ds & fileinfo = fileinfo, abccache = abccache}
	| compileStarted
		# current = [{iclModule = mdn, options = co, slot = free_slot} : current]
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}
		= (ps,dircache,True,False,rest,async,ds,True)
	// not compileStarted
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = Async current {max_n_processes=max_n_processes,compiler_process_ids=compiler_process_ids,unknown_finished_processors=unknown_finished_processors}
		= (ps,dircache,False,False,rest,async,ds,False)

UpdateAsyncDependenciesWin current {win_max_n_processes,win_compiler_process_ids} mdn rest co dircache ds ps
	# free_slot = get_free_slot current
	# (compileStarted, fileinfo, dircache, abccache,win_compiler_process_ids,ps)
		= CompileTheProjectModuleStart Compilation mdn free_slot ds.fileinfo dircache ds.abccache ds.project win_compiler_process_ids ps;
	# ds = {ds & fileinfo = fileinfo, abccache = abccache}
	| compileStarted
		# current = [{iclModule = mdn, options = co, slot = free_slot} : current]
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}
		= (ps,dircache,True,False,rest,async,ds,True)
	// not compileStarted
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = AsyncWin current {win_max_n_processes=win_max_n_processes,win_compiler_process_ids=win_compiler_process_ids}
		= (ps,dircache,False,False,rest,async,ds,False)

get_free_slot :: [CurrentlyCompiled] -> Int
get_free_slot current
	=	hd (removeMembers [0..] [slot \\ {slot} <- current])

UpdatePersDependencies mdn cstate rest co dircache ds ps
	# (cstate,(fileinfo,abccache,project,ok,newpaths,_,deps,dircache,ps))
			= PersistentCompile cstate Compilation mdn ds.fileinfo dircache ds.abccache ds.project ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache, project = project}
	= (ps,dircache,ok,newpaths,deps++|rest,Pers cstate,ds,ok)

//	Compile the designated module.
CompileTheProjectModule :: !CompileOrCheckSyntax !ModuleDirAndName !Pathname !FileInfoCache !*ABCCache !Project !*DirCache !*GeneralSt
	-> *(FileInfoCache,*ABCCache,Project,Bool,Bool,Pathname,[!ModuleDirAndName],*DirCache,*GeneralSt)
CompileTheProjectModule compileOrCheckSyntax mdn hierarchical_imp_pathname fileinfo abccache project dircache ps
	# (ok,ccstring,write_module_times,errwin,typwin,srcpaths,project_compiler_options,co,startupdir,ps)
								= ShowInfoAndCompile compileOrCheckSyntax mdn.mdn_name project ps
	| not ok
		= (fileinfo,abccache,project,False,False,"",[!],dircache,ps)
	# (use_compiler_process_ids,compiler_process_ids,ps) = get_use_compiler_process_ids_and_compiler_process_ids ps
	# (abcpath,res,compiler_process_ids,ps)
		= Compile ccstring use_compiler_process_ids write_module_times errwin typwin compileOrCheckSyntax mdn hierarchical_imp_pathname srcpaths project_compiler_options co startupdir compiler_process_ids ps
	# ps = setCompilerProcessIds compiler_process_ids ps
	# (_,res)					= ProcessCompilerMsg Nothing compileOrCheckSyntax co mdn abcpath res fileinfo dircache abccache project ps
	= res

get_use_compiler_process_ids_and_compiler_process_ids :: !*GeneralSt -> (!Bool,!CompilerProcessIds,!*GeneralSt)
get_use_compiler_process_ids_and_compiler_process_ids ps
	# (method,ps) = getCurrentMeth ps
	# use_compiler_process_ids = case method of CompileAsync _ -> True ; _ -> False
	# (compiler_process_ids,ps) = getCompilerProcessIds ps
	= (use_compiler_process_ids,compiler_process_ids,ps)

CompileTheProjectModuleStart :: !CompileOrCheckSyntax !ModuleDirAndName !Int !FileInfoCache !*DirCache !*ABCCache !Project !CompilerProcessIds !*GeneralSt
	-> *(!Bool, FileInfoCache, *DirCache, *ABCCache, CompilerProcessIds, *GeneralSt)
CompileTheProjectModuleStart compileOrCheckSyntax mdn slot fileinfo dircache abccache project compiler_process_ids ps
	# (ok,ccstring,write_module_times,errwin,_,srcpaths,project_compiler_options,co,startupdir,ps)
								= CTPMcommon mdn.mdn_name project ps
	| not ok
		= (False, fileinfo, dircache, abccache,compiler_process_ids,ps)
	# (compileStarted,compiler_process_ids,ps)
		= CompileStartCommand ccstring write_module_times errwin compileOrCheckSyntax mdn.mdn_name srcpaths slot project_compiler_options co startupdir compiler_process_ids ps
	= (compileStarted, fileinfo, dircache, abccache,compiler_process_ids,ps)

PersistentCompile :: !*CompilingInfo !CompileOrCheckSyntax !ModuleDirAndName !FileInfoCache !*DirCache !*ABCCache !Project !*GeneralSt
	-> (*CompilingInfo,*(FileInfoCache,*ABCCache,Project,Bool,Bool,Pathname,[!ModuleDirAndName],*DirCache,*GeneralSt))
PersistentCompile cstate compileOrCheckSyntax mdn fileinfo dircache abccache project ps
	# (ok,ccstring,write_module_times,errwin,typwin,srcpaths,project_compiler_options,co,startupdir,ps)
								= ShowInfoAndCompile compileOrCheckSyntax mdn.mdn_name project ps
	| not ok
		= (cstate,(fileinfo,abccache,project,False,False,"",[!],dircache,ps))
	# (cstate,(ps,abcpath,res))	= CompilePersistent ccstring write_module_times errwin typwin compileOrCheckSyntax mdn srcpaths project_compiler_options co startupdir cstate ps
	# (Just cstate,rest)
		= ProcessCompilerMsg (Just cstate) compileOrCheckSyntax co mdn abcpath res fileinfo dircache abccache project ps
	= (cstate,rest)

ShowInfoAndCompile :: !CompileOrCheckSyntax !Pathname !Project !*GeneralSt
	-> *(Bool, String, Bool, ([String] *GeneralSt -> *GeneralSt), ([String] *GeneralSt -> *GeneralSt), List String, ProjectCompilerOptions, CompilerOptions, String, *GeneralSt)
ShowInfoAndCompile compileOrCheckSyntax module_name project ps
	# line						= Level2 ((if (compileOrCheckSyntax == Compilation) "Compiling '" "Checking '") +++ module_name +++ "'.")
	# ps						= showInfo line ps
	= CTPMcommon module_name project ps

CTPMcommon :: !Modulename !Project !*GeneralSt
	-> *(Bool, String, Bool, ([String] *GeneralSt -> *GeneralSt), ([String] *GeneralSt -> *GeneralSt), List String, ProjectCompilerOptions, CompilerOptions, String, *GeneralSt)
CTPMcommon module_name project ps
	# (syspaths,ps)				= getCurrentPaths ps
	# (startupdir,ps)			= getStup ps
	# ({compopts},ps)			= getPrefs ps
	# defaultCO					= compopts
	# modinfo					= PR_GetModuleInfo module_name project
	# co						= if (isJust modinfo) ((fromJust modinfo).compilerOptions) defaultCO
	# (ccstring,ps)				= getCurrentComp ps
	# write_module_times		= True
	# prjpaths					= PR_GetPaths project
	#! srcpaths					= Concat prjpaths syspaths
	# ao						= PR_GetApplicationOptions project
	# link_dynamic = case (PR_GetLinkOptions project).method of LM_Static -> False; LM_Dynamic -> True
	# project_compiler_options = {	pco_memory_profiling=ao.memoryProfiling,
									pco_time_profiling=ao.profiling,
									pco_desc_exl=ao.desc_exl,
									pco_dynamics=ao.dynamics,
									pco_link_dynamic=link_dynamic}
	= (True,ccstring,write_module_times,updateErrorWindow,typewin module_name,srcpaths,project_compiler_options,co,startupdir,ps)
where
	typewin :: !String ![String] !*GeneralSt -> *GeneralSt
	typewin module_name strings ps
		# (interact, ps)	= getInteract ps
		= update_type_window interact module_name strings ps

ProcessCompilerMsg :: !*(Maybe *CompilingInfo) !CompileOrCheckSyntax !CompilerOptions !ModuleDirAndName !Pathname !CompilerMsg !FileInfoCache !*DirCache !ABCCache !Project !*GeneralSt
						-> *(*(Maybe *CompilingInfo),(FileInfoCache,*ABCCache,Project,Bool,Bool,Pathname,[!ModuleDirAndName],*DirCache,*GeneralSt))
ProcessCompilerMsg cstate compileOrCheckSyntax _ mdn abcpath (Patherror pathname) fileinfo dircache abccache project ps
	#	(interact, ps)		= getInteract ps
	| not interact
		= (cstate,(fileinfo,abccache,project,False,False,abcpath,[!],dircache,ps))
	#	(ps,project,new)	= NewPathsDialog mdn.mdn_name pathname project ps
	| new
		# (srcpaths,ps)			= get_project_and_environment_paths project ps
		# ((errs,warns,dircache),ps)
								= accFiles (DC_Setup srcpaths) ps
		# (prefs,ps)			= getPrefs ps
		# ps					= HandleDCErrors prefs.be_verbose errs warns ps
		# (cstate,ps)			= case cstate of
			Nothing
				-> (cstate,ps)
			Just pinfo
				# (pinfo,ps)	= ExitCleanCompiler (pinfo, ps)
				-> (Just pinfo, ps)
		// RWS: this compile is still blocking...
		# hierarchical_imp_pathname = MakeFullPathname mdn.mdn_dir (mdn.mdn_name +++ ".icl")
		# (fileinfo,abccache,project,ok,_,abcpath,deps,dircache,ps)
			= CompileTheProjectModule compileOrCheckSyntax mdn hierarchical_imp_pathname fileinfo abccache project dircache ps
		= (cstate,(fileinfo,abccache,project,ok,True,abcpath,deps,dircache,ps))
	= (cstate,(fileinfo,abccache,project,False,new,abcpath,[!],dircache,ps))
where
	NewPathsDialog :: !String !String !Project !*GeneralSt -> *(*GeneralSt,Project,Bool)
	NewPathsDialog module_name path project ps
		# (ap,ps)				= getStup ps
		# (defp,ps)				= getCurrentPaths ps
		# prjpaths				= PR_GetPaths project
		# pp					= PR_GetRootDir project
		# line					= "Where is '" +++ path +++ "' imported by '" +++ module_name +++ "'"
		# (backupproject,ps)	= getProject ps
		# ps					= setProject project ps
		# ps					= doPathsDialog line ap pp prjpaths (set defp) ps
		# (project,ps)			= getProject ps
		# (defpaths`,ps)		= getCurrentPaths ps
		# prjpaths`				= PR_GetPaths project
		# newpaths				=  not (EQStrings (SortStrings defp) (SortStrings defpaths`))
								|| not (EQStrings (SortStrings prjpaths) (SortStrings prjpaths`))
		# ps					= setProject backupproject ps
		= (ps,project,newpaths)
	where
		set defp ao ps
			# (prj,ps)	= getProject ps
			# prj		= PR_SetPaths False defp ao prj
			= setProject prj ps

ProcessCompilerMsg cstate _ _ path abcpath SyntaxError fileinfo dircache abccache project ps
	= (cstate,(fileinfo,abccache,project, False, False, abcpath,[!],dircache,ps))

ProcessCompilerMsg cstate compileOrCheckSyntax co mdn abcpath CompilerOK fileinfo dircache abccache project ps0
	| compileOrCheckSyntax == SyntaxCheck
		= (cstate,(fileinfo,abccache,project,True, False,EmptyPathname,[!],dircache,ps))
	#	(abcdate,ps)		= accFiles (FModified abcpath) ps
		(((sys,stack,version,abcoptions),(mods,_,_,objs,libs),abccache), ps)
							= accFiles (ParseABCInfoAndDependencies abcpath abcdate abccache) ps
		update				= \finfo	->
								{ finfo 
								& abcpath		= abcpath
								, abcdate		= abcdate
								, sys			= sys
								, seq_stack		= stack
								, version		= version
								, abcOptions	= abcoptions
								}
		fileinfo			= FI_UpdateFileInfo mdn.mdn_name update fileinfo
	# ((ok,paths,dircache),ps) = accFiles (AccTuple4 (LookupModulePaths mods dircache)) ps
	| not ok				// NO, should fix with add new paths dialogue...
		# line				= Level3 ["Error: Unable to find '" +++ (Hd paths).mdn_name +++ "'."]
		# ps				= showInfo line ps
		= (cstate,(fileinfo,abccache,project, False, False, abcpath,[!],dircache,ps))
	#! project				= PR_AddABCInfo mdn objs libs co project
	= (cstate,(fileinfo,abccache,project,ok, False, abcpath, paths,dircache,ps))
where
	(version,ps)		= getCurrentVers ps0

check_dependant_dates :: !Modulename !(List Modulename) !(Maybe ModuleDate) !(List ModuleDate) !*DirCache !*Files
	-> (!Bool,String,!*DirCache,!*Files)
check_dependant_dates modname mods xxx_md xxx_dd dircache files
	| isNothing xxx_md
		= (False,"["+++modname+++".icl,]: has no date",dircache,files)
	# xxx_md = fromJust xxx_md
	# (ok,ext,yyy_md,dircache,files) = find_implementation_module modname dircache files
	| not ok
		= (False,"["+++modname+++ext+++",]: not found in cached paths",dircache,files)
	| not (eqDate xxx_md yyy_md)
		= (False,"["+++modname+++ext+++",]: module has changed",dircache,files)
	= moretricks ext mods xxx_dd dircache files
where
	moretricks ext (md:!ms) (dt:!ds) dc files
		# (ok,_,_,depdate,dc,files) = find_definition_module md dc files
		| not ok
			= (False,"["+++modname+++ext+++",]: can`t find "+++md+++" in cached paths",dc,files)
		| not (eqDate depdate dt)
			= (False,"["+++modname+++ext+++",]: "+++md+++" has changed",dc,files)
		= moretricks ext ms ds dc files
	moretricks _ Nil Nil dircache files
		= (True,"Fine!",dircache,files)
	moretricks _ Nil _ dircache files
		= (False,"["+++modname+++".icl,]: more stored dates than modules??",dircache,files)
	moretricks _ _ Nil dircache files
		= (False,"["+++modname+++".icl,]: more stored modules than dates??",dircache,files)

	eqDate (ld,lt) (rd,rt)
		| lt.seconds	<> rt.seconds	= False
		| lt.minutes	<> rt.minutes	= False
		| lt.hours		<> rt.hours		= False
		| ld.day		<> rd.day		= False
		| ld.month		<> rd.month		= False
		| ld.year		<> rd.year		= False
		= True

	find_implementation_module module_name dircache files
		# (ok,_,yyy_md,dircache,files) = DC_HSearch module_name ".icl" dircache files
		| ok
			= (ok,".icl",yyy_md,dircache,files)
		# (ok,_,yyy_md,dircache,files) = DC_HSearch module_name ".hs" dircache files
		| ok
			= (ok,".hs",yyy_md,dircache,files)
		# (ok,_,yyy_md,dircache,files) = DC_HSearch module_name ".lhs" dircache files
		| ok
			= (ok,".lhs",yyy_md,dircache,files)
			= (ok,".icl",yyy_md,dircache,files)

check_module_options :: !String !FileInfo !CompilerOptions !ProjectCompilerOptions !Bool !Int -> (!Bool,{#Char});
check_module_options modname info=:{version,abcOptions} co project_compiler_options use_64_bit_processor expectedVersion
	| version <> expectedVersion
		= (False,"["+++modname+++".icl,]: .abc out of date, different abc version.")

	|	abcOptions.abc64Bits<>use_64_bit_processor
		= incorrect_option modname "32 or 64 bit code"
	|	abcOptions.abcExportLocalLabels <> expectedExportLocalLabels
		= incorrect_option modname "Export local labels or link dynamic"
	|	abcOptions.abcTimeProfile			<> expectedTimeProfile
		= incorrect_option modname "Time Profiling"
	|	abcOptions.abcDescriptors <> expectedDescriptors
		= incorrect_option modname "Generate descriptors or heap profiling"
	|	abcOptions.abcStrictnessAnalysis	<> expectedStrictnessAnalysis
		= incorrect_option modname "Strictness Analysis"
//	||	abcOptions.abcGenerateComments		<> expectedGenerateComments	// <- do we need to regenerate for this?
	|	(expectedGenerateComments && (not abcOptions.abcGenerateComments)) // want comments but don't have
		= incorrect_option modname "Generate Comments"
	|	abcOptions.abcReuseUniqueNodes		<> expectedReuseUniqueNodes
		= incorrect_option modname "Reuse Unique Nodes"
	| abcOptions.abcFusion<>co.fusion
		= incorrect_option modname "Fusion"
	| abcOptions.abcDynamics <> project_compiler_options.pco_dynamics
		= incorrect_option modname "Dynamics"
	= (True,"")
where
	expectedDescriptors 		= project_compiler_options.pco_desc_exl || project_compiler_options.pco_link_dynamic || (project_compiler_options.pco_memory_profiling && (not co.neverMemoryProfile))
	expectedTimeProfile			= project_compiler_options.pco_time_profiling && (not co.neverTimeProfile)
	expectedStrictnessAnalysis	= co.sa
	expectedExportLocalLabels	= project_compiler_options.pco_desc_exl || project_compiler_options.pco_link_dynamic
	expectedGenerateComments	= co.gc
	expectedReuseUniqueNodes	= co.reuseUniqueNodes

	incorrect_option modname option_name
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. ("+++option_name+++")")

//-- Generate Phase...

//	Generate code for the designated module.
GenCodeTheProjectModule :: !Bool !Bool !CodeGenerateAsmOrCode !ModuleDirAndName !Pathname !*ABCCache !FileInfoCache !Project !*GeneralSt -> *(*GeneralSt,*ABCCache,FileInfoCache,Project,Bool,Pathname)
GenCodeTheProjectModule outofdate sys genAsmOrCode mdn abc_path abccache fileinfo project ps
	# module_name = mdn.mdn_name
	# (proc,ps) = getCurrentProc ps
	# ((info,abccache,fileinfo), ps) = FI_GetFileInfo proc mdn abccache fileinfo ps
	| not outofdate
		= (ps,abccache,fileinfo,project,True,info.objpath)
	# ps					= showInfo (Level2 ("Generating code for '" +++ module_name +++ "'.")) ps
	# (startupdir,ps)		= getStup ps
	  (cgen,ps)				= getCurrentCgen ps
	  (use_compiler_process_ids,compiler_process_ids,ps) = get_use_compiler_process_ids_and_compiler_process_ids ps
	  (neverTimeProfile,ps) = get_neverTimeProfile_option module_name project ps
	  ao = PR_GetApplicationOptions project
	  timeprofile = ao.profiling && (not neverTimeProfile)
	  obj_path = ModuleDirAndNameToObjSystemPathname proc mdn
	  cgo = PR_GetCodeGenOptions project
	# (obj_path,res,compiler_process_ids,ps) = CodeGen cgen use_compiler_process_ids updateErrorWindow genAsmOrCode abc_path obj_path timeprofile cgo proc ao startupdir compiler_process_ids ps
	# ps = setCompilerProcessIds compiler_process_ids ps
	| genAsmOrCode == CodeGeneration && res
		# (fileinfo,ps)		= accFiles (FI_UpdateObjDate module_name obj_path fileinfo) ps
		# project			= if sys
								(PR_SetSysCodeGenerated project)
								(PR_SetCodeGenerated module_name project)
		= (ps, abccache, fileinfo, project, res, obj_path)
	= (ps, abccache, fileinfo, project, res, obj_path)

//	Checks whether .o files in the project are out of date.
check_object_file_out_of_date :: !ModuleDirAndName !Bool !*ABCCache !FileInfoCache !Project !*GeneralSt -> *(*ABCCache,FileInfoCache,Bool,Pathname,!*GeneralSt)
check_object_file_out_of_date mdn sys abccache fileinfo project ps
	# (tp,ps)					= getCurrentProc ps
	# ((modinfo,abccache,fileinfo), ps)
								= FI_GetFileInfo tp mdn abccache fileinfo ps
	# abcexists					= modinfo.abcdate.exists
	# objexists					= modinfo.objdate.exists
	# olderdate					= Older_Date modinfo.objdate modinfo.abcdate
	# abc						= abcexists && (olderdate || not objexists)
	# mn						= mdn.mdn_name
	# cg_opt					= if sys
									(not (PR_SysUptoDate project))
									(not (PR_ABCUpToDate mn project))
	# gencode					= cg_opt || abc
	# ({be_verbose},ps)			= getPrefs ps
	# lines						= if be_verbose
									(Level3 (MakeABCOutOfDateMessage tp mn abc abcexists objexists cg_opt))
									(Level3 [])
	# ps						= verboseInfo be_verbose lines ps
	= (abccache,fileinfo, gencode, modinfo.abcpath,ps)
where
	MakeABCOutOfDateMessage :: !Processor !Modulename !Bool !Bool !Bool !Bool -> [String]
	MakeABCOutOfDateMessage tp mn abc abcexists objexists cgo
		| abcexists
			| abc || not objexists
				| cgo
					| objexists
						= ["[" +++ MakeObjPathname tp mn +++ ",]: is older than .abc file, new paths or new code generator options set"]
					= ["[" +++ MakeObjPathname tp mn +++ ",]: does not exist, new paths or new code generator options set"]
				| objexists
					= ["[" +++ MakeObjPathname tp mn +++ ",]: is older than .abc file"]
				= ["[" +++ MakeObjPathname tp mn +++ ",]: does not exist"]
			| cgo
				= ["[" +++ MakeObjPathname tp mn +++ ",]: new paths or new code generator options set"]
			= []
		= []
	
//-- Link Phase...

MakeOptionsName :: !.String !Processor -> String
MakeOptionsName path processor
	= path`+++DirSeparatorString+++"Clean System Files"+++DirSeparatorString+++"_"+++name+++MakeObjPathname processor "_options"
where
	path` = RemoveFilename path
	name  = RemoveSuffix` (RemovePath path)

CheckObjsOutOfDate gen execpath objs ps
	#	({be_verbose},ps)	= getPrefs ps
	#	execname			= RemovePath execpath
	| gen
		#	lines			= if be_verbose
									(Level3 ["[" +++ execname +++ ",]: out of date. Linking new executable."])
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	#	(date, ps)			= accFiles (FModified execpath) ps
	| not date.exists
		#	lines			= if be_verbose
									(Level3 ["[" +++ execname +++ ",]: does not exist. Linking new executable."])
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	# (ood,ps) = accFiles (check date objs) ps
	| ood
		#	lines			= if be_verbose
									(Level3 ["[" +++ execname +++ ",]: is older than object files. Linking new executable."])
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	= (False,ps)
where
	check date Nil files			= (False,files)
	check date (hd :! tl) files
		#	(objDate, files)		= FModified hd files
		| Older_Date date objDate	= (True,files)
		= check date tl files

CheckExecOutOfDate :: !Bool !Pathname !FileInfoCache !Project !*GeneralSt -> *(Bool,*GeneralSt)
CheckExecOutOfDate gen execpath fileinfo project ps
	| gen
		= (True,ps)
	#	({be_verbose},ps)	= getPrefs ps
	#	execname			= RemovePath execpath
	| not (PR_ExecUpToDate project)
		#	lines			= if be_verbose
								(Level3 ["'" +++ execname +++ "' was linked with different application options"])
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	#	(date, ps)			= accFiles (FModified execpath) ps
	#	youngest			= YoungestObj NoDate fileinfo
	#	link				= youngest.exists && (not date.exists || Older_Date date youngest)
	| link
		#	lines			= if be_verbose
								(if date.exists
									(Level3 ["[" +++ execname +++ ",]: is older than object files. Linking new executable."])
									(Level3 ["[" +++ execname +++ ",]: does not exist. Linking new executable."])
									)
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	= (False,ps)

//-- dircache functions

GetPathNames :: !(List String) !(List String) !*DirCache -> (.Bool,List String,!*DirCache)
GetPathNames Nil acc cache
	= (True, acc, cache)
GetPathNames (fn:!fns) acc cache
	# (ok,pn,_,cache) = DC_Search fn cache
	| ok
		= GetPathNames fns (pn +++ DirSeparatorString +++ fn :! acc) cache
		= (False, (fn :! Nil), cache)

// Lookup Module Paths in Directory Cache
LookupModulePaths :: !(List .String) !*DirCache !*Files -> (Bool,.[!ModuleDirAndName],*DirCache,!*Files);
LookupModulePaths Nil dc files
	= (True,[!],dc,files)
LookupModulePaths (mn :! ms) dc files
	# (ok,ext,pt,_,dc,files) = find_definition_module mn dc files
	| not ok
		= (False, [!{mdn_dir="",mdn_name=mn}], dc, files)
	# (ok,ps,dc,files) = LookupModulePaths ms dc files
	| not ok
		= (ok, ps, dc,files)
	= (ok,[!{mdn_dir=pt,mdn_name=RemoveSuffix mn} : ps], dc, files)

find_definition_module module_name dircache files
	# (ok,pt,yyy_md,dircache,files) = DC_HSearch module_name ".dcl" dircache files
	| ok
		= (ok,".dcl",pt,yyy_md,dircache,files)
	# (ok,pt,yyy_md,dircache,files) = DC_HSearch module_name ".hs" dircache files
	| ok
		= (ok,".hs",pt,yyy_md,dircache,files)
	# (ok,pt,yyy_md,dircache,files) = DC_HSearch module_name ".lhs" dircache files
	| ok
		= (ok,".lhs",pt,yyy_md,dircache,files)
		= (ok,".dcl",pt,yyy_md,dircache,files)
	
//-- Handle DirCache Setup Errors...

HandleDCErrors :: !Bool ![String] ![Warn] !*GeneralSt -> *GeneralSt
HandleDCErrors _ [] [] ps
	= ps
HandleDCErrors verbose [] warns ps
	# line				= Level3 (flatten
							[[ "Warning: Multiple file instances: '" +++ n +++ "'."
							,  "First found at: '" +++ p +++ "'."
							:[ "Also found at: '" +++ p +++ "'."
							\\ (_,p,_) <- c
							]
							] \\ Warn n p c <- warns])
	= verboseInfo verbose line ps
HandleDCErrors verbose errs _ ps
	# line				= Level3 ["Warning: Unable to setup directory cache: '" +++ err +++ "'." \\ err <- errs]
	= verboseInfo verbose line ps

//--

ClearCompilerCache` ps = PlatformDependant ps (clear ps)
where
  clear ps
	# (method,ps) = getCurrentMeth ps
	= case method of
		CompileAsync _
			# (compiler_process_ids,ps) = getCompilerProcessIds ps
			# (_,ps) = ClearCompilerCaches compiler_process_ids ps;
			-> ps
		_	
			# (ccstring,ps)			= getCurrentComp ps
			# (startupdir,ps)		= getStup ps
			# (_,ps) = ClearCompilerCache ccstring startupdir ps
			-> ps 
