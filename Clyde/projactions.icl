implementation module Clyde.projactions

import StdEnv

// MacOS
import Platform		// application_path
// Batch
import IdeState		// abortLog, initGeneral, (ls,gst_world)
import PmEnvironment// EnvsFileName, openEnvironments
// IDE
import PmDriver		// CleanupCont, BringProjectUpToDate
import PmProject	// ReadProjectFile
import logfile		// openLogfile
import messwin		// showInfo, closeInfo

import StdDebug
import qualified Data.Maybe as DM
import qualified System.Environment as SE
import System._Unsafe

cleanhome :: !String
cleanhome	=: accUnsafe getenv			// no trailing slash...
where
	getenv :: !*World -> (!String,!*World)
	getenv world
		#!	(mh,world)	= 'SE'.getEnvironmentVariable "CLEAN_HOME" world
		| 'SE'.isJust mh
			= ('SE'.fromJust mh,world)
			= (defaulthome,world)
defaulthome = "/usr/local/Cellar/clean-itasks/20160630/"

build :: !Bool !String !*World -> (!Int,!*World)
build force proj_path world
	| trace_n ("projactions:build\t"+++cleanhome) False = undef
	//	ed_ask_save_all False True (enableProjectMenu o bring_project_upto_date force cont o disableProjectMenu) ps
	//	mb_update_undoinfo ps
	// '/Users/dvanarkelmaccom/Documents/CleanLab/Clyde.prj'
	// '/usr/local/Cellar/clean-itasks/20151022/etc/'

	# envloc					= cleanhome +++ "/etc/"
	# envspath					= envloc +++. EnvsFileName
	# startup					= cleanhome
	# (envs,world)				= openEnvironments startup envspath world

	| trace_n ("proj_path: '"+++proj_path+++"'") False = undef
	| trace_n ("application_path: '"+++application_path ""+++"'") False = undef
	| trace_n ("startup: '"+++startup+++"'") False = undef
	| trace_n ("envspath: '"+++envspath+++"'") False = undef
	| trace_n ("#envs: '"+++toString (length envs)+++"'") False = undef

	# default_compiler_options	= DefaultCompilerOptions

// write logging to <proj>.log
	# (ok,logfile,world)		= openLogfile proj_path world
	# ((proj,ok,err),world)		= accFiles (ReadProjectFile proj_path startup) world

	# iniGeneral				= initGeneral True default_compiler_options startup proj_path proj envs logfile
	# ps = {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
	# ps						= bring_project_upto_date force cont ps
	= (42,ps.gst_world)
where
	cont exepath linked ok ps
		| trace_n ("cont\t"+++exepath+++"\t"+++toString linked+++"\t"+++toString ok) False = undef
		| linked || not ok
			= closeInfo ps
		= showInfo (Level1 "Project is up to date") ps

/*
proj_path: '/Users/dvanarkelmaccom/Documents/CleanLab/bake.prj'
application_path: '/Users/dvanarkelmaccom/Documents/CleanLab/Clyde.app/Contents/MacOS/'
startup: '/usr/local/Cellar/clean-itasks/20151022/'
envspath: '/usr/local/Cellar/clean-itasks/20151022//etc/IDEEnvs'
#envs: '8'
/usr/local/Cellar/clean-itasks/20151022//lib/exe/cocl	cocl -dynamics -wmt -lset -ou bake -P "/Users/dvanarkelmaccom/Documents/CleanLab:/Users/dvanarkelmaccom/Documents/CleanLab/Bake:/Users/dvanarkelmaccom/Documents/CleanLab/CleanPlatform:/Users/dvanarkelmaccom/Documents/CleanLab/CleanPlatform/Deprecated/StdLib:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/cpm:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/cpm/Posix:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/BatchBuild:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/MacOSX:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/Pm:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/Util:/Users/dvanarkelmaccom/Documents/CleanLab/cleantools/Interfaces/LinkerInterface:/usr/local/Cellar/clean-itasks/20151022//lib/StdEnv:/usr/local/Cellar/clean-itasks/20151022//lib/Dynamics" -RE "/usr/local/Cellar/clean-itasks/20151022/Temp/errors" -RO "/usr/local/Cellar/clean-itasks/20151022/Temp/out"

cont	/Users/dvanarkelmaccom/Documents/CleanLab/bake.exe	False	False
*/

buildAndRun :: !*World -> (!Int,!*World)
buildAndRun env
	//	ed_ask_save_all False True (enableProjectMenu o bring_project_upto_date False cont o disableProjectMenu) ps
	//	mb_update_undoinfo ps
	= (42,env)
where
	cont execpath linked ok ps
		# ps		= closeInfo ps
		| not ok
			= ps
		//	(lo,ps)					= getFromProject PR_GetLinkOptions ps
		//  (prj_path`,ps)			= getFromProject PR_GetRootDir ps
		//	(app_path,ps)			= getStup ps
		//	execpath				= fulPath app_path prj_path` execpath
		//	= RunProgram execpath ps
		= (42,env)

run :: !*World -> (!Int,!*World)
run env
	| trace_n "'run' not yet implemented..." False = undef
	//	(app_path,ps)				= getStup ps
	//	(prj_path`,ps)				= getFromProject PR_GetRootDir ps
	//	(execpath,ps)				= getFromProject PR_GetExecPath ps
	//	execpath					= fulPath app_path prj_path` execpath
	//	= RunProgram execpath ps
	= (42,env)

bring_project_upto_date :: !Bool CleanupCont !*GeneralSt -> *GeneralSt
bring_project_upto_date force continuation ps
	//	ps				= ew_safe_close ps							// close error window
	//	ps				= tw_safe_close ps							// close types window
	= BringProjectUptoDate force continuation ps


