implementation module Clyde.projactions

import StdEnv

// MacOS
//import Platform		// application_path
// Batch
import IdeState		// abortLog, initGeneral, (ls,gst_world)
import PmEnvironment// EnvsFileName, openEnvironments
// IDE
import PmDriver		// CleanupCont, BringProjectUpToDate
import PmProject	// ReadProjectFile
import logfile		// openLogfile
import messwin		// showInfo, closeInfo

import StdDebug
import Cocoa.UserDefaults
import Clyde.ClydeApplicationController

cleanhome :: !*env -> (!String,!*env)
cleanhome env
		= stringForKey "CLEAN_HOME" env

build :: !Bool !String !(String Bool Bool *GeneralSt -> *GeneralSt) !*World -> (!Int,!*World)
build force proj_path cont world
	//	ed_ask_save_all False True (enableProjectMenu o BringProjectUptoDate force cont o disableProjectMenu) ps
	//	mb_update_undoinfo ps
	#!	(startup,world)			= cleanhome world
		envloc					= startup +++ "/etc/"
		envspath				= envloc +++. EnvsFileName
		(envs,world)			= openEnvironments startup envspath world

	| trace_n ("proj_path: '"+++proj_path+++"'") False = undef
//	| trace_n ("application_path: '"+++application_path ""+++"'") False = undef
	| trace_n ("startup: '"+++startup+++"'") False = undef
	| trace_n ("envspath: '"+++envspath+++"'") False = undef
	| trace_n ("#envs: '"+++toString (length envs)+++"'") False = undef

	# default_compiler_options	= DefaultCompilerOptions

// write logging to <proj>.log
	#	(ok,logfile,world)		= openLogfile proj_path world
		world					= openTypeWindow world
		world					= openLogWindow world
	# ((proj,ok,err),world)		= accFiles (ReadProjectFile proj_path startup) world
	| not ok && trace_n ("failed to read project file: '"+++proj_path+++"' with error: '"+++err+++"'") True
		#!	logfile				= logfile <<< ("failed to read project file: '"+++proj_path+++"' with error: '"+++err+++"'")
			(ok,world)			= fclose logfile world
		= (0,world)
	# (ok,target_name)			= testGeneral True default_compiler_options startup proj_path proj envs
	| not ok && trace_n ("Unable to find project environment for target '" +++ target_name +++ "' in available environments.") True
		#!	logfile				= logfile <<< ("Unable to find project environment for target '" +++ target_name +++ "' in available environments.\n")
			(ok,world)			= fclose logfile world
		= (0,world)
	# iniGeneral				= initGeneral True default_compiler_options startup proj_path proj envs logfile
	# ps 						= {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
	# ps						= BringProjectUptoDate force cont ps
	= (42,ps.gst_world)
