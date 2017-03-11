definition module PmFiles

//	File I/O routines for the project manager.

import StdPathname, StdMaybe
import UtilOptions
import PmCompilerOptions
import PmTypes

ProjectTable			:: OptionsTable ProjectGlobalOptions
project_table			:: OptionsTable ProjectGlobalOptions
edit_options_table		:: OptionsTable ProjectGlobalOptions
project_template_table	:: OptionsTable ProjectGlobalOptions
CompilerOptionsTable	:: OptionsTable CompilerOptions
CodeGenOptionsTable		:: OptionsTable CodeGenOptions
LinkOptionsTable		:: OptionsTable LinkOptions
ApplicationOptionsTable	:: OptionsTable ApplicationOptions

ProjectFileVersion :== "1.4"

:: ProjectGlobalOptions =
	{ pg_codegen			:: CodeGenOptions
	, pg_application		:: ApplicationOptions
	, pg_link				:: LinkOptions
	, pg_projectPaths		:: List Pathname
	, pg_otherModules		:: List ModInfoAndName
	, pg_mainModuleInfo		:: ModInfoAndName
	, pg_staticLibInfo		:: StaticLibInfo
	, pg_target				:: String					// specify used environment
	, pg_execpath			:: String					// move to ApplicationOptions
	, pg_dynamic			:: !ProjectDynamicInfo
	, pg_root_directory		:: !String
	, pg_precompile			:: !Maybe String			// experiment: move to LinkOptions
	, pg_postlink			:: !Maybe String			// experiment: move to LinkOptions
	}

:: ProjectDynamicInfo =
	{ dyn_syms				:: !List UndefSymbol
	, dyn_mods				:: !List UndefModule
	, dyn_objs				:: !List Pathname
	, dyn_slibs				:: !List Pathname
	, dyn_dlibs				:: !List Pathname
	, dyn_paths				:: !List Pathname
	}

EmptyDynamicInfo	:: ProjectDynamicInfo

:: UndefSymbol =
	{ symbol_name	:: !String
	, path			:: !String
	}
 
:: UndefModule =
	{ module_name	:: !String
	, path			:: !String
	}
 
EmptyUndefSymbol	:: UndefSymbol
EmptyUndefModule	:: UndefModule
