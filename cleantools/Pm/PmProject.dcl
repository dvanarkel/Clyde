definition module PmProject

import PmTypes
from UtilStrictLists import :: List
import PmFiles
import StdMaybe, StdFile


DclMod :== True
IclMod :== False

:: Def_and_Imp		:== Bool

:: Project

SaveProjectFile	::
	!String			// path to projectfile
	!Project		// the project
	!String			// the application directory
	!*Files			// the filesystem environment
	->
	( !Bool			// success
	, !*Files		// returned filesystem
	);

ReadProjectFile	::
	!String			// path to projectfile
	!String			// the application directory
	!*Files			// the filesystem environment
	->
	(!(!Project		// the project
	, !Bool			// success: true if successful except when failed to close
					// project file. Then success is true but errmsg (next entry)
					// is nonempty.
	, !{#Char}		// errmsg: reports the encountered error if any
	),!*Files		// returned filesystem
	)

save_project_template_file :: !String !Project !String !*Files -> (!Bool, !*Files)

read_project_template_file	:: !String !String !*Files -> (!(!Bool, !Project, !{#Char}),!*Files)

change_root_directory_of_project :: !{#Char} !{#Char} !Project -> Project

getDynamicInfo	:: !Project -> (ProjectDynamicInfo,Project)
setDynamicInfo	:: !.ProjectDynamicInfo !.Project -> .Project

//--

PR_InitProject	:: Project
PR_ProjectSet	:: !Project -> Bool
PR_NewProject	:: !String !EditWdOptions !CompilerOptions !CodeGenOptions !ApplicationOptions
					!(List String) !LinkOptions -> Project

PR_SetBuilt					:: ![!ModuleDirAndName] !u:Project -> u:Project
PR_ClearDependencies		:: !Project -> Project
PR_SetRoot					:: !String !EditWdOptions !CompilerOptions !Project -> Project
PR_SetRelativeRootDir	    :: !String !Project -> Project
PR_SetCompiled				:: !Modulename !Project -> Project
PR_SetCodeGenerated			:: !Modulename !Project -> Project
PR_SetSysCodeGenerated		:: !Project -> Project
PR_SetLinked				:: !Project -> Project
PR_SetSaved					:: !Project -> Project
PR_SetCodeGenOptions		:: !CodeGenOptions !Project -> Project
PR_SetApplicationOptions	:: !ApplicationOptions !Project -> Project
PR_SetPaths					:: !Bool !(List String) !(List String) !Project -> Project

PR_GetCodeGenOptions		:: !Project -> CodeGenOptions
//PR_GetProcessor				:: !Project -> Processor
PR_GetApplicationOptions	:: !Project -> ApplicationOptions
PR_GetPaths					:: !Project -> List String
PR_GetRootModuleName		:: !Project -> String
PR_GetRootPathName			:: !Project -> (!String,!Project)
PR_GetRootModuleDir			:: !Project -> String
PR_GetRootModuleDirAndName	:: !Project -> (!ModuleDirAndName,!Project)
PR_GetRootDir :: !Project -> String
PR_GetRelativeRootDir :: !Project -> String
PR_GetModulenames			:: !Bool !Def_and_Imp !Project -> (List String,Project)
PR_GetDirAndModulenames		:: !Project -> ([!ModuleDirAndName],Project)
PR_GetOpenModulenames		:: !Project -> List String
PR_GetModuleStuff			:: !Project -> List (Modulename,String,Modulename,String)

PR_SrcUpToDate				:: !Modulename !Project -> Bool
PR_ABCUpToDate				:: !Modulename !Project -> Bool
PR_SysUptoDate				:: !Project -> Bool
PR_ExecUpToDate				:: !Project -> Bool
PR_Saved					:: !Project -> Bool

PR_GetModuleInfo			:: !Modulename !Project -> Maybe ModInfo

PR_UpdateModule				:: !Modulename !(ModInfo -> ModInfo) !Project -> Project
PR_UpdateModules			:: ![Modulename] !(ModInfo -> ModInfo) !Project -> Project

PR_SetLinkOptions			:: !Project !LinkOptions -> Project
PR_GetLinkOptions			:: !Project -> LinkOptions

PR_AddABCInfo				:: !ModuleDirAndName !(List LinkObjFileName) !(List LinkLibraryName) !CompilerOptions !Project -> Project

PR_GetABCLinkInfo			:: !Project -> ABCLinkInfo

PR_GetStaticLibsInfo		:: !Project -> StaticLibInfo
PR_SetStaticLibsInfo		:: !StaticLibInfo !Project -> Project

PR_GetTarget				:: !Project -> String
PR_SetTarget				:: !String !Project -> Project

PR_GetExecPath				:: !Project -> String
PR_SetExecPath				:: !String !Project -> Project

SL_Add		:: !String !StaticLibInfo -> StaticLibInfo
SL_Rem		:: ![String] !String !String !StaticLibInfo -> StaticLibInfo
SL_Libs		:: !StaticLibInfo -> List String
SL_Dcls		:: !StaticLibInfo -> List String
SL_Deps		:: !StaticLibInfo -> List String
SL_SetLibs	:: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDcls	:: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDeps	:: !(List String) !StaticLibInfo -> StaticLibInfo

PR_SetPrecompile	:: !(Maybe String) !Project -> Project
PR_GetPrecompile	:: !Project -> (!Maybe String, !Project)
PR_SetPostlink		:: !(Maybe String) !Project -> Project
PR_GetPostlink		:: !Project -> (!Maybe String, !Project)

make_project_dir :: !Int !{#Char} -> {#Char}
