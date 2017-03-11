implementation module PmFiles

//	File I/O routines for the project.

import StdArray, StdFunc, StdInt
import UtilNewlinesFile, UtilOptions, UtilStrictLists
import PmProject
import UtilDate
from PmPath import convert_path_separators,convert_exec_path_separators_and_extension

ProjectFileVersion :== "1.4"

:: ProjectGlobalOptions =
	{ pg_codegen			:: CodeGenOptions
	, pg_application		:: ApplicationOptions
	, pg_link				:: LinkOptions
	, pg_projectPaths		:: List Pathname
	, pg_otherModules		:: List ModInfoAndName
	, pg_mainModuleInfo		:: ModInfoAndName
	, pg_staticLibInfo		:: StaticLibInfo
	, pg_target				:: String
	, pg_execpath			:: String
	, pg_dynamic			:: !ProjectDynamicInfo
	, pg_root_directory		:: !String
	, pg_precompile			:: !Maybe String
	, pg_postlink			:: !Maybe String
	}

:: ProjectDynamicInfo =
	{ dyn_syms				:: !List UndefSymbol
	, dyn_mods				:: !List UndefModule
	, dyn_objs				:: !List Pathname
	, dyn_slibs				:: !List Pathname
	, dyn_dlibs				:: !List Pathname
	, dyn_paths				:: !List Pathname
	}

EmptyDynamicInfo :: ProjectDynamicInfo
EmptyDynamicInfo =
	{ dyn_syms				= Nil
	, dyn_mods				= Nil
	, dyn_objs				= Nil
	, dyn_slibs				= Nil
	, dyn_dlibs				= Nil
	, dyn_paths				= Nil
	}


:: UndefSymbol =
	{ symbol_name	:: !String
	, path			:: !String
	}
 
:: UndefModule =
	{ module_name	:: !String
	, path			:: !String
	}

EmptyUndefSymbol	:: UndefSymbol
EmptyUndefSymbol =
	{ symbol_name	= ""
	, path			= ""
	}
 
EmptyUndefModule	:: UndefModule
EmptyUndefModule =
	{ module_name	= ""
	, path			= ""
	}

project_root_option = SimpleOption	"ProjectRoot"							(\a->a.pg_root_directory)   (\v a->{a & pg_root_directory = v})
target_option		= SimpleOption	"Target"								(\a->a.pg_target)			(\v a->{a & pg_target=v})
code_gen_option		= GroupedOption	"CodeGen"		CodeGenOptionsTable		(\a->a.pg_codegen)			(\v a->{a & pg_codegen=v})
application_option	= GroupedOption	"Application"	ApplicationOptionsTable	(\a->a.pg_application)		(\v a->{a & pg_application=v})
link_option			= GroupedOption	"Link"			LinkOptionsTable		(\a->a.pg_link)				(\v a->{a & pg_link=v})
paths_option		= ListOption	"Paths"			PathName ""				(\a->a.pg_projectPaths)		(\v a->{a & pg_projectPaths=v})
static_option		= GroupedOption	"Static"		StaticLibsInfoTable		(\a->a.pg_staticLibInfo)	(\v a->{a & pg_staticLibInfo=v})
precompile_option	= SimpleOption	"Precompile"							(\a->unwrap a.pg_precompile)(\v a->{a & pg_precompile = wrap v})
postlink_option		= SimpleOption	"Postlink"								(\a->unwrap a.pg_postlink)	(\v a->{a & pg_postlink = wrap v})

// Making Precompile & Postlink 'List' options is probably prettier...
unwrap Nothing = ""
unwrap (Just s) = s
	
wrap "" = Nothing
wrap s = Just s

ProjectGlobalOptionsTable :: OptionsTable ProjectGlobalOptions
ProjectGlobalOptionsTable =
	{	project_root_option
	,	target_option
	,	SimpleWithStringConversionOption convert_exec_path_separators_and_extension "Exec" (\a->a.pg_execpath) (\v a->{a & pg_execpath=v})
	,	code_gen_option
	,	application_option
	,	link_option
	,	paths_option
	,	static_option
	,	precompile_option
	,	postlink_option
	}

project_template_global_options_table :: OptionsTable ProjectGlobalOptions
project_template_global_options_table =
	{	project_root_option
	,	target_option
	,	code_gen_option
	,	application_option
	,	link_option
	,	paths_option
	,	static_option
	,	precompile_option
	,	postlink_option
	}

instance fromString Bool
where
	fromString "False"
		=	False
	fromString _ 
		=	True

name_option = SimpleOption	"Name" (\a->a.name) (\v a->{a & name=v})
dir_option = SimpleWithStringConversionOption convert_path_separators "Dir" (\a->a.info.dir) (\v a->{a & info.dir=v})
compiler_option = GroupedOption	"Compiler" CompilerOptionsTable	(\a->a.info.compilerOptions)(\v a->{a & info.compilerOptions=v})
needed_obj_files_option = ListOption "NeededObjFiles" ObjectFile "" (\a->a.info.abcLinkInfo.linkObjFileNames)
																  	(\v a->{a & info.abcLinkInfo.linkObjFileNames=v})
needed_libraries_option = ListOption "NeededLibraries" Library "" (\a->a.info.abcLinkInfo.linkLibraryNames)
																  (\v a->{a & info.abcLinkInfo.linkLibraryNames=v}) 

dcl_option = GroupedOption "Dcl" EditWdOptionsTable (\a->a.info.mod_edit_options.defeo) (\v a->{a & info.mod_edit_options.defeo=v})
dcl_open_option = SimpleOption "DclOpen" (\a->a.info.mod_edit_options.defopen) (\v a->{a & info.mod_edit_options.defopen=v})
icl_option = GroupedOption "Icl" EditWdOptionsTable (\a->a.info.mod_edit_options.impeo) (\v a->{a & info.mod_edit_options.impeo=v})
icl_open_option	= SimpleOption "IclOpen" (\a->a.info.mod_edit_options.impopen) (\v a->{a & info.mod_edit_options.impopen=v})

ModInfoAndNameTable :: OptionsTable ModInfoAndName
ModInfoAndNameTable =
	{ name_option
	, dir_option
	, compiler_option
	, dcl_option
	, dcl_open_option
	, icl_option
	, icl_open_option
	, needed_obj_files_option
	, needed_libraries_option
	}

ModInfoAndNameEntry = GroupedOption "Module" ModInfoAndNameTable id const

prj_mod_info_and_name_table :: OptionsTable ModInfoAndName
prj_mod_info_and_name_table
	= { name_option, dir_option, compiler_option, needed_obj_files_option, needed_libraries_option }

prj_mod_info_and_name_entry = GroupedOption "Module" prj_mod_info_and_name_table id const

prp_mod_info_and_name_table :: OptionsTable ModInfoAndName
prp_mod_info_and_name_table
	= { name_option, dir_option, dcl_option, dcl_open_option, icl_option, icl_open_option }

prp_mod_info_and_name_entry = GroupedOption "Module" prp_mod_info_and_name_table id const

ProjectTable :: OptionsTable ProjectGlobalOptions
ProjectTable
	# main_module_option = GroupedOption "MainModule" ModInfoAndNameTable (\a->a.pg_mainModuleInfo)	(\v a->{a & pg_mainModuleInfo=v})
	# other_modules_option = ListOption "OtherModules" ModInfoAndNameEntry {info=EmptyModInfo,name=""} (\a->a.pg_otherModules) (\v a->{a & pg_otherModules=v})
	= make_project_table main_module_option other_modules_option

project_table :: OptionsTable ProjectGlobalOptions
project_table
	# main_module_option = GroupedOption "MainModule" prj_mod_info_and_name_table (\a->a.pg_mainModuleInfo)	(\v a->{a & pg_mainModuleInfo=v})
	# other_modules_option = ListOption "OtherModules" prj_mod_info_and_name_entry {info=EmptyModInfo,name=""} (\a->a.pg_otherModules) (\v a->{a & pg_otherModules=v})
	= make_project_table main_module_option other_modules_option

edit_options_table :: OptionsTable ProjectGlobalOptions
edit_options_table
	# main_module_option = GroupedOption "MainModule" prp_mod_info_and_name_table (\a->a.pg_mainModuleInfo)	(\v a->{a & pg_mainModuleInfo=v})
	# other_modules_option = ListOption "OtherModules" prp_mod_info_and_name_entry {info=EmptyModInfo,name=""} (\a->a.pg_otherModules) (\v a->{a & pg_otherModules=v})
	= { main_module_option, other_modules_option }

make_project_table :: !(OptionsTableEntry ProjectGlobalOptions) !(OptionsTableEntry ProjectGlobalOptions) -> OptionsTable ProjectGlobalOptions
make_project_table main_module_option other_modules_option
	=	// +++ order is important here
	{	GroupedOption "Global" ProjectGlobalOptionsTable id const
	,	main_module_option
	,	other_modules_option
	,	GroupedOption	"Dynamic"		DynamicInfoTable			(\a->a.pg_dynamic)			(\v a->{a & pg_dynamic=v})
	}

project_template_table :: OptionsTable ProjectGlobalOptions
project_template_table
	= { GroupedOption "Global" project_template_global_options_table id const }

EmptyModInfo :: ModInfo
EmptyModInfo
	# defaultEditWdOptions = {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize}
	# mod_edit_options = {defeo=defaultEditWdOptions,impeo=defaultEditWdOptions,
						  defopen=False,impopen=False}
	= {	dir		= EmptyPathname,
		compilerOptions = DefaultCompilerOptions,
		mod_edit_options = mod_edit_options,
		abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} }
where
	DefaultEditOptions :: EditOptions;
	DefaultEditOptions =
		{	newlines	= HostNativeNewlineConvention
/*		{	tabs		= 4
		,	fontname	= "Courier"
		,	fontsize	= 9
		,	autoi		= True
		,	newlines	= HostNativeNewlineConvention
		,	showtabs	= False
		,	showlins	= False
		,	showsync	= True
*/		}

CompilerOptionsTable :: OptionsTable CompilerOptions
CompilerOptionsTable =
	{
		SimpleOption "NeverMemoryProfile"	(\a->a.neverMemoryProfile)	(\v a->{a & neverMemoryProfile=v}),
		SimpleOption "NeverTimeProfile"		(\a->a.neverTimeProfile)	(\v a->{a & neverTimeProfile=v}),
		SimpleOption "StrictnessAnalysis"	(\a->a.sa)					(\v a->{a & sa=v}),
		SimpleOption "ListTypes"			(\a->a.listTypes)			(\v a->{a & listTypes=v}),
		SimpleOption "ListAttributes"		(\a->a.attr)				(\v a->{a & attr=v}),
		SimpleOption "Warnings"				(\a->a.gw)					(\v a->{a & gw=v}),
		SimpleOption "Verbose"				(\a->a.bv)					(\v a->{a & bv=v}),
		SimpleOption "ReadableABC"			(\a->a.gc)					(\v a->{a & gc=v}),
		SimpleOption "ReuseUniqueNodes"		(\a->a.reuseUniqueNodes)	(\v a->{a & reuseUniqueNodes=v}),
		SimpleOption "Fusion"				(\a->a.fusion)				(\v a->{a & fusion=v})
	}

CodeGenOptionsTable :: OptionsTable CodeGenOptions
CodeGenOptionsTable	=
	{
		SimpleOption "CheckStacks"		(\a->a.cs)		(\v a->{a & cs=v}),
		SimpleOption "CheckIndexes" 	(\a->a.ci)		(\v a->{a & ci=v})
//		SimpleOption "KeepABC" 			(\a->a.kaf)		(\v a->{a & kaf=v}),
//		SimpleOption "TargetProcessor"	(\a->a.tp)		(\v a->{a & tp=v})
	}

instance fromString Int
where
	fromString s
		=	toInt s

ApplicationProfileOptionsTable :: OptionsTable ApplicationOptions
ApplicationProfileOptionsTable	=
	{
		SimpleOption "Memory"					(\a->a.memoryProfiling)					(\v a->{a & memoryProfiling=v}),
		SimpleOption "MemoryMinimumHeapSize"	(\a->a.memoryProfilingMinimumHeapSize)	(\v a->{a & memoryProfilingMinimumHeapSize=v}),
		SimpleOption "Time"						(\a->a.profiling)						(\v a->{a & profiling=v}),
		SimpleOption "Stack"					(\a->a.stack_traces)					(\v a->{a & stack_traces=v}),
		SimpleOption "Dynamics"					(\a->a.dynamics)						(\v a->{a & dynamics=v}),
		SimpleOption "DescExL"					(\a->a.desc_exl)						(\v a->{a & desc_exl=v})
	}

ApplicationOutputOptionsTable :: OptionsTable ApplicationOptions
ApplicationOutputOptionsTable	=
	{
		SimpleOption "Output"		(\a->a.o)						(\v a->{a & o=v}),
		SimpleOption "Font"			(\a->a.fn)						(\v a->{a & fn=v}),
		SimpleOption "FontSize"		(\a->a.fs)						(\v a->{a & fs=v}),
		SimpleOption "WriteStdErr"	(\a->a.write_stderr_to_file)	(\v a->{a & write_stderr_to_file=v})
	}

ApplicationOptionsTable :: OptionsTable ApplicationOptions
ApplicationOptionsTable	=
	{
		SimpleOption "HeapSize"						(\a->a.hs)					(\v a->{a & hs=v}),
		SimpleOption "StackSize"					(\a->a.ss)					(\v a->{a & ss=v}),
		SimpleOption "ExtraMemory"					(\a->a.em)					(\v a->{a & em=v}),
		SimpleOption "IntialHeapSize"				(\a->a.initial_heap_size)	(\v a->{a & initial_heap_size=v}),
		SimpleOption "HeapSizeMultiplier"			(\a->a.heap_size_multiple)	(\v a->{a & heap_size_multiple=v}),
		SimpleOption "ShowExecutionTime"			(\a->a.set)					(\v a->{a & set=v}),
		SimpleOption "ShowGC"						(\a->a.sgc)					(\v a->{a & sgc=v}),
		SimpleOption "ShowStackSize"				(\a->a.pss)					(\v a->{a & pss=v}),
		SimpleOption "MarkingCollector"				(\a->a.marking_collection)	(\v a->{a & marking_collection=v}),
		SimpleOption "DisableRTSFlags"				(\a->a.disable_rts_flags)	(\v a->{a & disable_rts_flags=v}),		
		SimpleOption "StandardRuntimeEnv"			(\a->a.standard_rte)		(\v a->{a & standard_rte=v}),
		GroupedOption "Profile"	ApplicationProfileOptionsTable	id				const,
		GroupedOption "Output"	ApplicationOutputOptionsTable	id				const
	}

PathName :: OptionsTableEntry {#Char}
PathName
	=	SimpleWithStringConversionOption convert_path_separators "Path"	id const

ModuleName :: OptionsTableEntry {#Char}
ModuleName
	=	SimpleOption "Module" id const

ObjectFile :: OptionsTableEntry {#Char}
ObjectFile
	=	SimpleOption "ObjectFile" id const

Library :: OptionsTableEntry {#Char}
Library
	=	SimpleOption "Library" id const

StaticLibsInfoTable :: OptionsTable StaticLibInfo
StaticLibsInfoTable =
	{ ListOption	"StaticLibs"			PathName ""		(\a->SL_Libs a)				(\v a->SL_SetLibs v a)
	, ListOption	"StaticDcls"			ModuleName ""	(\a->SL_Dcls a)				(\v a->SL_SetDcls v a)
	, ListOption	"StaticDeps"			ModuleName ""	(\a->SL_Deps a)				(\v a->SL_SetDeps v a)
	}

LinkOptionsTable :: OptionsTable LinkOptions
LinkOptionsTable	=
	{ ListOption	"ExtraObjects"			PathName ""	(\a->a.extraObjectModules)		(\v a->{a & extraObjectModules=v})
	, ListOption	"ExtraLibraries"		PathName ""	(\a->a.libraries)				(\v a->{a & libraries=v})
	, SimpleOption	"LinkMethod"						(\a->a.method)					(\v a->{a & method = v})
	, SimpleOption	"GenerateRelocations"				(\a->a.generate_relocations)	(\v a->{a & generate_relocations = v})
	, SimpleOption	"GenerateSymbolTable"				(\a->a.generate_symbol_table)	(\v a->{a & generate_symbol_table = v})
	, SimpleOption	"GenerateLinkMap"					(\a->a.generate_link_map)		(\v a->{a & generate_link_map = v})
	, SimpleOption	"LinkResources"						(\a->a.link_resources)			(\v a->{a & link_resources = v})
	, SimpleOption	"ResourceSource"					(\a->a.resource_source)			(\v a->{a & resource_source = v})
	, SimpleOption	"GenerateDLL"						(\a->a.generate_dll)			(\v a->{a & generate_dll = v})
	, SimpleOption	"ExportedNames"						(\a->a.dll_export_list_name)	(\v a->{a & dll_export_list_name = v})
//	, SimpleOption	"AddCarbResource"					(\a->a.add_carb_resource)		(\v a->{a & add_carb_resource = v})
	}

EditWdOptionsTable :: OptionsTable EditWdOptions
EditWdOptionsTable	=
	{
		GroupedOption "Editor"			EditOptionsTable	(\a->a.eo)			(\v a->{a & eo=v}),
		OptionalGroupedOption "WindowPosition" is_no_position_and_size WindowPositionTable (\a->a.pos_size) (\v a->{a & pos_size=v})
	}

is_no_position_and_size NoWindowPosAndSize
	= True
is_no_position_and_size _
	= False

EditOptionsTable :: OptionsTable EditOptions
EditOptionsTable	=
	{}
/*	{ SimpleOption "TabSize"		(\a->a.EditOptions.tabs)		(\v a->{EditOptions | a & tabs=v})
	, SimpleOption "Font"			(\a->a.EditOptions.fontname)	(\v a->{EditOptions | a & fontname=v})
	, SimpleOption "FontSize"		(\a->a.EditOptions.fontsize)	(\v a->{EditOptions | a & fontsize=v})
	, SimpleOption "AutoIndent"	(\a->a.EditOptions.autoi)		(\v a->{EditOptions | a & autoi=v})
	, SimpleOption "ShowTabs"	(\a->a.EditOptions.showtabs)		(\v a->{EditOptions | a & showtabs=v})
	, SimpleOption "ShowLins"	(\a->a.EditOptions.showlins)		(\v a->{EditOptions | a & showlins=v})
	}
*/
WindowPositionTable :: OptionsTable OptionalWindowPosAndSize
WindowPositionTable	=
	{
		SimpleOption "X"		(\a->(getWindowPosAndSize a).posx)	(\v a->WindowPosAndSize {getWindowPosAndSize a & posx=v}),
		SimpleOption "Y"		(\a->(getWindowPosAndSize a).posy)	(\v a->WindowPosAndSize {getWindowPosAndSize a & posy=v}),
		SimpleOption "SizeX"	(\a->(getWindowPosAndSize a).sizex)	(\v a->WindowPosAndSize {getWindowPosAndSize a & sizex=v}),
		SimpleOption "SizeY"	(\a->(getWindowPosAndSize a).sizey)	(\v a->WindowPosAndSize {getWindowPosAndSize a & sizey=v})
	}

getWindowPosAndSize (WindowPosAndSize wps) = wps
getWindowPosAndSize NoWindowPosAndSize = DefWindowPos_and_Size

DynamicInfoTable =
	{ ListOption	"Syms"	Usym esym	(\a->a.dyn_syms)	(\v a->{a & dyn_syms=v})
	, ListOption	"Mods"	Umod emod	(\a->a.dyn_mods)	(\v a->{a & dyn_mods=v})
	, ListOption	"Objs"	PathName ""	(\a->a.dyn_objs)	(\v a->{a & dyn_objs=v})
	, ListOption	"Slib"	PathName ""	(\a->a.dyn_slibs)	(\v a->{a & dyn_slibs=v})
	, ListOption	"Dlib"	PathName ""	(\a->a.dyn_dlibs)	(\v a->{a & dyn_dlibs=v})
	, ListOption	"Pths"	PathName ""	(\a->a.dyn_paths)	(\v a->{a & dyn_paths=v})
	}

esym = {symbol_name = "", path = ""}
emod = {module_name = "", path = ""}

Usym = GroupedOption "usym" USTable id const
Umod = GroupedOption "umod" UMTable id const

USTable =
	{ SimpleOption "name" (\a->a.symbol_name)		(\v a -> {a & symbol_name = v})
	, SimpleOption "path" (\a->a.UndefSymbol.path)	(\v a -> {UndefSymbol | a & path = v})
	}
 
UMTable =
	{ SimpleOption "name" (\a->a.module_name)		(\v a -> {a & module_name = v})
	, SimpleOption "path" (\a->a.UndefModule.path)	(\v a -> {UndefModule | a & path = v})
	}
