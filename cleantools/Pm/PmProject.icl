implementation module PmProject

import StdClass,StdBool,StdInt,StdString,StdArray,StdFunc,StdTuple,StdList,StdStrictLists
import StdMaybe
from StdOverloadedList import ++|,Hd
import PmPath, UtilStrictLists
from UtilDate import NoDate, :: DATE
import UtilNewlinesFile
import PmTypes
import Platform
import UtilOptions, PmFiles

::	Def_and_Imp				:== Bool;
DclMod					:== True;
IclMod					:== False;

::	WindowOpen_and_Closed	:== Bool;
WinOpen					:== True;
WinClosed				:== False;
	
::	Modification			:== Bool;
Modified				:== True;
Unmodified				:== False;

//	The Project: A list of constituent modules and their mutual dependencies

::	Project	=
	{ saved						:: !Bool
	, exec						:: !Bool					// exe linked ok?
	, inflist					:: !InfList					// list with constituent modules
	, codegenopt				:: !CodeGenOptions			// code generator options
	, code_gen_options_unchanged :: !Bool
	, applicationopt			:: !ApplicationOptions		// application options
	, linkOptions				:: !LinkOptions
	, prjpaths					:: !List String				// project paths
	, staticLibInfo				:: !StaticLibInfo
	, target					:: !String					// environment

	, dynamic_info				:: !ProjectDynamicInfo

	, relative_root_directory	:: !String			// string of '.'s, relative to .prj file
	, root_directory			:: !String
	, execpath					:: !String			// move to app_opts
	, prec						:: !Maybe String	// " (precompile command)
	, posl						:: !Maybe String	// " (postlink command)
	}

//	First element of InfList (if any) is the root module.

::	InfList		:==	List InfListItem
	
::	InfListItem	=
	{ mn	:: !Modulename		// module name
	, info	:: !ModInfo			// module info
	, src	:: !Bool			// src up to date?
	, abc	:: !Bool 			// abc up to date?
	}

::	InfUpdate 	:== InfListItem -> (!InfListItem, Bool)

PR_InitProject :: Project;
PR_InitProject =
	{ saved				= True
	, exec				= True
	, execpath			= EmptyPathname
	, inflist			= Nil
	, codegenopt		= DefCodeGenOptions
	, code_gen_options_unchanged	= True
	, applicationopt	= DefApplicationOptions
	, linkOptions		= DefaultLinkOptions
	, prjpaths			= Nil
	, staticLibInfo		= DefStaticLibInfo
	, target			= ""
	, dynamic_info		= EmptyDynamicInfo
	, relative_root_directory = "."
	, root_directory = ""
	, prec = Nothing
	, posl = Nothing
	}

PR_GetExecPath	:: !Project -> String
PR_GetExecPath {execpath} = execpath

PR_SetExecPath	:: !String !Project -> Project
PR_SetExecPath pth prj = {prj & execpath = pth}

DefStaticLibInfo =
	{ sLibs = Nil
	, sDcls = Nil
	, sDeps = Nil
	}

PR_ProjectSet :: !Project -> Bool;
PR_ProjectSet project=:{inflist=Nil}	=  False;
PR_ProjectSet project=:{inflist}		=  True;

PR_NewProject :: !String !EditWdOptions !CompilerOptions !CodeGenOptions !ApplicationOptions
				!(List String) !LinkOptions -> Project;
PR_NewProject main_module_file_name eo compilerOptions cgo ao prjpaths linkOptions
	# modname	= GetModuleName main_module_file_name;
	  dirname	= RemoveFilename main_module_file_name;
	= { PR_InitProject
	& saved			= False
	, exec			= False
	, execpath		= PlatformDependant	//MakeExecPathname main_module_file_name
						("{Project}"+++DirSeparatorString+++modname+++".exe")		// Win
						("{Project}"+++DirSeparatorString+++modname)				// Mac
	, inflist		=
		{ mn		= modname
		, info		=	{ dir		= "{Project}"//dirname
						, compilerOptions		= compilerOptions
						, mod_edit_options = {defeo=eo,impeo=eo,defopen=False,impopen=True}
						, abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil}
						}
		, src		= True
		, abc		= True
		} :! Nil
	, codegenopt	= cgo
	, code_gen_options_unchanged	= True
	, applicationopt= ao
	, prjpaths		= if (StringOccurs dirname prjpaths) prjpaths (dirname:!prjpaths)
	, linkOptions	= linkOptions
	, staticLibInfo = DefStaticLibInfo
	, target		= "StdEnv"
	, root_directory = dirname
	}

PR_SetBuilt	:: ![!ModuleDirAndName] !u:Project -> u:Project;
PR_SetBuilt used project=:{inflist=Nil}
	= project;
PR_SetBuilt used prj=:{inflist=infl=:(root:!rest),saved}
	#! len		= LLength rest
	# rest		= RemoveUnusedModules used rest
	# len`		= LLength rest
	# unchanged	= len == len`
	= {prj & saved=saved && unchanged,inflist= root:!rest}
where	
	{mn=rootmn,info} = root
	RemoveUnusedModules used list = FilterR member list
	where
		member {mn}	= module_occurs mn used && rootmn <> mn

module_occurs :: !String ![!ModuleDirAndName] -> Bool
module_occurs s [|x:xs] = x.mdn_name == s || module_occurs s xs
module_occurs s [!] =  False

PR_AddABCInfo :: !ModuleDirAndName !(List LinkObjFileName) !(List LinkLibraryName) !CompilerOptions !Project -> Project
PR_AddABCInfo mdn dep_objects dep_libraries compilerOptions project=:{inflist=Nil}
	= project
PR_AddABCInfo {mdn_dir=mod_dir,mdn_name=mod_name} dep_objects dep_libraries compilerOptions project=:{inflist}
	# inflist		= TryInsertInList mod_name mod_dir inflist
	# (inflist,_)	= UpdateList mod_name update inflist;
	= {project & saved=False,inflist=inflist}
where
	update infListItem=:{InfListItem | info}
		= (	{ InfListItem | infListItem
			& info.abcLinkInfo.linkObjFileNames		= dep_objects
			, info.abcLinkInfo.linkLibraryNames		= dep_libraries
			, info.dir								= mod_dir
			, info.compilerOptions					= compilerOptions
			}, True)

	TryInsertInList :: !String !String !InfList -> InfList
	TryInsertInList importermn importerdir Nil	// no root module...
		= Nil
	TryInsertInList importermn importerdir ((root=:{mn,info}):!rest)
		| importermn == mn	// updating root module...
			# root	= {InfListItem | root & info = {ModInfo | info & dir = importerdir}}
			= root :! rest
		# rest		= TryInsertImporter rest rest
		= root :! rest
	where
		TryInsertImporter ::	!InfList !InfList -> InfList
		TryInsertImporter Nil list
			# default_edit_options = {pos_size = NoWindowPosAndSize, eo = {newlines= HostNativeNewlineConvention}}
			# item =
				  {	mn		= importermn, 
					info	= { dir		= importerdir,
								compilerOptions = compilerOptions,
								mod_edit_options = {defeo = default_edit_options,impeo = default_edit_options,
													defopen = False, impopen = False},
								abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} },
					src		= True,
					abc		= True }
			= item :! list
		TryInsertImporter (({mn}):!rest) list
			| importermn<>mn
				= TryInsertImporter rest list
				= list

PR_ClearDependencies :: !Project -> Project
PR_ClearDependencies project=:{inflist=Nil}
	= {project & saved = False, inflist = Nil}
PR_ClearDependencies project=:{inflist=il=:(root :! rest)}
	= {project & saved = False, inflist = root` :! Nil}
where
	root` = {InfListItem | root & info.abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil}}

PR_SetRoot :: !String !EditWdOptions !CompilerOptions !Project -> Project;
PR_SetRoot root eo co project=:{inflist=Nil}
	= project;
PR_SetRoot newroot eo compilerOptions project=:{prjpaths}
	=	{project &	saved	= False,
					exec	= False,
					inflist	= {	mn		= modname,
								info	= {	dir		= dirname,
											compilerOptions		= compilerOptions,
											mod_edit_options = {defeo = eo,impeo = eo,
																impopen = True,
																defopen = False	/* nonsense, we don't know this! */
															   },
											abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} },
								src		= True,
								abc		= True
								} :! Nil,
					prjpaths= if (StringOccurs dirname prjpaths) prjpaths (dirname:!prjpaths) /* a bit iffy */
		  };
where 
	modname	= GetModuleName newroot;
	dirname	= RemoveFilename newroot;

PR_SetCompiled	:: !Modulename !Project -> Project;
PR_SetCompiled modname project=:{inflist}
	=  {project & inflist = inf`}
where 
	(inf`, _)	= UpdateList modname setcompiled inflist;
	
	setcompiled	:: !InfListItem -> (!InfListItem,!Bool);
	setcompiled itm=:{src,abc}
		= ({itm & src = True},True);
	
		
PR_SetCodeGenerated	:: !Modulename !Project -> Project;
PR_SetCodeGenerated modname project=:{inflist}
	= {project & inflist = inf`};
where 
	(inf`,_)	= UpdateList modname setcode inflist;
	
	setcode :: !InfListItem -> (!InfListItem, !Bool);
	setcode itm=:{InfListItem | src}
		= ({InfListItem | itm & abc = True}, True);
	
	
PR_SetSysCodeGenerated :: !Project -> Project;
PR_SetSysCodeGenerated project = {project & code_gen_options_unchanged = True};
	
PR_SetLinked	:: !Project -> Project;
PR_SetLinked project=:{inflist}
	=  {project & exec = True};
	
PR_SetSaved	:: !Project -> Project;
PR_SetSaved project = {Project | project & saved = True};
		
PR_SetCodeGenOptions	:: !CodeGenOptions !Project -> Project;
PR_SetCodeGenOptions options project=:{inflist,saved,codegenopt,code_gen_options_unchanged} =
	{ project
	& inflist						= infl`
	, saved							= saved && unchanged
	, code_gen_options_unchanged	= code_gen_options_unchanged && cg_unchanged
	, codegenopt					= options
	}
where 
	(infl`,_)				= infl`_;
	infl`_ | cg_unchanged	= (inflist,True);
							= P_MapR setcode inflist;
	unchanged				= cg_unchanged;
	cg_unchanged			= options == codegenopt;
	
	setcode :: !InfListItem -> (!InfListItem, !Bool);
	setcode itm=:{InfListItem | src}
		= ({InfListItem | itm & abc = False}, True);
	
	
PR_SetApplicationOptions :: !ApplicationOptions !Project -> Project;
PR_SetApplicationOptions options project=:{saved,exec,applicationopt}
	= {project & applicationopt = options, exec = exec && unchanged,saved=saved && unchanged};
	where 
		unchanged	= eqAppOpts options applicationopt;

eqAppOpts :: !ApplicationOptions !ApplicationOptions -> Bool
eqAppOpts ao1 ao2
	=	ao1.hs == ao2.hs &&
		ao1.ss == ao2.ss &&
		ao1.em == ao2.em &&
		ao1.heap_size_multiple == ao2.heap_size_multiple &&
		ao1.initial_heap_size == ao2.initial_heap_size &&
		ao1.set == ao2.set &&
		ao1.sgc == ao2.sgc &&
		ao1.pss == ao2.pss &&
		ao1.marking_collection == ao2.marking_collection &&
		ao1.disable_rts_flags == ao2.disable_rts_flags &&		
		ao1.o == ao2.o &&
		ao1.fn == ao2.fn &&
		ao1.fs == ao2.fs &&
		ao1.write_stderr_to_file == ao2.write_stderr_to_file &&
		ao1.memoryProfiling == ao2.memoryProfiling &&
		ao1.memoryProfilingMinimumHeapSize == ao2.memoryProfilingMinimumHeapSize &&
		ao1.profiling == ao2.profiling &&
		ao1.stack_traces == ao2.stack_traces &&
		ao1.dynamics == ao2.dynamics &&
		ao1.desc_exl == ao2.desc_exl &&
		ao1.standard_rte == ao2.standard_rte

// do we need to check resource linking flags???
eqLinkOpts :: !LinkOptions !LinkOptions -> Bool
eqLinkOpts lo1 lo2 =
	lo1.method == lo2.method &&
	lo1.generate_relocations == lo2.generate_relocations &&
	lo1.generate_symbol_table == lo2.generate_symbol_table &&
	lo1.generate_link_map == lo2.generate_link_map &&
	lo1.link_resources == lo2.link_resources &&
	(if lo1.link_resources (lo1.resource_source == lo2.resource_source) True) &&
	EQStrings (SortStrings lo1.extraObjectModules) (SortStrings lo2.extraObjectModules) &&
	EQStrings (SortStrings lo1.libraries) (SortStrings lo2.libraries) &&
	lo1.generate_dll == lo2.generate_dll &&
	lo1.dll_export_list_name == lo2.dll_export_list_name

	
PR_SetLinkOptions	:: !Project !LinkOptions -> Project;
PR_SetLinkOptions project linkOptions
	| eqLinkOpts linkOptions project.Project.linkOptions
		=	{Project | project & linkOptions = linkOptions, saved = False};
	| otherwise
		=	{Project | project & linkOptions = linkOptions, exec = False, saved = False};

PR_GetLinkOptions	:: !Project -> LinkOptions;
PR_GetLinkOptions project
	=  project.Project.linkOptions;

PR_SetPaths	:: !Bool !(List String) !(List String) !Project -> Project;
PR_SetPaths def defs new project=:{Project | inflist=Nil} = project;
PR_SetPaths def defs new project=:{Project | inflist=infl=:((root=:{InfListItem | info={dir}}):!rest),prjpaths,saved}
	| def	= {Project | project &
							saved				= saved && olddirs,
							inflist				= inflist1 };
			= {Project | project &
							saved				= saved && unchanged && olddirs,
							inflist				= inflist1,
							prjpaths			= prjpaths1 };
	where 
		(inflist1,olddirs)	= P_MapR SetDcl_and_Icl_and_ABCModified infl;
		unchanged			= EQStrings (SortStrings prjpaths) (SortStrings prjpaths1);
		prjpaths1 	| def	= prjpaths;
					| StringOccurs dir new
							= new;
							= dir:!new;
						
		SetDcl_and_Icl_and_ABCModified :: !InfListItem -> (!InfListItem,!Bool);
		SetDcl_and_Icl_and_ABCModified itm=:{InfListItem | info=minfo=:{dir}}
		| unchanged	= ({itm & src=False}, True);
					= ({itm & info = {minfo & dir=""}, src=False},False);
			where 
				unchanged	= StringOccurs dir defs || StringOccurs dir prjpaths1;
			
		
		
PR_GetCodeGenOptions :: !Project -> CodeGenOptions;
PR_GetCodeGenOptions {codegenopt} =  codegenopt;

//PR_GetProcessor :: !Project -> Processor;
//PR_GetProcessor project=:{codegenopt={tp}} = tp;
	
PR_GetApplicationOptions :: !Project -> ApplicationOptions;
PR_GetApplicationOptions {applicationopt} =  applicationopt;

PR_GetPaths	:: !Project -> List String;
PR_GetPaths {Project | prjpaths} = prjpaths;

PR_GetRootModuleName :: !Project -> String
PR_GetRootModuleName {inflist=Nil}
	= EmptyPathname
PR_GetRootModuleName {inflist={mn}:!rest}
	= mn

PR_GetRootPathName	:: !Project -> (!String,!Project)
PR_GetRootPathName p=:{inflist=Nil}
	= (EmptyPathname,p)
PR_GetRootPathName p=:{inflist={mn,info={dir}}:!rest}
	| size dir==0
		= (EmptyPathname,p)
		= (MakeFullPathname dir (MakeImpPathname mn),p)

PR_GetRootModuleDir	:: !Project -> String
PR_GetRootModuleDir {inflist=Nil}
	= EmptyPathname;
PR_GetRootModuleDir {inflist={mn,info={dir}}:!rest}
	| size dir==0
		= EmptyPathname;
		= dir;

PR_GetRootModuleDirAndName :: !Project -> (!ModuleDirAndName,!Project)
PR_GetRootModuleDirAndName p=:{inflist=Nil}
	= ({mdn_dir=EmptyPathname,mdn_name=""},p)
PR_GetRootModuleDirAndName p=:{inflist={mn,info={dir}}:!rest}
	| size dir==0
		= ({mdn_dir=EmptyPathname,mdn_name=""},p)
		= ({mdn_dir=dir,mdn_name=mn},p)

PR_GetRootDir :: !Project -> String
PR_GetRootDir {root_directory}
	= root_directory;

PR_GetRelativeRootDir :: !Project -> String
PR_GetRelativeRootDir {relative_root_directory}	
	= relative_root_directory

PR_SetRelativeRootDir	:: !String !Project -> Project
PR_SetRelativeRootDir pth prj = {prj & relative_root_directory = pth}

PR_GetModulenames	:: !Bool !Def_and_Imp !Project -> (List String,Project)
PR_GetModulenames full def project=:{inflist}
	= (modnames,project)
where 
	modnames	= MapR GetModulenames inflist
	
	GetModulenames :: !InfListItem -> String
	GetModulenames {mn,info={dir}}
		| full && def	= MakeFullPathname dir (MakeDefPathname mn)
		| full			= MakeFullPathname dir (MakeImpPathname mn)
						= mn

PR_GetDirAndModulenames	:: !Project -> ([!ModuleDirAndName],Project)
PR_GetDirAndModulenames project=:{inflist}
	# modnames = get_dir_and_module_names_r inflist [|]
	= (modnames,project)
where
	get_dir_and_module_names_r ({mn,info={dir}}:!inflist) r
		= get_dir_and_module_names_r inflist [|{mdn_dir=dir,mdn_name=mn}:r]
	get_dir_and_module_names_r Nil r
		= r

PR_GetOpenModulenames	:: !Project -> List String
PR_GetOpenModulenames project=:{inflist}
	= FlattenList modnames
where 
	modnames = MapR GetModulenames inflist

	GetModulenames :: !InfListItem -> List String
	GetModulenames {mn,info={dir,mod_edit_options={defopen,impopen}}}
		| defopen && impopen	= defname :! impname :! Nil
		| defopen				= defname :! Nil
		| impopen				= impname :! Nil
								= Nil
	where
		defname = ModuleDirAndNameToDefPathname {mdn_dir=dir,mdn_name=mn}
		impname = ModuleDirAndNameToImpPathname {mdn_dir=dir,mdn_name=mn}

PR_GetModuleStuff :: !Project -> List (Modulename,String,Modulename,String)
PR_GetModuleStuff project=:{inflist}
	= stuff
where 
	(stuff,_)	= P_MapR GetModulenames inflist
	
	GetModulenames :: !InfListItem -> ((!Modulename,!String,!Modulename,!String),!Bool)
	GetModulenames {mn,info={dir}}
						= ((MakeDefPathname mn,dir,MakeImpPathname mn,dir),True)
		
PR_SrcUpToDate :: !Modulename !Project -> Bool;
PR_SrcUpToDate modname project=:{inflist}
	# item = FindInList modname inflist
	| isNothing item
		= False
	# item = fromJust item
	= item.src

PR_ABCUpToDate	:: !Modulename !Project -> Bool;
PR_ABCUpToDate modname project=:{inflist}
	# item = FindInList modname inflist
	| isNothing item
		= False
	# item = fromJust item
	= item.abc

PR_SysUptoDate :: !Project -> Bool;
PR_SysUptoDate project=:{code_gen_options_unchanged} = code_gen_options_unchanged;
	
PR_ExecUpToDate	:: !Project -> Bool;		
PR_ExecUpToDate project=:{exec} = exec;

PR_Saved :: !Project -> Bool;
PR_Saved {Project | saved} = saved;

PR_AddRootModule ::	!CodeGenOptions !ApplicationOptions !(List String) !LinkOptions
					!Modulename !ModInfo -> Project;
PR_AddRootModule cg ao prjs linkOptions mn info=:{dir}
  = { PR_InitProject
  	& saved			= False
	, exec			= True
	, execpath		= ""
	, code_gen_options_unchanged
						= True
	, inflist			= root:!Nil
	, codegenopt		= cg
	, applicationopt	= ao
	, prjpaths		= prjs
	, linkOptions	= linkOptions
	, staticLibInfo = DefStaticLibInfo
	, target = ""
	};
where 
	root	= {	mn		= mn,info	= info,src		= True,abc		= True };

PR_AddModule :: !Modulename !ModInfo !Project -> Project;
PR_AddModule mn info=:{dir} project=:{inflist=root:!rest}
	= {project & inflist = root:!new:!rest};
where 
	new	= {	mn		= mn,
			info	= info,
			src		= True,
			abc		= True };
PR_AddModule mn info=:{dir} project=:{inflist=Nil}
	= {project & inflist = new:!Nil};
where 
	new	= {	mn		= mn,
			info	= info,
			src		= True,
			abc		= True };
	
PR_GetModuleInfo :: !Modulename !Project -> Maybe ModInfo
PR_GetModuleInfo mn {Project | inflist}
	# item = FindInList mn inflist
	| isNothing item
		= Nothing
	= Just (fromJust item).InfListItem.info
	
PR_UpdateModule :: !Modulename !(ModInfo -> ModInfo) !Project -> Project;
PR_UpdateModule mn update project=:{inflist,saved}
	= {project & inflist = infl`,saved = saved && unchanged};
where 
	(infl`,unchanged)	= UpdateList mn update` inflist;
	update` itm=:{InfListItem | info}	= ({InfListItem | itm & info = info`}, unchanged);
	where 
		info`		= update info;
		unchanged	= eqInfo info info`;

eqInfo :: !ModInfo !ModInfo -> Bool
eqInfo info1=:{mod_edit_options=mod_edit_options1} info2=:{mod_edit_options=mod_edit_options2}
	=	mod_edit_options1.defeo.eo == mod_edit_options2.defeo.eo &&
		mod_edit_options1.impeo.eo == mod_edit_options2.impeo.eo &&
		eqCO info1.compilerOptions info2.compilerOptions &&
		mod_edit_options1.defeo.pos_size == mod_edit_options2.defeo.pos_size && 
		mod_edit_options1.impeo.pos_size == mod_edit_options2.impeo.pos_size &&
		mod_edit_options1.defopen == mod_edit_options2.defopen &&
		mod_edit_options1.impopen == mod_edit_options2.impopen

eqCO :: !CompilerOptions !CompilerOptions -> Bool
eqCO co1 co2
	=	co1.neverTimeProfile == co2.neverTimeProfile &&
		co1.neverMemoryProfile == co2.neverMemoryProfile &&
		co1.sa == co2.sa &&
		co1.CompilerOptions.listTypes == co2.CompilerOptions.listTypes &&
		co1.gw == co2.gw &&
		co1.bv == co2.bv &&
		co1.gc == co2.gc
							
PR_UpdateModules :: ![Modulename] !(ModInfo -> ModInfo) !Project -> Project
PR_UpdateModules mn update project
	= seq [(PR_UpdateModule m update) \\ m <- mn] project	// DvA quick hack, not very efficient!

//	Operations on tables

UpdateList :: !String InfUpdate !InfList -> (!InfList,!Bool)
UpdateList key update list = UpdateList2 key update list Nil
where
	UpdateList2 :: !String InfUpdate !InfList !InfList -> (!InfList,!Bool)
	UpdateList2 key update Nil acc
		=  (Reverse2 acc Nil,True)
	UpdateList2 key update ((first=:{mn,info}):!rest) acc
		| mn <> key
			= UpdateList2 key update rest (first:!acc)
		# (first,changed)	= update first
		= (Reverse2 acc (first :! rest), changed)

FindInList	:: !String !InfList -> Maybe InfListItem
FindInList key Nil									= Nothing
FindInList key ((itm=:{mn,info}):!rest)	| mn <> key	= FindInList key rest
													= Just itm

SetProject :: !{#Char} !{#Char} !ProjectGlobalOptions -> Project
SetProject applicationDir project_file_dir
		{ pg_codegen, pg_application
		, pg_projectPaths, pg_link, pg_mainModuleInfo={name, info}, pg_otherModules
		, pg_target, pg_staticLibInfo, pg_execpath, pg_dynamic
		, pg_root_directory, pg_precompile, pg_postlink
		}
	# project_dir = make_project_dir (size pg_root_directory) project_file_dir
	# paths = ExpandPaths applicationDir project_dir pg_projectPaths
	# linkOptions = ExpandLinkOptionsPaths applicationDir project_dir pg_link
	# project = PR_AddRootModule pg_codegen pg_application paths linkOptions name (ExpandModuleInfoPaths applicationDir project_dir info)
	# project		= addModules pg_otherModules project_dir project
	# staticLibInfo = ExpandStaticLibPaths applicationDir project_dir pg_staticLibInfo
	# project		= PR_SetStaticLibsInfo staticLibInfo project
	# project		= PR_SetTarget pg_target project
	# exepath		= ExpandPath applicationDir project_dir pg_execpath
	# project		= PR_SetExecPath exepath project
	// default of used appopts in exe are ok isn't right :-(
	# pg_postlink	= case pg_postlink of
							Just post_link -> Just (ExpandPath applicationDir project_dir post_link)
							Nothing -> Nothing
	= {project & relative_root_directory = pg_root_directory, root_directory = project_dir, dynamic_info = pg_dynamic,
				 prec = pg_precompile, posl = pg_postlink}
where
	addModules Nil project_dir project
		=	project
	addModules ({name, info} :! t) project_dir project
		=	addModules t project_dir (PR_AddModule name (ExpandModuleInfoPaths applicationDir project_dir info) project)

GetProject :: !{#Char} !Project -> ProjectGlobalOptions
GetProject applicationDir project
	# project_dir = project.root_directory
	  post_link = case project.posl of
					Just post_link -> Just (SubstitutePath applicationDir project_dir post_link)
					Nothing -> Nothing 
	  mainModuleName = PR_GetRootModuleName project
	  target = PR_GetTarget project
	  (otherModuleNames,project`) = PR_GetModulenames False IclMod project
	  mainModuleInfo = getModule project_dir mainModuleName
	  otherModules = Map (getModule project_dir) (Filter ((<>) mainModuleName) otherModuleNames)
	  linkOptions = SubstituteLinkOptionsPaths applicationDir project_dir (PR_GetLinkOptions project)
	  projectPaths = SubstitutePaths applicationDir project_dir (PR_GetPaths project)
	  staticLibInfo = SubstituteStaticLibPaths applicationDir project_dir (PR_GetStaticLibsInfo project)
	=	{ pg_codegen			= PR_GetCodeGenOptions project
		, pg_application		= PR_GetApplicationOptions project
		, pg_projectPaths		= projectPaths
		, pg_link				= linkOptions
		, pg_mainModuleInfo		= mainModuleInfo
		, pg_otherModules		= otherModules
		, pg_staticLibInfo		= staticLibInfo
		, pg_target				= target
		, pg_execpath			= exepath project_dir
		, pg_dynamic			= project.dynamic_info
		, pg_root_directory		= project.relative_root_directory
		, pg_precompile			= project.prec
		, pg_postlink			= post_link
		}
where
	exepath project_dir
		# xp	= PR_GetExecPath project
		= symPath applicationDir project_dir xp

	getModule project_dir name
		# info = PR_GetModuleInfo name project
		# info = if (isJust info) (fromJust info) defaultModInfo
		# info = SubstituteModuleInfoPaths applicationDir project_dir info
		= {name = name, info = info}

	defaultModInfo :: ModInfo
	defaultModInfo	=
		{ dir = EmptyPathname
		, compilerOptions = DefaultCompilerOptions
		, mod_edit_options = {	defeo = defaultEditWdOptions,impeo = defaultEditWdOptions,
								defopen = False,impopen = False}
		, abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} 
		}
	where
		defaultEditWdOptions = {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize}
		DefaultEditOptions =
			{ newlines = HostNativeNewlineConvention}
/*			{ tabs = 4
			, fontname = "Courier New"	//NonProportionalFontDef.fName
			, fontsize = 10				//NonProportionalFontDef.fSize
			, autoi = True
			, newlines = HostNativeNewlineConvention
			, showtabs = False
			, showlins = False
			, showsync = True
			} 
*/

make_project_dir :: !Int !{#Char} -> {#Char}
make_project_dir n_dots project_directory
	| n_dots>1
		# parent_of_project_directory = RemoveFilename project_directory
		| size parent_of_project_directory==size project_directory || size parent_of_project_directory==0
			= project_directory // error, not enough directories
			= make_project_dir (n_dots-1) parent_of_project_directory
		= project_directory

ExpandModuleInfoPaths :: {#Char} {#Char} ModInfo -> ModInfo
ExpandModuleInfoPaths applicationDir projectDir moduleInfo=:{dir}
	= {moduleInfo & dir = ExpandPath applicationDir projectDir dir}

ExpandLinkOptionsPaths :: {#Char} {#Char} LinkOptions -> LinkOptions
ExpandLinkOptionsPaths applicationDir projectDir linkOptions=:{extraObjectModules, libraries}
	=	{ linkOptions
		& extraObjectModules	= ExpandPaths applicationDir projectDir extraObjectModules
		, libraries				= ExpandPaths applicationDir projectDir libraries
		}

ExpandStaticLibPaths :: {#Char} {#Char} StaticLibInfo -> StaticLibInfo
ExpandStaticLibPaths applicationDir projectDir staticLibs=:{sLibs}
	=	{ staticLibs
		& sLibs = ExpandPaths applicationDir projectDir sLibs
		}

//ExpandPaths :: {#Char} {#Char} (List {#Char}) -> List {#Char}
ExpandPaths applicationDir projectDir list
	:== fulPaths applicationDir projectDir list

ExpandPath applicationDir projectDir path
	:== fulPath applicationDir projectDir path

SubstituteModuleInfoPaths :: {#Char} {#Char} ModInfo -> ModInfo
SubstituteModuleInfoPaths applicationDir projectDir info
	=	{info & dir = SubstitutePath applicationDir projectDir info.dir}

SubstituteLinkOptionsPaths :: {#Char} {#Char} LinkOptions -> LinkOptions
SubstituteLinkOptionsPaths applicationDir projectDir linkOptions=:{extraObjectModules, libraries}
	=	{linkOptions
		& extraObjectModules = SubstitutePaths applicationDir projectDir extraObjectModules
		, libraries = SubstitutePaths applicationDir projectDir libraries
		}

SubstituteStaticLibPaths :: {#Char} {#Char} StaticLibInfo -> StaticLibInfo
SubstituteStaticLibPaths applicationDir projectDir staticLibs=:{sLibs}
	=	{ staticLibs
		& sLibs = SubstitutePaths applicationDir projectDir sLibs
		}

//SubstitutePaths :: {#Char} {#Char} (List {#Char}) -> List {#Char}
SubstitutePaths applicationDir projectDir list
	:== symPaths applicationDir projectDir list

SubstitutePath applicationDir projectDir path
	:== symPath applicationDir projectDir path

PR_GetABCLinkInfo	:: !Project -> ABCLinkInfo;
PR_GetABCLinkInfo project=:{inflist}
	#	allLinkInfoRecords	= map (\{InfListItem | info={abcLinkInfo}} -> abcLinkInfo) (StrictListToList inflist);
		oneLinkInfoRecord	= foldl mergeTwoRecords emptyRecord allLinkInfoRecords;
	= oneLinkInfoRecord;
where
		mergeTwoRecords { linkObjFileNames=linkObjFileNames1, linkLibraryNames=linkLibraryNames1}
						{ linkObjFileNames=linkObjFileNames2, linkLibraryNames=linkLibraryNames2}
			= { linkObjFileNames	= UnionStringList linkObjFileNames2 linkObjFileNames1,
				linkLibraryNames	= UnionStringList linkLibraryNames2 linkLibraryNames1};
		emptyRecord
			= { linkObjFileNames = Nil, linkLibraryNames	= Nil};

PR_GetStaticLibsInfo :: !Project -> StaticLibInfo
PR_GetStaticLibsInfo {Project | staticLibInfo} = staticLibInfo

PR_SetStaticLibsInfo :: !StaticLibInfo !Project -> Project
PR_SetStaticLibsInfo staticLibInfo project
	= {Project | project & staticLibInfo = staticLibInfo}

PR_GetTarget :: !Project -> String
PR_GetTarget {Project | target} = target

PR_SetTarget :: !String !Project -> Project
PR_SetTarget target project
	| target == project.Project.target
		= project
		= {Project | project & target = target, exec = False, saved = False}

SL_Add :: !String !StaticLibInfo -> StaticLibInfo
SL_Add pathname sl
	// load sDcls and sDeps info...
	// read some kind of libdef file...
	= {sl & sLibs = Append sl.sLibs pathname}

SL_Rem :: ![String] !String !String !StaticLibInfo -> StaticLibInfo
SL_Rem pathsel ap pp sl
	// remove sDcls and sDeps...
	= {sl & sLibs = seq
		[ RemoveStringFromList (fulPath ap pp s)
		\\ s <- pathsel
		] sl.sLibs}

SL_Libs :: !StaticLibInfo -> List String
SL_Libs sl=:{sLibs} = sLibs

SL_Dcls :: !StaticLibInfo -> List String
SL_Dcls sl=:{sDcls} = sDcls

SL_Deps :: !StaticLibInfo -> List String
SL_Deps sl=:{sDeps} = sDeps

SL_SetLibs :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetLibs lp sl = {sl & sLibs = lp}

SL_SetDcls :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDcls lp sl = {sl & sDcls = lp}

SL_SetDeps :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDeps lp sl = {sl & sDeps = lp}

SaveProjectFile	:: !String !Project !String !*Files -> (!Bool, !*Files);
SaveProjectFile	projectPath project applicationDir files
	= IF_WINDOWS
		(SaveProjectAndPropsFile projectPath project applicationDir files)
		(SaveProjectFileOnly projectPath project applicationDir files)
where
	SaveProjectAndPropsFile	projectPath project applicationDir files
		# (opened, prj_file, files) = fopen projectPath FWriteText files
		| not opened
			=	(False, files)
		# prp_path = RemoveSuffix` projectPath +++ ".prp"
		# (opened, prp_file, files) = fopen prp_path FWriteText files
		| not opened
			# (_,files) = fclose prj_file files
			=	(False, files)
		#! projectGO				= GetProject applicationDir project
		# projectGO = sort_modules projectGO
		# prj_file = WriteOptionsFile ProjectFileVersion (PutOptions project_table projectGO) prj_file
		# prp_file = WriteOptionsFile ProjectFileVersion (PutOptions edit_options_table projectGO) prp_file
		#! (prj_ok, files) = fclose prj_file files
		#! (prp_ok, files) = fclose prp_file files
		= (prj_ok && prp_ok, files)
		
	SaveProjectFileOnly projectPath project applicationDir files
		# (opened, prj_file, files) = fopen projectPath FWriteText files
		| not opened
			=	(False, files)
		#! projectGO				= GetProject applicationDir project
		# projectGO = sort_modules projectGO
		# prj_file = WriteOptionsFile ProjectFileVersion (PutOptions project_table projectGO) prj_file
		#! (prj_ok, files) = fclose prj_file files
		= (prj_ok, files)

save_project_template_file :: !String !Project !String !*Files -> (!Bool, !*Files);
save_project_template_file projectPath project applicationDir files
	# (opened, prt_file, files) = fopen projectPath FWriteText files
	| not opened
		=	(False, files)
	#! projectGO = GetProject applicationDir project
	#! main_module_dir_path = projectGO.pg_mainModuleInfo.ModInfoAndName.info.dir
	# projectGO = {projectGO & pg_projectPaths = [|p \\ p<|-projectGO.pg_projectPaths | p<>main_module_dir_path]}
	# prt_file = WriteOptionsFile ProjectFileVersion (PutOptions project_template_table projectGO) prt_file
	= fclose prt_file files

sort_modules :: !ProjectGlobalOptions -> ProjectGlobalOptions
sort_modules projectGO=:{pg_projectPaths,pg_otherModules}
	# project_paths = {project_path \\ project_path<|-pg_projectPaths}
	# n_project_paths = size project_paths
	# (project_modules,other_modules) = collect_project_modules pg_otherModules (createArray n_project_paths [!!]) project_paths
	# pg_otherModules = append_project_modules 0 project_modules other_modules
	= {projectGO & pg_otherModules=pg_otherModules}
  where
	collect_project_modules :: ![!ModInfoAndName!] !*{![!ModInfoAndName!]} !{#Pathname} -> (!*{![!ModInfoAndName!]},![!ModInfoAndName!])
	collect_project_modules [!mod=:{ModInfoAndName|info={dir}}:mods!] project_modules project_paths
		# path_index = find_path dir 0 project_paths
		| path_index>=0
			# (modules_in_dir,project_modules) = project_modules![path_index]
			# project_modules = {project_modules & [path_index] = [!mod:modules_in_dir!]}
			= collect_project_modules mods project_modules project_paths
			# (project_modules,other_modules) = collect_project_modules mods project_modules project_paths
			= (project_modules,[!mod:other_modules!])
	collect_project_modules [!!] project_modules project_paths
		= (project_modules,[!!])

	find_path :: !{#Char} !Int !{#{#Char}} -> Int
	find_path dir i project_paths
		| i<size project_paths
			| project_paths.[i]==dir
				= i
				= find_path dir (i+1) project_paths
			= -1

	append_project_modules :: !Int !{![!ModInfoAndName!]} [!ModInfoAndName!] -> [!ModInfoAndName!]
	append_project_modules i project_modules other_modules
		| i<size project_modules
			= sort_by_name project_modules.[i] ++| append_project_modules (i+1) project_modules other_modules
			= sort_by_dir_and_name other_modules

	sort_by_name :: !u:[!ModInfoAndName!] -> u:[!ModInfoAndName!]
	sort_by_name l = Hd (msort (pair l)) // mergesort
	  where
		pair [|x1,x2:xs]
			| x2.name < x1.name
				= [[|x2,x1]:pair xs];
				= [[|x1,x2]:pair xs];
		pair x = [|x];

		msort [|x1,x2:xs] = msort (merge_stage [|x1,x2:xs]);
		msort x	= x;

		merge_stage [|xs1,xs2:xxs] = [|merge xs1 xs2 : merge_stage xxs];
		merge_stage x = x;
	
		merge :: !u:[!ModInfoAndName!] !v:[!ModInfoAndName!] -> w:[!ModInfoAndName!], [u v <= w];
		merge [|] y = y
		merge f=:[|x:xs] [|] = f
		merge f=:[|x:xs] s=:[|y:ys]
			| y.name < x.name
				= [|y:merge f ys]
				= [|x:merge xs s]

	sort_by_dir_and_name :: !u:[!ModInfoAndName!] -> u:[!ModInfoAndName!]
	sort_by_dir_and_name l = Hd (msort (pair l)) // mergesort
	  where
		pair [|x1,x2:xs]
			| x2.ModInfoAndName.info.dir == x1.ModInfoAndName.info.dir
				| x2.name < x1.name
					= [[|x2,x1]:pair xs];
					= [[|x1,x2]:pair xs];
				| x2.ModInfoAndName.info.dir < x1.ModInfoAndName.info.dir
					= [[|x2,x1]:pair xs];
					= [[|x1,x2]:pair xs];
		pair x = [|x];

		msort [|x1,x2:xs] = msort (merge_stage [|x1,x2:xs]);
		msort x	= x;

		merge_stage [|xs1,xs2:xxs] = [|merge xs1 xs2 : merge_stage xxs];
		merge_stage x = x;
	
		merge :: !u:[!ModInfoAndName!] !v:[!ModInfoAndName!] -> w:[!ModInfoAndName!], [u v <= w];
		merge [|] y = y
		merge f=:[|x:xs] [|] = f
		merge f=:[|x:xs] s=:[|y:ys]
			| y.ModInfoAndName.info.dir == x.ModInfoAndName.info.dir
				| y.name < x.name
					= [|y:merge f ys]
					= [|x:merge xs s]
				| y.ModInfoAndName.info.dir < x.ModInfoAndName.info.dir
					= [|y:merge f ys]
					= [|x:merge xs s]

ReadProjectFile	:: !String !String !*Files -> (!(!Project, !Bool, !{#Char}),!*Files)
ReadProjectFile projectPath applicationDir files
	#	(opened, file, files)	= fopen projectPath FReadData files
		emptyProject			= PR_InitProject
		projectName				= RemovePath projectPath
		projectDir				= RemoveFilename projectPath
	| not opened
		= ((emptyProject,False,"The file \"" +++  projectName +++ "\" could not be opened."),files)
	#	(version, file)			= ReadVersion file
	| version == ""
		#	(_, files)			= fclose file files
		=	((emptyProject,False,"The file \"" +++  projectName +++ "\" is an old project and could not be opened."),files)
	#!	(options, file)			= ReadOptionsFile file
		empty_projectGO			= GetProject applicationDir emptyProject
		projectGO				= GetOptions ProjectTable options empty_projectGO
		(projectGO,files)
			= add_edit_options_from_prp_file (RemoveSuffix` projectPath +++ ".prp") projectGO empty_projectGO files
		projectGO				= (if (version == "1.3")
									(\p->{p&pg_target="StdEnv"})
									(id)
								) projectGO	// DvA: need to set needs save flag for project;
		unexpanded_exec_path	= projectGO.pg_execpath
		project					= SetProject applicationDir projectDir projectGO
		execpath				= PR_GetExecPath project
		(rootpath,project)		= PR_GetRootPathName project
		project					= PR_SetExecPath (if (size unexpanded_exec_path==0) (MakeExecPathname rootpath) execpath) project
		(closed, files)			= fclose file files
	| not closed
		// generate warning?
		=	((project, True,"The file \"" +++ projectName +++ "\" could not be closed."), files)
	=	((project, True,""), files)

read_project_template_file :: !String !String !*Files -> (!(!Bool, !Project, !{#Char}),!*Files)
read_project_template_file template_file_path applicationDir files
	# (opened, file, files)	= fopen template_file_path FReadData files
	  empty_project = PR_InitProject
	  template_file_name = RemovePath template_file_path
	  template_file_dir = RemoveFilename template_file_path
	| not opened
		= ((False,empty_project,"The file \"" +++  template_file_name +++ "\" could not be opened."),files)
	# (version, file) = ReadVersion file
	| size version==0
		# (_, files) = fclose file files
		= ((False,empty_project,"The file \"" +++  template_file_name +++ "\" is not a project template and could not be opened."),files)
	# (options, file) = ReadOptionsFile file
	  empty_projectGO = GetProject applicationDir empty_project
	  projectGO = GetOptions project_template_table options empty_projectGO
	  project = SetProject applicationDir template_file_dir projectGO
	  (closed, files) = fclose file files
	| not closed
		= ((False,empty_project,"Could not read the file \"" +++ template_file_name +++ "\"."), files)
	= ((True,project,""), files)

add_edit_options_from_prp_file :: !String !ProjectGlobalOptions !ProjectGlobalOptions !*Files -> (!ProjectGlobalOptions, !*Files)
add_edit_options_from_prp_file prp_path projectGO empty_projectGO files
	# (opened, prp_file, files) = fopen prp_path FReadData files
	| not opened
		= (projectGO, files)
	# (version, prp_file) = ReadVersion prp_file
	| size version==0
		= (projectGO, files)
	# (options,prp_file) = ReadOptionsFile prp_file
	#! edit_projectGO = GetOptions ProjectTable options empty_projectGO
	# (closed, files) = fclose prp_file files
	| not closed
		= (projectGO, files)
		# projectGO = add_edit_options edit_projectGO projectGO
		= (projectGO, files)

add_edit_options :: !ProjectGlobalOptions !ProjectGlobalOptions -> ProjectGlobalOptions
add_edit_options {pg_mainModuleInfo,pg_otherModules} projectGO
	#! edit_options = pg_mainModuleInfo.ModInfoAndName.info.mod_edit_options
	# other_modules = add_edit_options_of_other_modules pg_otherModules projectGO.pg_otherModules
	= {projectGO & pg_mainModuleInfo.ModInfoAndName.info.mod_edit_options = edit_options, pg_otherModules = other_modules}
  where
	add_edit_options_of_other_modules (m=:{name,info}:!mods_edit_options) (mod:!mods)
		// the IDE saves the modules in the .prj and .prp files in the same order
		| mod.ModInfoAndName.name==name && mod.ModInfoAndName.info.dir==info.dir
			# mod = {mod & ModInfoAndName.info.mod_edit_options = info.mod_edit_options}
			  mods = add_edit_options_of_other_modules mods_edit_options mods
			= mod:!mods
			// slow version in case the .prj and .prp file were not saved together 
			# mods = add_edit_options_of_other_module m mods
			= add_edit_options_of_other_modules mods_edit_options (mod:!mods)
	add_edit_options_of_other_modules mods_edit_options Nil
		= mods_edit_options
	add_edit_options_of_other_modules Nil mods
		= mods

	add_edit_options_of_other_module m=:{name,info} (mod:!mods)
		| mod.ModInfoAndName.name==name && mod.ModInfoAndName.info.dir==info.dir
			= {mod & ModInfoAndName.info.mod_edit_options = info.mod_edit_options} :! mods
			= mod :! add_edit_options_of_other_module m mods
	add_edit_options_of_other_module m Nil
	 	= m:!Nil

change_root_directory_of_project :: !{#Char} !{#Char} !Project -> Project 
change_root_directory_of_project relative_root_dir root_dir project
	= {project & relative_root_directory=relative_root_dir, root_directory=root_dir}

getDynamicInfo :: !Project -> (ProjectDynamicInfo,Project)
getDynamicInfo prj=:{dynamic_info} = (dynamic_info,prj)

setDynamicInfo :: !.ProjectDynamicInfo !.Project -> .Project
setDynamicInfo inf prj = {prj & dynamic_info = inf}

PR_SetPrecompile	:: !(Maybe String) !Project -> Project
PR_SetPrecompile prec prj = {prj & prec = prec}

PR_GetPrecompile	:: !Project -> (!Maybe String, !Project)
PR_GetPrecompile prj=:{prec} = (prec,prj)

PR_SetPostlink		:: !(Maybe String) !Project -> Project
PR_SetPostlink posl prj = {prj & posl = posl}

PR_GetPostlink		:: !Project -> (!Maybe String, !Project)
PR_GetPostlink prj=:{posl} = (posl,prj)
