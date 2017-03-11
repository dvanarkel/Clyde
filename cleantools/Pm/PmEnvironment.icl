implementation module PmEnvironment

import StdArray, StdFunc, StdList
import UtilIO
import UtilOptions
import UtilStrictLists
import PmPath
import StdMaybe
import Platform
import PmTypes

EnvsFileName :== "IDEEnvs"

emptyTargets
	= [t_StdEnv20,t_StdIO20]

t_StdEnv :: Target
t_StdEnv
	= t_StdEnv20

:: Target =
	{ target_name	:: !String		// environment identifier
	, target_path	:: !List String	// search paths
	, target_libs	:: !List String	// dynamic libraries
	, target_objs	:: !List String	// object files
	, target_stat	:: !List String	// static libraries
	, target_comp	:: !String		// compiler
	, target_cgen	:: !String		// code generator
	, target_link	:: !String		// static/eager linker
	, target_dynl	:: !String		// dynamic linker
	, target_vers	:: !Int			// abc version
	, env_64_bit_processor :: !Bool
	, target_redc	:: !Bool		// redirect console?
	, target_meth	:: !CompileMethod	// compile strategy
	, target_proc	:: !Processor		// object type
	}

:: CompileMethod
	= CompileSync
	| CompileAsync !Int
	| CompilePers
import Directory, StdBool
//import dodebug
getEnvironments :: !String !String !*env -> *([Target],*env) | FileSystem, FileEnv env
getEnvironments stup envpath env
	# ((ok,path),env)		= pd_StringToPath envpath env
	# ((err,entries),env)	= getDirectoryContents path env
	| err <> NoDirError		= (map (fixAppPaths stup) emptyTargets,env)
	# eentries				= [fileName \\ {fileName} <- entries | size fileName > 4 && fileName%(size fileName - 4,size fileName) == ".env"]
//	# eentries				= [fileName%(size fileName - 4,size fileName) \\ {fileName} <- entries ]//| size fileName > 4 && fileName%(size fileName - 4,size fileName) == ".env"]
	| isEmpty eentries		= openEnvironments stup (MakeFullPathname envpath EnvsFileName) env
	# (ts,env)				= seqSt eentries env
	= case ts of
		[]					-> (map (fixAppPaths stup) emptyTargets,env)
		ts					-> (map (fixAppPaths stup) ts,env)
where
	seqSt [] e = ([],e)
	seqSt [e:es] env
		# ((t,ok,err),env)	= openEnvironment (MakeFullPathname envpath e) env
		# env = case ok of
				True -> env
//				_ -> trace_n` ("Error",err) env
				_ -> env
		# (ts,env)			= seqSt es env
		= (t++ts,env)

openEnvironments :: !String !String !*env -> *([Target],*env) | FileEnv env
openEnvironments stup envpath env
//	# (stup,env)				= accFiles GetFullApplicationPath env
	# ((targets,ok,_),env)		= accFiles (openEnvironment envpath) env
	| not ok
		#	targets				= emptyTargets
		#	(_,env)				= saveEnvironments envpath targets env
		#	targets				= map (fixAppPaths stup) targets
		= (targets,env)
	# targets				= map (fixAppPaths stup) targets
	= (targets,env)

openEnvironment :: !String *a -> *(([Target],.Bool,{#Char}),*a) | FileSystem a
openEnvironment envpath env
	# (opened, file, env)		= fopen envpath FReadData env
	| not opened
		= (([],False,"The file \"" +++  envpath +++ "\" could not be opened."),env)
	# (version, file)			= ReadVersion file
	| version <> EnvFileVersion
		# (_, env)				= fclose file env
		= (([],False,"The file \"" +++  envpath +++ "\" has the wrong version."+++version+++"<<<"),env)
	#! 	(options, file)			= ReadOptionsFile file
		targets					= REO options
		(closed, env)			= fclose file env
	| not closed
		=	((targets, True,"The file \"" +++ envpath +++ "\" clould not be closed."), env)	// warning genereren of zo?
	=	((targets, True,""), env)

saveEnvironments :: !String ![Target] !*env -> *(Bool,*env) | FileEnv env
saveEnvironments envpath targets env
	# (stup,env)	= accFiles GetFullApplicationPath env
	# targets		= map (unfixAppPaths stup) targets
	# (err,env)		= accFiles (saveEnvironments envpath targets) env
	# ok			= isNothing err
	= (ok,env)
where
	saveEnvironments :: !String [.Target] *a -> *(Maybe .[{#Char}],*a) | FileSystem a
	saveEnvironments envpath targets env
		# (opened, file, env)		= fopen envpath FWriteText env
		| not opened
			=	(Just ["Fatal open environments..."],env)
		#! options					= WEO targets
		#! file						= WriteOptionsFile EnvFileVersion options file
		# (closed,env)				= fclose file env
		| not closed
			= (Just ["Fatal close environments..."],env)
		= (Nothing,env)

/* Variant die in dir zoekt naar alle *.env bestanden?
 * Eerst beginnen met targets in leesbare variant weg te schrijven...
 * Rekening houden met exemplaren oude variant...
 */

EnvFileVersion :== "1.0"
emptyTarget =
	{ target_name	= ""
	, target_path	= Nil
	, target_libs	= Nil
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= ""
	, target_cgen	= ""
	, target_link	= ""
	, target_dynl	= ""
	, target_vers	= 42
	, env_64_bit_processor = False
	, target_redc	= False
	, target_meth	= CompileSync
	, target_proc	= DefaultProcessor
	}

WEO prefs
	= PutOptions TargetsTable prefs

REO options
	= GetOptions TargetsTable options []

TargetsTable =
	{ ListOption "Environments" (TargetTableOption) emptyTarget (\a->ListToStrictList a) (\v a->StrictListToList v)
	}

TargetTableOption = GroupedOption "Environment" TargetTable id const

TargetTable :: OptionsTable Target
TargetTable =
	{ SimpleOption	"EnvironmentName"			(\a->a.target_name) (\v a->{a & target_name=v})
	, ListOption	"EnvironmentPaths"			(PathOption) "" (\a-> a.target_path) (\v a->{a & target_path= v})
	, ListOption	"EnvironmentDynamicLibs"	(PathOption) "" (\a-> a.target_libs) (\v a->{a & target_libs= v})
	, ListOption	"EnvironmentObjects"		(PathOption) "" (\a-> a.target_objs) (\v a->{a & target_objs= v})
	, ListOption	"EnvironmentStaticLibs"		(PathOption) "" (\a-> a.target_stat) (\v a->{a & target_stat= v})
	, SimpleOption	"EnvironmentCompiler"		(\a->a.target_comp) (\v a->{a & target_comp=v})
	, SimpleOption	"EnvironmentCodeGen"		(\a->a.target_cgen) (\v a->{a & target_cgen=v})
	, SimpleOption	"EnvironmentLinker"			(\a->a.target_link) (\v a->{a & target_link=v})
	, SimpleOption	"EnvironmentDynLink"		(\a->a.target_dynl) (\v a->{a & target_dynl=v})
	, SimpleOption	"EnvironmentVersion"		(\a->toString a.target_vers) (\v a->{a & target_vers=toInt v})
	, SimpleOption	"EnvironmentRedirect"		(\a->b2s a.target_redc) (\v a->{a & target_redc=string_to_bool v})
	, SimpleOption	"EnvironmentCompileMethod"	(\a->m2s a.target_meth) (\v a->{a & target_meth=s2m v})
	, SimpleOption	"EnvironmentProcessor"		(\a->toString a.target_proc) (\v a->{a & target_proc=fromString v})
	, SimpleOption	"Environment64BitProcessor" (\a->toString a.env_64_bit_processor) (\v a->{a & env_64_bit_processor=string_to_bool v})
	}
where
	b2s True	= "True"
	b2s _		= "False"

	string_to_bool "True"	= True
	string_to_bool  _		= False
	
	m2s CompileSync			= "Sync"
	m2s CompilePers			= "Pers"
	m2s (CompileAsync n)	= toString n
	
	s2m "Sync"	= CompileSync
	s2m "Pers"	= CompilePers
	s2m n		= CompileAsync (toInt n)
	
PathOption = SimpleWithStringConversionOption convert_path_separators "Path" id const

t_StdEnv20 :: Target
t_StdEnv20 =
	{ target_name	= "StdEnv"
	, target_path	=
		( "{Application}\\Libraries\\StdEnv"
		:! Nil
		)
	, target_libs	= PlatformDependant
		( Nil )	// Win
		( Nil )	// Mac
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= "Tools\\Clean System\\CleanCompiler.exe"
	, target_cgen	= "Tools\\Clean System\\CodeGenerator.exe"
	, target_link	= "Tools\\Clean System\\StaticLinker.exe"
	, target_dynl	= "Tools\\Dynamics\\DynamicLinker.exe"
	, target_vers	= 919
	, env_64_bit_processor = False
	, target_redc	= False
	, target_meth	= CompilePers
	, target_proc	= DefaultProcessor
	}

t_StdIO20 =
	{ t_StdEnv20
	& target_name	= "Object IO"
	, target_path	=
		(	"{Application}\\Libraries\\StdEnv"
		:!	"{Application}\\Libraries\\StdLib"
		:!	"{Application}\\Libraries\\ObjectIO ObjectIO"
		:!	"{Application}\\Libraries\\ObjectIO ObjectIO\\OS Windows"
		:!	Nil
		)
	}


//--

fixAppPaths stup target=:{target_path = path, target_libs = libs, target_objs=objs, target_stat=stat}
	= {target & target_path = path`, target_libs = libs`, target_objs=objs`, target_stat = stat`}
where
	path` = fulAppPaths stup path
	libs` = fulAppPaths stup libs
	objs` = fulAppPaths stup objs
	stat` = fulAppPaths stup stat

unfixAppPaths stup target=:{target_path = path, target_libs = libs, target_objs=objs, target_stat=stat}
	= {target & target_path = path`, target_libs = libs`, target_objs=objs`, target_stat=stat`}
where
	path` = symAppPaths stup path
	libs` = symAppPaths stup libs
	objs` = symAppPaths stup objs
	stat` = symAppPaths stup stat
	