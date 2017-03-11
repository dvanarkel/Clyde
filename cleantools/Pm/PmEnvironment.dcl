definition module PmEnvironment

import StdFile,StdString,StdMaybe
import UtilStrictLists
from PmTypes import ::Processor

EnvsFileName :== "IDEEnvs"

:: Target =
	{ target_name	:: !String			// environment identifier
	, target_path	:: !List String		// search paths
	, target_libs	:: !List String		// dynamic libraries
	, target_objs	:: !List String		// object files
	, target_stat	:: !List String		// static libraries
	, target_comp	:: !String			// compiler
	, target_cgen	:: !String			// code generator
	, target_link	:: !String			// static/eager linker
	, target_dynl	:: !String			// dynamic linker
	, target_vers	:: !Int				// abc version
	, env_64_bit_processor :: !Bool
	, target_redc	:: !Bool			// redirect console?
	, target_meth	:: !CompileMethod	// compile strategy
	, target_proc	:: !Processor		// object type
	}

:: CompileMethod
	= CompileSync
	| CompileAsync !Int
	| CompilePers
	
getEnvironments :: !String !String !*env -> *([Target],*env) | FileSystem, FileEnv env
openEnvironments	:: !String !String !*env -> *([Target],*env) | FileEnv env
saveEnvironments	:: !String ![Target] !*env -> *(Bool,*env) | FileEnv env

t_StdEnv :: Target
