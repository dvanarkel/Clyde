definition module PmCleanSystem

from StdFile import ::Files
from UtilStrictLists import ::List
from PmTypes import ::CodeGenOptions,::Processor,::ApplicationOptions,::LinkMethod,::ModuleDirAndName
from PmCompilerOptions import ::CompilerOptions,::ListTypes
from IdeState import ::GeneralSt,instance FileSystem GeneralSt,instance FileEnv GeneralSt
from StdFile import class FileSystem,class FileEnv

import StdOverloaded
import StdPathname
//import PmCallBack

:: CompilerProcessIds

::	CompileOrCheckSyntax	= SyntaxCheck | Compilation
:: StartedCodeGenerator
:: CompilePollCompletedResult = NoFinishedCompiler | UnknownFinishedCompiler | FinishedCompiler !Int !Int
::	CodeGenerateAsmOrCode	= AsmGeneration | CodeGeneration
:: CompilingInfo
::	CompilerMsg
	= 	CompilerOK
	| 	SyntaxError
	| 	Patherror Pathname

::	WindowFun env :== ([String]) -> env -> env

:: ProjectCompilerOptions = {
	pco_memory_profiling :: !Bool,
	pco_time_profiling :: !Bool,
	pco_desc_exl :: !Bool,
	pco_dynamics :: !Bool,
	pco_link_dynamic :: !Bool
   }

instance == CompileOrCheckSyntax
instance == CodeGenerateAsmOrCode

NoCompilerProcessIds :: CompilerProcessIds

standardStaticLibraries :: !Processor !LinkMethod -> List String
standardObjectFiles :: !Bool !Bool !Processor !Bool -> List String
getLibs :: ![String] !*Files -> (!(![String],![String]),!*Files)

CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *env) !(WindowFun *env) !ModuleDirAndName
				!ListTypes !*env -> (!Pathname,!CompilerMsg,!*env) | FileEnv env

InitCompilingInfo :: *CompilingInfo

Compile :: !String !Bool !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !ModuleDirAndName !Pathname
	!(List Pathname) !ProjectCompilerOptions !CompilerOptions !Pathname !CompilerProcessIds !*env
	-> (!Pathname,!CompilerMsg,!CompilerProcessIds,!*env)
	| FileEnv env

CompilePersistent ::
	!String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !ModuleDirAndName
	!(List Pathname) !ProjectCompilerOptions !CompilerOptions !Pathname !*CompilingInfo !*env
	-> (!*CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env

CompileStartCommand ::
	!String !Bool !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Int !ProjectCompilerOptions !CompilerOptions !Pathname !CompilerProcessIds !*env
	-> (!Bool,!CompilerProcessIds,!*env)
	| FileEnv env

ExitCleanCompiler :: !*(!*CompilingInfo,*env) -> *(!*CompilingInfo,*env)

QuitCleanCompiler :: !Bool !CompilerProcessIds !*World -> *World

CodeGen	::	!String !Bool !(WindowFun *GeneralSt) !CodeGenerateAsmOrCode !Pathname !Pathname !Bool
			!CodeGenOptions !Processor !ApplicationOptions !Pathname !CompilerProcessIds !*GeneralSt
			-> (!Pathname,!Bool,!CompilerProcessIds,!*GeneralSt)

start_code_generator ::	!String !(WindowFun *GeneralSt) !Pathname !Int !Bool !CodeGenOptions !Processor !Pathname !*GeneralSt
						-> (!Bool,!Int/*HANDLE*/,!StartedCodeGenerator,!*GeneralSt)

wait_for_finished_code_generator :: !{#Int} !*GeneralSt -> (!Int,!Int,!*GeneralSt)

finish_code_generator :: !Int/*HANDLE*/ !StartedCodeGenerator !Int !(WindowFun *GeneralSt) !*GeneralSt -> (!Bool,!*GeneralSt)

Link ::	!String !(WindowFun *GeneralSt) !Pathname !ApplicationOptions
		!Pathname !(List Pathname) !(List Pathname) !(List Pathname) !Bool !Bool !Bool !Bool !Bool !String
		!Bool !String !Pathname !String !Processor !Bool !*GeneralSt
		 -> (!*GeneralSt,!Bool)

DelayEventLoop :: !.ps -> .ps

CompilePollCompleted :: !CompilerProcessIds !*env -> (!CompilePollCompletedResult, !*env) | FileEnv env

ClearCompilerCache :: .a
ClearCompilerCaches :: .a
SendRepeatResult :: .a
StartCodeGenerator :: .a
Execute` ::	!String !*GeneralSt -> (!Bool,!Int,!*GeneralSt)
