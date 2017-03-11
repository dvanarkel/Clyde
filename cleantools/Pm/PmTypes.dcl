definition module PmTypes

// The types for the Project Manager

from	StdPathname			import :: Pathname
from	UtilNewlinesFile	import :: NewlineConvention(..)
import	PmCompilerOptions
from	UtilStrictLists		import :: List
import	UtilDate

::	Modulename			:== String

:: ModuleDirAndName = {mdn_dir :: !{#Char}, mdn_name :: !{#Char}}

::	Processor //= CurrentProcessor | MC68000 | MC68020 | MC68020_and_68881
DefaultProcessor :: Processor
PlatformProcessors :: [Processor]

ProcessorSuffix :: !Processor -> String
instance == Processor
instance toString Processor
instance fromString Processor

:: LinkObjFileName	:== String
:: LinkLibraryName	:== String

::	LinkOptions =
	{ extraObjectModules	:: !List {#Char}
	, libraries				:: !List {#Char}
	, method				:: !LinkMethod
	, generate_relocations	:: !Bool			// Win only option?
	, generate_symbol_table	:: !Bool
	, generate_link_map		:: !Bool
	, link_resources		:: !Bool
	, resource_source		:: !String

	, generate_dll			:: !Bool
	, dll_export_list_name	:: !String
	
//	, add_carb_resource		:: !Bool			// Mac-only!
	}

DefaultLinkOptions		:: LinkOptions

:: LinkMethod				// => is really project method/type now?
	= LM_Static
//	| LM_Eager
	| LM_Dynamic
	| LM_StaticLibrary		// produce static library
	| LM_DynamicLibrary		// produce shared library

instance toString LinkMethod
instance fromString LinkMethod
instance == LinkMethod

//	Window position and size

::	WindowPos_and_Size	=
	{ posx	:: !Int
	, posy	:: !Int
	, sizex	:: !Int
	, sizey	:: !Int
	}

:: OptionalWindowPosAndSize = NoWindowPosAndSize | WindowPosAndSize !WindowPos_and_Size;

//instance == WindowPos_and_Size
instance == OptionalWindowPosAndSize

DefWindowPos_and_Size	:: WindowPos_and_Size

::	EditOptions	=
	{ newlines	:: !NewlineConvention	// newline convention
/*	
	{ tabs		:: !Int					// tab size
	, fontname	:: !String				// !FontName
	, fontsize	:: !Int					// !FontSize
	, autoi		:: !Bool				// auto-indent
	, newlines	:: !NewlineConvention	// newline convention
	, showtabs	:: !Bool
	, showlins	:: !Bool
	, showsync	:: !Bool
*/
	}

instance == EditOptions

//	The Edit Window parameters: edit options and window position and size

::	EditWdOptions	=
	{ eo			:: !EditOptions
	, pos_size		:: !OptionalWindowPosAndSize 
	}

//	The Code Generator Options: default settings for the code generator

::	CodeGenOptions	=
	{ cs	:: !Bool			// generate stack checks
	, ci	:: !Bool			// generate index checks
//	, kaf	:: !Bool			// keep abc-files
//	, tp	:: !Processor
	}

instance == CodeGenOptions

DefCodeGenOptions		:: CodeGenOptions

//	The Application Options: default settings for the application.

::	ApplicationOptions	=
	{ hs								:: !Int			// heap size
	, ss								:: !Int			// stack size
	, em								:: !Int			// extra memory
	, heap_size_multiple				:: !Int
	, initial_heap_size					:: !Int
	, set								:: !Bool		// show execution time
	, sgc								:: !Bool		// show garbage collections
	, pss								:: !Bool		// print stack size
	, marking_collection				:: !Bool		// use marking garbage collector
	, disable_rts_flags					:: !Bool		// disable checking command line arguments for RTS flags

	, o									:: !Output		// console type
	, fn								:: !String		// font name: only on Mac platform
	, fs								:: !Int			// font size: only on Mac platform
	, write_stderr_to_file				:: !Bool

	, memoryProfiling					:: !Bool
	, memoryProfilingMinimumHeapSize	:: !Int
	, profiling							:: !Bool		// time profiling
	, stack_traces						:: !Bool		// stack traces
	, dynamics							:: !Bool
	, desc_exl							:: !Bool
	, standard_rte						:: !Bool		// DvA: use standard RTE (only in IDE)
	}

DefApplicationOptions	:: ApplicationOptions

::	Output
	= BasicValuesOnly
	| ShowConstructors
	| NoReturnType
	| NoConsole

instance == Output
instance toString Output
instance fromString Output

:: ModInfoAndName =
	{ info	:: ModInfo
	, name	:: {#Char}
	}

::	ModInfo =
	{ dir				:: !String				// !Pathname
	, compilerOptions	:: !CompilerOptions
	, mod_edit_options	:: !ModEditOptions
	, abcLinkInfo		:: !ABCLinkInfo			// found dependant libs and objs
	}

::	ModEditOptions = {
		defeo 		:: !EditWdOptions,		// definition module edit options
		impeo		:: !EditWdOptions,		// implementation module edit options
		defopen 	:: !Bool,				// definition module is open
		impopen 	:: !Bool				// implementation module is open
	}

:: ABCLinkInfo =
	{ linkObjFileNames :: !List LinkObjFileName
	, linkLibraryNames :: !List LinkLibraryName
	}

:: StaticLibInfo =
	{ sLibs :: !List Pathname
	, sDcls :: !List Modulename
	, sDeps :: !List Modulename
	}

isProjLibraryModule :: !.String !StaticLibInfo -> Bool

