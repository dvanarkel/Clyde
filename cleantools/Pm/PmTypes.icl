implementation module PmTypes

// The types for the Project Manager

import StdBool, StdInt
import UtilStrictLists
from UtilNewlinesFile import :: NewlineConvention(..)
import PmCompilerOptions
from StdPathname import :: Pathname
import StdFile
import Platform

::	Modulename			:== String

// Unexpected :: .a {#Char} -> .a
Unexpected message defaultValue
	:== UnexpectedEvalBefore defaultValue (UnexpectedReport message)

UnexpectedEvalBefore :: !.a !.b -> .a
UnexpectedEvalBefore defaultValue _
	=	defaultValue

Don`tCareValue :== 0

UnexpectedReport :: !{#Char} -> Int
UnexpectedReport message
	=	UnexpectedEvalBefore Don`tCareValue (fwrites message stderr)

UnexpectedConstructor :: !{#Char} !{#Char} !.a -> .a
UnexpectedConstructor typeName string defaultValue
	=	Unexpected ( "fromString (" +++ typeName +++ "): unknown value (" +++ string +++ ")\n") defaultValue

//--

::	Processor
	= CurrentProcessor
	| MC68000
	| MC68020
	| MC68020_and_68881
	| PowerPC_Classic
	| PowerPC_Carbon
	| PowerPC_MachO
	| I386

DefaultProcessor :: Processor
DefaultProcessor = PlatformDependant I386 PowerPC_Carbon

PlatformProcessors :: [Processor]
PlatformProcessors = PlatformDependant
	[I386]
	[PowerPC_Classic,PowerPC_Carbon,PowerPC_MachO]

ProcessorSuffix :: !Processor -> String
ProcessorSuffix CurrentProcessor	= ProcessorSuffix DefaultProcessor
ProcessorSuffix MC68000				= ".obj0"
ProcessorSuffix MC68020				= ".obj1"
ProcessorSuffix MC68020_and_68881	= ".obj2"
ProcessorSuffix PowerPC_Classic		= ".cxo"
ProcessorSuffix PowerPC_Carbon		= ".xo"
ProcessorSuffix PowerPC_MachO		= ".o"
ProcessorSuffix I386				= ".o"

instance == Processor
where
	(==) :: !Processor !Processor -> Bool
	(==) CurrentProcessor CurrentProcessor		=	True
	(==) MC68000 MC68000						=	True
	(==) MC68020 MC68020						=	True
	(==) MC68020_and_68881 MC68020_and_68881	=	True
	(==) PowerPC_Classic PowerPC_Classic		=	True
	(==) PowerPC_Carbon PowerPC_Carbon			=	True
	(==) PowerPC_MachO PowerPC_MachO			=	True
	(==) I386 I386								=	True
	(==) _ _									=	False

instance fromString Processor
where
	fromString :: !{#Char} -> Processor
	fromString "CurrentProcessor"		=	CurrentProcessor
	fromString "MC68000"				=	MC68000
	fromString "MC68020"				=	MC68020
	fromString "MC68020_and_68881"		=	MC68020_and_68881
	fromString "PowerPC_Classic"		=	PowerPC_Classic
	fromString "PowerPC_Carbon"			=	PowerPC_Carbon
	fromString "PowerPC_MachO"			=	PowerPC_MachO
	fromString "I386"					=	I386
	fromString string					=	UnexpectedConstructor "Processor" string CurrentProcessor

instance toString Processor
where
	toString :: !Processor -> {#Char}
	toString MC68000				=	"MC68000"
	toString MC68020				=	"MC68020"
	toString MC68020_and_68881		=	"MC68020_and_68881"
	toString PowerPC_Classic		=	"PowerPC_Classic"
	toString PowerPC_Carbon			=	"PowerPC_Carbon"
	toString PowerPC_MachO			=	"PowerPC_MachO"
	toString I386					=	"I386"
	toString CurrentProcessor		=	"CurrentProcessor"

:: ModInfoAndName =
	{
		info	:: ModInfo,
		name	:: {#Char}
	}

::	ModInfo	=
	{ dir				:: !String				// !Pathname			// directory
	, compilerOptions	:: !CompilerOptions		// compiler options
	, mod_edit_options	:: !ModEditOptions
	, abcLinkInfo		:: !ABCLinkInfo
	}

::	ModEditOptions = {
		defeo 		:: !EditWdOptions,		// definition module edit options
		impeo		:: !EditWdOptions,		// implementation module edit options
		defopen 	:: !Bool,				// definition module is open
		impopen 	:: !Bool				// implementation module is open
	}

:: ABCLinkInfo = {linkObjFileNames :: !List LinkObjFileName, linkLibraryNames :: !List LinkLibraryName}

:: LinkObjFileName	:== String
:: LinkLibraryName	:== String

//-- Link Options

::	LinkOptions =
	{ extraObjectModules	:: !List {#Char}
	, libraries				:: !List {#Char}
	, method				:: !LinkMethod
	, generate_relocations	:: !Bool			// Win only option
	, generate_symbol_table	:: !Bool
	, generate_link_map		:: !Bool
	, link_resources		:: !Bool
	, resource_source		:: !String

	, generate_dll						:: !Bool
	, dll_export_list_name				:: !String
	
//	, add_carb_resource		:: !Bool			// Mac-only!
	}
/*
instance == LinkOptions		// do we need to check resource linking flags???
where
	(==) :: !LinkOptions !LinkOptions -> Bool
	(==) lo1 lo2 =

		lo1.method == lo2.method &&
		lo1.generate_relocations == lo2.generate_relocations &&
		lo1.generate_link_map == lo2.generate_link_map &&
		lo1.link_resources == lo2.link_resources &&
		(if lo1.link_resources (lo1.resource_source == lo2.resource_source) True) &&
		EQStrings (SortStrings lo1.extraObjectModules) (SortStrings lo2.extraObjectModules) &&
		EQStrings (SortStrings lo1.libraries) (SortStrings lo2.libraries) &&
		lo1.add_carb_resource == lo2.add_carb_resource
*/
DefaultLinkOptions :: LinkOptions;
DefaultLinkOptions =
	{ extraObjectModules		= Nil
	, libraries					= Nil
	, method					= LM_Static
	, generate_relocations		= False
	, generate_symbol_table		= False
	, generate_link_map			= False
	, link_resources			= False
	, resource_source			= ""

	, generate_dll						= False
	, dll_export_list_name				= ""
//	, add_carb_resource					= False	// Clean2 ide targets Carbon?!
	}

:: LinkMethod
	= LM_Static
//	| LM_Eager
	| LM_Dynamic
	| LM_StaticLibrary		// produce static library
	| LM_DynamicLibrary		// produce shared library

instance toString LinkMethod
where
	toString LM_Static	= "Static"
//	toString LM_Eager	= "Eager"
	toString LM_Dynamic	= "Dynamic"
	toString LM_StaticLibrary	= "StaticLibrary"
	toString LM_DynamicLibrary	= "DynamicLibrary"
	
instance fromString LinkMethod
where
	fromString "Static"			= LM_Static
	fromString "Eager"			= LM_Dynamic
	fromString "Dynamic"		= LM_Dynamic
	fromString "StaticLibrary"	= LM_StaticLibrary
	fromString "DynamicLibrary"	= LM_DynamicLibrary
/*
	fromString "Eager"			= LM_Eager
	fromString "Dynamic"		= LM_Dynamic
*/
	fromString _				= LM_Static

instance == LinkMethod
where
	(==) :: !LinkMethod !LinkMethod -> Bool
	(==) LM_Static			LM_Static			= True
//	(==) LM_Eager			LM_Eager			= True
	(==) LM_Dynamic			LM_Dynamic			= True
	(==) LM_StaticLibrary	LM_StaticLibrary	= True
	(==) LM_DynamicLibrary	LM_DynamicLibrary	= True
	(==) _					_					= False

//	Window position and size

::	WindowPos_and_Size	=
	{	posx	:: !Int
	,	posy	:: !Int
	,	sizex	:: !Int
	,	sizey	:: !Int
	}

instance == OptionalWindowPosAndSize
where
	(==) (WindowPosAndSize pos1) (WindowPosAndSize pos2)
		= pos1==pos2
	(==) NoWindowPosAndSize NoWindowPosAndSize
		= True
	(==) _ _
		= False

instance == WindowPos_and_Size
where
	(==) :: !WindowPos_and_Size !WindowPos_and_Size -> Bool
	(==) pos1 pos2
		=	pos1.posx == pos2.posx &&
			pos1.posy == pos2.posy &&
			pos1.sizex == pos2.sizex &&
			pos1.sizey == pos2.sizey

DefWindowPos_and_Size :: WindowPos_and_Size
DefWindowPos_and_Size =
	{ posx=0
	, posy=0
	, sizex=500
	, sizey=300
	}


//	The Editor Options: default settings for the EditWindows.

::	EditOptions	=
	{/*	tabs		:: !Int
	,	fontname	:: !String				// !FontName
	,	fontsize	:: !Int					// !FontSize
	,	autoi		:: !Bool
	,*/	newlines	:: !NewlineConvention
/*	,	showtabs	:: !Bool
	,	showlins	:: !Bool
	,	showsync	:: !Bool
*/	}
	// (tabs,font&size,auto indent)

instance == EditOptions
where
	(==) :: !EditOptions !EditOptions -> Bool
	(==) eo1 eo2
		= True
/*		=	eo1.tabs == eo2.tabs &&
			eo1.EditOptions.fontname == eo2.EditOptions.fontname &&
			eo1.EditOptions.fontsize == eo2.EditOptions.fontsize &&
			eo1.EditOptions.autoi == eo2.EditOptions.autoi
*/

//	The Window parameters: edit options and window position and size
::	EditWdOptions	=
	{	eo			:: !EditOptions
	,	pos_size	:: !OptionalWindowPosAndSize 
	}
	// edit options, window pos&size
	
//	The Code Generator Options: default settings for the code generator
::	CodeGenOptions =
	{ cs	:: !Bool
	, ci	:: !Bool
//	, kaf	:: !Bool
//	, tp	:: !Processor
	}

instance == CodeGenOptions
where
	(==) :: !CodeGenOptions !CodeGenOptions -> Bool
	(==) cg1 cg2
		=	cg1.cs == cg2.cs &&
			cg1.ci == cg2.ci

DefCodeGenOptions :: CodeGenOptions;
DefCodeGenOptions =
	{	cs		= False
	,	ci		= True
	}

//	The Application Options: default settings for the application.
::	ApplicationOptions	=
	{	hs								:: !Int
	,	ss								:: !Int
	,	em								:: !Int
	,	heap_size_multiple				:: !Int
	,	initial_heap_size				:: !Int
	,	set								:: !Bool
	,	sgc								:: !Bool
	,	pss								:: !Bool
	,	marking_collection				:: !Bool
	,	disable_rts_flags				:: !Bool
	,	o								:: !Output
	,	fn								:: !String		// !FontName
	,	fs								:: !Int			// !FontSize
	,	write_stderr_to_file			:: !Bool
	,	memoryProfiling 				:: !Bool
	,	memoryProfilingMinimumHeapSize	:: !Int
	,	profiling 						:: !Bool
	,	stack_traces					:: !Bool
	,	dynamics						:: !Bool
	,	desc_exl							:: !Bool
	,	standard_rte					:: !Bool		// DvA: use standard RTE (only in IDE)
	}

DefApplicationOptions :: ApplicationOptions;
DefApplicationOptions =
	{	hs	= 2<<20
	,	ss	= 500<<10
	,	em	= 8<<10
	,	heap_size_multiple = 16<<8
	,	initial_heap_size = 200<<10
	,	set	= False
	,	sgc	= False
	,	pss	= False
	,	marking_collection = False
	,   disable_rts_flags = False
	,	o	= ShowConstructors
	,	fn	= "Monaco"				//=> platform dependant?
	,	fs	= 9						//=> platform dependant?
	,	write_stderr_to_file
						= False
	,	memoryProfiling = False
	,	memoryProfilingMinimumHeapSize = 0
	,	profiling = False
	,	stack_traces = False
	,	dynamics = False
	,	desc_exl = False
	,	standard_rte = True
	}
	
::	Output = BasicValuesOnly | ShowConstructors | NoReturnType | NoConsole

instance == Output
where
	(==) :: !Output !Output -> Bool
	(==) BasicValuesOnly BasicValuesOnly
		=	True
	(==) ShowConstructors ShowConstructors
		=	True
	(==) NoReturnType NoReturnType
		=	True
	(==) NoConsole NoConsole
		=	True
	(==) _ _
		=	False;

instance fromString Output
where
	fromString :: !{#Char} -> Output
	fromString "BasicValuesOnly"
		=	BasicValuesOnly
	fromString "ShowConstructors"
		=	ShowConstructors
	fromString "NoReturnType"
		=	NoReturnType
	fromString "NoConsole"
		=	NoConsole
	fromString string
		=	UnexpectedConstructor "Output" string BasicValuesOnly

instance toString Output
where
	toString :: !Output -> {#Char}
	toString BasicValuesOnly
		=	"BasicValuesOnly"
	toString ShowConstructors
		=	"ShowConstructors"
	toString NoReturnType
		=	"NoReturnType"
	toString NoConsole
		=	"NoConsole"

:: StaticLibInfo =
	{ sLibs :: !List Pathname
	, sDcls :: !List Modulename
	, sDeps :: !List Modulename
	}

isProjLibraryModule :: !.String !StaticLibInfo -> Bool
isProjLibraryModule mod info=:{sDcls}
	= StringOccurs mod sDcls

