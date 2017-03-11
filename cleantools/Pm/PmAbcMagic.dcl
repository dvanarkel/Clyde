definition module PmAbcMagic

import StdFile, StdMaybe
import UtilStrictLists, StdPathname
from UtilDate	import :: DATE
from PmTypes	import :: LinkObjFileName, :: LinkLibraryName, :: Modulename
from Directory	import :: DateTime, :: Date, :: Time
from PmDirCache	import :: DirCache

//	Compiler options that are stored in the abc file
::	ABCOptions		=
	{	abcDescriptors			:: !Bool
	,	abcTimeProfile			:: !Bool
	,	abcStrictnessAnalysis	:: !Bool
	,	abcGiveWarnings			:: !Bool //.
	,	abcExportLocalLabels	:: !Bool
	,	abcGenerateComments		:: !Bool
	,	abcReuseUniqueNodes 	:: !Bool
	,	abcFusion				:: !Bool
	,	abc64Bits				:: !Bool
	,	abcDynamics				:: !Bool
	}

DefaultABCOptions :: ABCOptions

:: *ABCCache
:: ModuleDate :== DateTime

AC_Init :: ABCCache

ParseABCInfoAndDependencies ::
	!Pathname
	!DATE
	!ABCCache
	!Files
	->
	(!((!Bool, !Bool, !Int, !ABCOptions
	),(!List Modulename, !Maybe ModuleDate, !List ModuleDate, !List LinkObjFileName, !List LinkLibraryName
	),!ABCCache),!Files)

//	Returns further info from an .abc file:
GetABCCompiledInfo ::
	!Bool									// require update abcinfo?
	!Pathname								// full path name of the .abc file.
	!ABCCache								// 
	!Files									// Input filesystem
	->
	((!Bool									// is this module a system file?
	, !Bool									// does this .abc file contain sequential stack info?
	, !Int									// version number of compiler which generated the .abc file (-1 if .abc file could not be read).
	, !ABCOptions							// compiler options this .abc file was generated with (except 'Show Type Info').
	, !ABCCache								// 
	),!Files								// The new filesystem
	)

ParseABCDependencies` ::
	!Pathname								// The full .abc path name
	!DATE									// .abc last modified date
	!*ABCCache								// 
	!Files									// Input filesystem
	->
	((!Bool									// Indication whether .abc file could be read
	, !List Modulename						// The list of dependencies
	, !Maybe ModuleDate						// 
	, !List ModuleDate						// 
	, !List LinkObjFileName					// The list of dependant object files
	, !List LinkLibraryName					// The list of dependant (dynamic) library files
	, !*ABCCache							// 
	), !Files								// The new filesystem
	);

PatchSystemABC ::
	!Int									// 
	!Bool									// 
	!Pathname								// 
	!Bool									// 
	!*Files									// Input filesystem
	->
	( !Bool									// 
	, !*Files								// The new filesystem
	)
