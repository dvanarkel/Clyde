definition module PmFileInfo

import StdFile
import StdPathname
import UtilDate, UtilStrictLists
from PmTypes	import :: Processor, :: Modulename, :: StaticLibInfo, ::ModuleDirAndName
from PmAbcMagic	import :: ABCCache, :: ABCOptions

:: FileInfoCache

:: FileInfo =
	 {	fi_mod_name	:: !{#Char}
	 ,	abcpath		:: !Pathname
	 ,	objpath		:: !Pathname
	 ,	sys			:: !Bool			// system file?
	 ,	seq_stack	:: !Bool			// sequential code & stack info?
	 ,	version		:: !Int
	 ,	abcOptions	:: !ABCOptions
	 ,	abcdate		:: !DATE
	 ,	objdate		:: !DATE
	 }

FI_EmptyCache		:: FileInfoCache
FI_GetFileInfo		:: !Processor !ModuleDirAndName !ABCCache !FileInfoCache !*env -> (!(!FileInfo,!ABCCache,!FileInfoCache),!*env) | FileEnv env
FI_UpdateAbcDate	:: !Modulename !Pathname !Bool !FileInfoCache !*Files -> ((!DATE,!FileInfoCache), !*Files)
FI_UpdateObjDate	:: !Modulename !Pathname !FileInfoCache !*Files -> (!FileInfoCache, !*Files)
FI_UpdateFileInfo	:: !Modulename !(FileInfo -> FileInfo) !FileInfoCache -> FileInfoCache
FI_GetCleanModules	:: !Pathname !StaticLibInfo !FileInfoCache -> (!List Pathname, !FileInfoCache)

YoungestObj			:: !DATE !FileInfoCache -> DATE
