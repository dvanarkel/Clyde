implementation module PmFileInfo

import StdBool
from UtilIO import FModified
from UtilDate import Older_Date
import UtilStrictLists
import PmPath, PmCompilerOptions, PmTypes
import PmAbcMagic

/*
	? switch to array for cache instead of List
	? use dircache for certain file mod datetime lookups
*/

//--- FileInfo stuff

:: FileInfo =
	 {	fi_mod_name	:: !{#Char}
	 ,	abcpath		:: !Pathname
	 ,	objpath		:: !Pathname
	 ,	sys			:: !Bool			// system file?
	 ,	seq_stack	:: !Bool			// sequential code & stack info?
	 ,	version		:: !Int				// abc - version
	 ,	abcOptions	:: !ABCOptions
	 ,	abcdate		:: !DATE
	 ,	objdate		:: !DATE
	 }

:: FileInfoCache :== List FileInfo

FI_EmptyCache :: FileInfoCache
FI_EmptyCache = Nil

FI_GetFileInfo :: !Processor !ModuleDirAndName !ABCCache !FileInfoCache !*env -> (!(!FileInfo,!ABCCache,!FileInfoCache),!*env) | FileEnv env
FI_GetFileInfo tp mdn abccache fileinfo ps
	= accFiles (GetFileInfo1 tp mdn abccache fileinfo fileinfo) ps
where
	GetFileInfo1 ::	!Processor !ModuleDirAndName !ABCCache !FileInfoCache !FileInfoCache !Files
					-> (!(!FileInfo,!ABCCache,!FileInfoCache), Files)
	GetFileInfo1 tp mdn=:{mdn_name} abccache (fileinfo:!rest) acc files
		| mdn_name==fileinfo.fi_mod_name
			= ((fileinfo,abccache,acc), files)
			= GetFileInfo1 tp mdn abccache rest acc files
	GetFileInfo1 tp mdn=:{mdn_name} abccache Nil acc files
		#	abcpath				= ModuleDirAndNameToABCSystemPathname mdn
		#	objpath				= ModuleDirAndNameToObjSystemPathname tp mdn
		#	(abcdate, files)	= FModified abcpath files
		#	(objdate, files)	= FModified objpath files
		| not abcdate.exists
			#	finfo	= { fi_mod_name = mdn_name,
							abcpath		= abcpath,	objpath		= objpath,
							abcdate		= abcdate,	objdate		= objdate,
							sys			= False,	seq_stack	= False,
							version		= -1,		abcOptions	= DefaultABCOptions }
			= ((finfo,abccache,finfo:!acc), files)
		// otherwise
			#	((sys,seq_stack,version,abcOptions,abccache),files)
									= GetABCCompiledInfo False abcpath abccache files
				finfo	= { fi_mod_name = mdn_name,
							abcpath		= abcpath,	objpath		= objpath,
							abcdate		= abcdate,	objdate		= objdate,
							sys			= sys,		seq_stack	= seq_stack,
							version		= version,	abcOptions	= abcOptions }
			= ((finfo,abccache,finfo:!acc), files)

FI_UpdateFileInfo :: !Modulename !(FileInfo -> FileInfo) !FileInfoCache -> FileInfoCache
FI_UpdateFileInfo module_name update list = UpdateFileInfo1 module_name update list Nil
where
	UpdateFileInfo1 :: !Pathname !(FileInfo -> FileInfo) !FileInfoCache !FileInfoCache -> FileInfoCache
	UpdateFileInfo1 module_name update_function Nil acc
		= acc
	UpdateFileInfo1 module_name update_function ((first=:{fi_mod_name}):!rest) acc
		| fi_mod_name == module_name
			= Reverse2 (update_function first:!rest) acc
			= UpdateFileInfo1 module_name update_function rest (first:!acc)

FI_UpdateAbcDate :: !Modulename !Pathname !Bool !FileInfoCache !*Files -> ((!DATE,!FileInfoCache), !*Files)
FI_UpdateAbcDate module_name abcPath abcTimeProfile fileInfo files
	# (abcDate, files) = FModified abcPath files
	# update = \info -> {info & abcpath=abcPath, abcdate=abcDate, abcOptions.abcTimeProfile = abcTimeProfile}
	  fileInfo = FI_UpdateFileInfo module_name update fileInfo
	= ((abcDate,fileInfo), files)

FI_UpdateObjDate :: !Modulename !Pathname !FileInfoCache !*Files -> (!FileInfoCache, !*Files)
FI_UpdateObjDate module_name objPath fileInfo files
	# (objDate, files) = FModified objPath files
	  update = \finfo -> {finfo & objpath = objPath, objdate = objDate}
	  fileInfo = FI_UpdateFileInfo module_name update fileInfo
	= (fileInfo,files)

FI_GetCleanModules :: !Pathname !StaticLibInfo !FileInfoCache -> (!List Pathname, !FileInfoCache)
FI_GetCleanModules system_obj_path libsinfo fileinfo
	# clmodpaths				= Map (\{objpath}->objpath) fileinfo
	# clmodpaths				= Filter notSystemObject clmodpaths
	# clmodpaths				= Filter notLibraryObject clmodpaths
	# clmodpaths				= Reverse clmodpaths
	= (clmodpaths,fileinfo)
where
	notSystemObject objpath = objpath <> system_obj_path
	notLibraryObject objpath = not (isProjLibraryModule (GetModuleName objpath) libsinfo)

//	Finds the most recently modified .obj file in FileInfo...
YoungestObj :: !DATE !FileInfoCache -> DATE
YoungestObj youngest Nil = youngest
YoungestObj youngest ({objdate}:!rest)
	| not youngest.exists
		= YoungestObj objdate rest
	| not objdate.exists
		= YoungestObj youngest rest
	| Older_Date youngest objdate
		= YoungestObj objdate rest
	// otherwise
		= YoungestObj youngest rest
