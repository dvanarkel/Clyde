definition module IdeState

import StdPathname
import UtilStrictLists
from PmAbcMagic import :: ABCCache
from PmProject import :: Project
import PmCompilerOptions
import typewin
import PmEnvironment
from PmFileInfo import :: FileInfoCache
from PmCleanSystem import ::CompilerProcessIds,NoCompilerProcessIds

:: *General

:: *GeneralSt = {
		ls :: !General,
		gst_world :: !*World,
		gst_continue_or_stop :: !Bool
	}

app_world_instead_of_ps f gst :== {gst & gst_world=f gst.gst_world}

instance FileSystem GeneralSt
instance FileEnv GeneralSt

initGeneral :: !Bool !CompilerOptions !String !String !Project ![Target] !*File -> *General

:: Prefs =
	{ be_verbose			:: !Bool
	, compopts				:: !CompilerOptions
	, edwintabs				:: !(Int,Bool,Bool,Bool,Bool)
	, number_of_processes	:: !Int
	}

:: ErrPrefs
:: SrcPrefs
:: NewlinePrefs

getPrefs :: !*GeneralSt -> (Prefs,*GeneralSt)
setPrefs :: Prefs !*GeneralSt -> *GeneralSt

getProject :: !*GeneralSt -> (Project,*GeneralSt)
setProject :: !Project !*GeneralSt -> *GeneralSt

getABCCache :: !*GeneralSt -> *(!*ABCCache,!*GeneralSt)
setABCCache :: !*ABCCache !*GeneralSt -> *GeneralSt

getFICache :: !*GeneralSt -> (FileInfoCache,*GeneralSt)
setFICache :: !FileInfoCache !*GeneralSt -> *GeneralSt

getProjectFilePath :: !*GeneralSt -> (!Pathname,!*GeneralSt)
setProjectFilePath :: !Pathname !*GeneralSt -> *GeneralSt

getStup :: !*GeneralSt -> (!Pathname,!*GeneralSt)

getTargets :: !*GeneralSt -> (![Target],!*GeneralSt)
setTargets :: ![Target] !*GeneralSt -> *GeneralSt
getCurrentTarget :: !*GeneralSt -> (!Int,!*GeneralSt)
setCurrentTarget :: !Int !*GeneralSt -> *GeneralSt
getCurrentPaths :: !*GeneralSt -> (!(List Pathname),!*GeneralSt)
getCurrentDlibs :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentSlibs :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentObjts :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentComp :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentCgen :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentLink :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentDynl :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentVers :: !*GeneralSt -> (!Int,!*GeneralSt)
getCurrent64BitProcessor :: !*GeneralSt -> (!Bool,!*GeneralSt)
getCurrentProc :: !*GeneralSt -> (!Processor,!*GeneralSt)
getCurrentMeth :: !*GeneralSt -> (!CompileMethod,!*GeneralSt)

getCompilerProcessIds :: !*GeneralSt -> (!CompilerProcessIds,!*GeneralSt)
setCompilerProcessIds :: !CompilerProcessIds !*GeneralSt -> *GeneralSt

//-- boolean that indicates if user interaction is allowed

getInteract  :: !*GeneralSt -> (!Bool,!*GeneralSt)

//-- log functions for batch build

writeLog :: !String !*GeneralSt -> *GeneralSt
abortLog :: !Bool !String !*GeneralSt -> *GeneralSt
