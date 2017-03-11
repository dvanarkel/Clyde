implementation module IdeState

import StdMisc, StdList
import StdPathname
import UtilStrictLists
from PmAbcMagic import :: ABCCache, AC_Init
from PmProject import :: Project, PR_GetTarget
import PmCompilerOptions
import typewin
import PmEnvironment
import logfile
import set_return_code
import PmFileInfo
from PmCleanSystem import ::CompilerProcessIds,NoCompilerProcessIds

appPLoc f gst
	# s = f gst.ls
	:== {gst & ls=s}

accPLoc f gst
	# (r,s) = f gst.ls
	:== (r,{gst & ls=s})

instance FileSystem GeneralSt where
	fopen fName fMode pState
		# (b,f,w) = fopen fName fMode pState.gst_world
		# pState = {pState & gst_world=w}
		= (b,f,pState)
	fclose file pState
		# (b,w) = fclose file pState.gst_world
		# pState = {pState & gst_world=w}
		= (b,pState)
	stdio pState
		# (f,w) = stdio pState.gst_world
		# pState = {pState & gst_world=w}
		= (f,pState)
	sfopen fName fMode pState
		# (b,f,w) = sfopen fName fMode pState.gst_world
		# pState = {pState & gst_world=w}
		= (b,f,pState)

instance FileEnv GeneralSt where
	accFiles accfun io
		# (x,w) = accFiles accfun io.gst_world
		= (x,{io & gst_world=w})
	appFiles appfun io
		= {io & gst_world = appFiles appfun io.gst_world}

:: *General =
	{ prefs			:: !Prefs
	, project		:: !Project
	, cache			:: !*(Maybe *ABCCache)
	, fi_cache		:: !(Maybe FileInfoCache)
	, pr_path		:: !Pathname			// proj_path
	, stup			:: !Pathname			// appl_path
	, g_compiler_process_ids :: !CompilerProcessIds
	, pm_targets	:: ![Target]
	, pm_curtarg	:: !Int
	, logfile		:: !*File
	}

initGeneral :: !Bool !CompilerOptions !String !String !Project ![Target] !*File -> *General
initGeneral be_verb comp_opts application_path project_path project targets logfile
	| isNothing target_index	= abort ("Unable to find project environment for target '" +++ target_name +++ "' in available environments.\n")
	=
	{ prefs			= prefs
	, project		= project
	, cache			= Just AC_Init
	, fi_cache		= Just FI_EmptyCache
	, pr_path		= project_path
	, stup			= application_path
	, g_compiler_process_ids=NoCompilerProcessIds
	, pm_targets	= targets
	, pm_curtarg	= fromJust target_index
	, logfile		= logfile
	}
where
	prefs =
		{ be_verbose			= be_verb
		, compopts				= comp_opts
		, edwintabs				= (4,True,False,True,True)
		, number_of_processes	= 1
		}
	target_name	= PR_GetTarget project
	target_index = findIndex 0 target_name targets

	findIndex x name [] = Nothing
	findIndex x name [t=:{target_name=n}:ns]
		| n == name = Just x
		= findIndex (inc x) name ns

:: Prefs =
	{ be_verbose			:: !Bool
	, compopts				:: !CompilerOptions
	, edwintabs				:: !(Int,Bool,Bool,Bool,Bool)	// tabsize, autotab, showtabs, showlinenos, showsyncol
	, number_of_processes	:: !Int
	}

:: ErrPrefs		= ErrPrefs
:: SrcPrefs		= SrcPrefs
:: NewlinePrefs	= NwlPrefs

getPrefs :: !*GeneralSt -> (Prefs,*GeneralSt)
getPrefs ps = ps!ls.prefs

setPrefs :: Prefs !*GeneralSt -> *GeneralSt
setPrefs prefs ps = {ps & ls.prefs = prefs}

getProject :: !*GeneralSt -> (Project,*GeneralSt)
getProject ps = ps!ls.project

setProject :: !Project !*GeneralSt -> *GeneralSt
setProject project ps = {ps & ls.project = project}

getABCCache :: !*GeneralSt -> *(!*ABCCache,!*GeneralSt)
getABCCache ps = accPLoc (\p=:{cache = Just cache}->(cache,{p & cache = Nothing})) ps

setABCCache :: !*ABCCache !*GeneralSt -> *GeneralSt
setABCCache cache ps = {ps & ls.cache = Just cache}

getFICache :: !*GeneralSt -> (FileInfoCache,*GeneralSt)
getFICache ps = accPLoc (\p=:{fi_cache = Just fi_cache}->(fi_cache,{p & fi_cache = Nothing})) ps

setFICache :: !FileInfoCache !*GeneralSt -> *GeneralSt
setFICache ac ps = appPLoc (\p->{p & fi_cache = Just ac}) ps

getProjectFilePath :: !*GeneralSt -> (!Pathname,!*GeneralSt)
getProjectFilePath ps = ps!ls.pr_path

setProjectFilePath :: !Pathname !*GeneralSt -> *GeneralSt
setProjectFilePath path ps = {ps & ls.pr_path = path}

getStup :: !*GeneralSt -> (!Pathname,!*GeneralSt)
getStup ps = ps!ls.stup

//-- NOT YET IMPLEMENTED....

getTargets :: !*GeneralSt -> (![Target],!*GeneralSt)
getTargets ps = accPLoc (\p=:{pm_targets}->(pm_targets,p)) ps

setTargets :: ![Target] !*GeneralSt -> *GeneralSt
setTargets ts ps = appPLoc (\p->{p & pm_targets = ts}) ps

getCurrentTarget :: !*GeneralSt -> (!Int,!*GeneralSt)
getCurrentTarget ps = accPLoc (\p=:{pm_curtarg}->(pm_curtarg,p)) ps

setCurrentTarget :: !Int !*GeneralSt -> *GeneralSt
setCurrentTarget tg ps
	= appPLoc (\p->{p & pm_curtarg = tg}) ps

getCurrentPaths :: !*GeneralSt -> (!(List Pathname),!*GeneralSt)
getCurrentPaths ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_path,ps)

getCurrentDlibs :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentDlibs ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_libs,ps)

getCurrentSlibs :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentSlibs ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_stat,ps)

getCurrentObjts :: !*GeneralSt -> (!(List String),!*GeneralSt)
getCurrentObjts ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_objs,ps)

getCurrentComp :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentComp ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_comp,ps)

getCurrentCgen :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentCgen ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_cgen,ps)

getCurrentLink :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentLink ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_link,ps)

getCurrentDynl :: !*GeneralSt -> (!String,!*GeneralSt)
getCurrentDynl ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_dynl,ps)

getCurrentVers :: !*GeneralSt -> (!Int,!*GeneralSt)
getCurrentVers ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_vers,ps)

getCurrent64BitProcessor :: !*GeneralSt -> (!Bool,!*GeneralSt)
getCurrent64BitProcessor ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.env_64_bit_processor,ps)

getCurrentProc :: !*GeneralSt -> (!Processor,!*GeneralSt)
getCurrentProc ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_proc,ps)

getCurrentMeth :: !*GeneralSt -> (!CompileMethod,!*GeneralSt)
getCurrentMeth ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_meth,ps)

getCompilerProcessIds :: !*GeneralSt -> (!CompilerProcessIds,!*GeneralSt)
getCompilerProcessIds ps = accPLoc (\l -> l!g_compiler_process_ids) ps

setCompilerProcessIds :: !CompilerProcessIds !*GeneralSt -> *GeneralSt
setCompilerProcessIds compiler_project_ids ps = appPLoc (\l -> {l & g_compiler_process_ids = compiler_project_ids}) ps

getInteract  :: !*GeneralSt -> (!Bool,!*GeneralSt)
getInteract ps = (False,ps)

writeLog :: !String !*GeneralSt -> *GeneralSt
writeLog message ps
	= appPLoc (\ls=:{logfile} -> {ls & logfile = writeLogfile message logfile}) ps

abortLog :: !Bool !String !*GeneralSt -> *GeneralSt
abortLog flag message ps
	# ps		= case message of
					""	-> ps
					_	-> appPLoc (\ls=:{logfile} -> {ls & logfile = writeLogfile message logfile}) ps
	# (lf,ps)	= accPLoc (\ls=:{logfile} -> (logfile,{ls & logfile = stderr})) ps
	# (ok,ps)	= closeLogfile lf ps
//	| not ok ...
	# ps = case flag of
		True	-> app_world_instead_of_ps (set_return_code_world (-1)) ps
		_		-> ps
	= {ps & gst_continue_or_stop=True}
