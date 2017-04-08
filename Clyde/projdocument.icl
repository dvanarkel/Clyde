implementation module Clyde.projdocument

import StdEnv
import StdDebug

import System._Pointer
import System._Posix
import System._Unsafe

import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Cocoa.dyncall		// needed since we have callbacks with (too) many arguments

import Clyde.controls
import Clyde.menus
import Clyde.projwindowcontroller

documentClass	:== "ProjDocument\0"

createProjDocumentClass :: !*a -> *a
createProjDocumentClass env
	#!	env	= createClass "NSDocument" "ProjDocument" projDocumentMethods projDocumentIvars env
		env	= createClass "NSObject" "MyElement" elemMethods elemIvars env
	= env

projDocumentMethods =
	[ ("init",	 								imp_init,						"@@:\0")
	, ("readFromURL:ofType:error:",				exportedCBHandler2,				"i@:@@@\0")
	, ("writeToURL:ofType:error:",	 			exportedCBHandler3,				"i@:@@@\0")
	, ("makeWindowControllers", 				imp_makeWindowControllers,		"i@:@\0")
	, ("build:",								impBuild,						"v@:@\0")
	, ("buildAndRun:",							impBuildAndRun,					"v@:@\0")
	, ("run:",									impRun,							"v@:@\0")
	]
projDocumentIvars =
	[ ("ppath",		8,	3,	"@\0")
	, ("root",		8,	3,	"@\0")
	]

elemMethods =
	[ 
	]
elemIvars =
	[ ("string",		8,	8,	"\0")
	, ("path",			8,	8,	"\0")
	, ("isgroup",		8,	8,	"\0")	// is expandable; use as BOOL : YES/NO
	, ("children",		8,	8,	"\0")	// use for length and child retrieval (NSArray)
	]

// init

imp_init :: Int
imp_init = code {
		pushLc initProjDocument
	}
	
foreign export initProjDocument

initProjDocument :: !Int !Int -> Int
initProjDocument self cmd
	| trace_n ("entering "+++object_getClassName self+++"\t"+++sel_getName cmd) False = undef
//	#!	(sel,_)	= sel_getUid "init\0" newWorld
//	= objc_msgSendSuper (class_getSuperclass self) sel
//	= objc_msgSendSuper self sel
	#!	cls	= fst (objc_getClass documentClass newWorld)
		ptr	= malloc 16
		ptr	= writeInt ptr 0 self
		ptr	= writeInt ptr 8 cls
		(ret,world)		= msgS_P ptr cmd newWorld
	| ret == 0
		= ret
	#!	root			= -1
		(_,world)		= object_setInstanceVariable self "root\0" root world
	#!	(root`,world)	= object_getInstanceVariable self "root\0" world
		(_,world)		= object_setInstanceVariable self "ppath\0" 0 world
	#!	(path`,world)	= object_getInstanceVariable self "ppath\0" world
	| trace_n ("set root: "+++toString root+++"\t"+++toString root`+++"\t"+++ toString path` +++ ns2cls path`) False = undef
	= force world ret


// readFromURL_ofType_error

foreign export readProjFromURL

readProjFromURL :: !Pointer !Pointer !Pointer !Pointer -> Int
readProjFromURL cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(tv,env)	= dcbArgPointer args_ env
		(col,env)	= dcbArgPointer args_ env
		(row,env)	= dcbArgInt args_ env
		result		= readFromURL_ofType_error self cmd tv col row
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'i')

addrMyCBHandler2 :: Int
addrMyCBHandler2 = code {
		pushLc 	readProjFromURL
	.d 0 1 i
		rtn
	}

exportedCBHandler2 :: Pointer
exportedCBHandler2
	= dcbNewCallback "iiiii)i\0" addrMyCBHandler2 0

readFromURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
readFromURL_ofType_error self cmd absoluteURL typeName outError
	| trace_n ("readFromURL_ofType_error called") False = undef
	| trace_n ("self class: "+++object_getClassName self+++"\t"+++toString self) False = undef
	#!	env					= newWorld
		(fileB,env)			= msgI_I absoluteURL "isFileURL\0" env
		(pathN,env)			= msgI_P absoluteURL "path\0" env
	| trace_n ("url is file: "+++toString fileB) False = undef
	| trace_n ("path: '"+++ ns2cls pathN+++"'") False = undef
	| trace_n ("type: '"+++ ns2cls typeName +++ "'") False = undef

	#!	path				= ns2cls pathN
		(prjPath,prj)		= splitRight path
		(appPath,env)		= cleanhome env
		(root,env)			= initProjectTree prj prjPath appPath env
		(_,env)				= object_setInstanceVariable self "root\0" root env			// HUH??? Doesn't appear to actually work?!
		env					= msgI_V root "retain\0" env
		(root`,env)			= object_getInstanceVariable self "root\0" env
// need to release previous one if present...
//	| trace_n ("set contentString to: "+++toString str+++"\t"+++toString str`) False = undef
	| trace_n ("set root: "+++toString root+++"\t"+++toString root`+++"\tself\t"+++toString self) False = undef

	#!	(_,env)				= object_setInstanceVariable self "ppath\0" pathN env			// HUH??? Doesn't appear to actually work?!
		env					= msgI_V pathN "retain\0" env	// does this make sense?
		(path`,env)			= object_getInstanceVariable self "ppath\0" env
	| trace_n ("set path: "+++toString pathN +++"\t"+++toString path` +++"\t"+++ns2cls path`) False = undef
// need to release previous one if present...
	= YES

splitRight path
	#!	idx	= last [ i \\ i <- [0..size path-1] | path.[i] == '/' ]
	= (path%(0,idx-1),path%(idx,size path - 1))

getError :: !Int !*a -> (!String,!*a)
getError errorHdl env
	#!	errorObj	= readInt errorHdl 0
	| errorObj == 0
		= ("",env)
	#!	(err,env)	= msgI_P errorObj "localizedDescription\0" env
	= (ns2cls err,env)

// writeToURL_ofType_error

foreign export writeProjToURL

writeProjToURL :: !Pointer !Pointer !Pointer !Pointer -> Int
writeProjToURL cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(tv,env)	= dcbArgPointer args_ env
		(col,env)	= dcbArgPointer args_ env
		(row,env)	= dcbArgInt args_ env
		result		= writeToURL_ofType_error self cmd tv col row
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'i')

addrMyCBHandler3 :: Int
addrMyCBHandler3 = code {
		pushLc 	writeProjToURL
	.d 0 1 i
		rtn
	}

exportedCBHandler3 :: Pointer
exportedCBHandler3
	= dcbNewCallback "iiiii)i\0" addrMyCBHandler3 0

writeToURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
writeToURL_ofType_error self cmd absoluteURL typeName outError
	| trace_n ("writeToURL_ofType_error called") False = undef
	| trace_n ("self class: "+++object_getClassName self) False = undef
	#!	env				= newWorld
		(fileB,env)		= msgI_I absoluteURL "isFileURL\0" env
		(pathN,env)		= msgI_P absoluteURL "path\0" env
	| trace_n ("url is file: "+++toString fileB) False = undef
	| trace_n ("path: '"+++ ns2cls pathN+++"'") False = undef
	| trace_n ("type: '"+++ ns2cls typeName +++ "'") False = undef

	#!	(wind,env)		= msgI_P self "windowForSheet\0" env
	| trace_n ("window class: '"+++object_getClassName wind+++"'\t"+++toString wind) False = undef
	#!	(cont,env)		= msgI_P wind "contentView\0" env
	| trace_n ("content class: '"+++object_getClassName cont+++"'\t"+++toString cont) False = undef
	#!	(docv,env)		= msgI_P cont "documentView\0" env
	| trace_n ("document class: '"+++object_getClassName docv+++"'\t"+++toString docv) False = undef
/* NEED TO FIGURE OUT HOW TO SAVE PROJECT BACK OUT (AND STORE IT IN THE FIRST PLACE)
	#! (nss,env)		= msgI_P docv "string\0" env
	| trace_n ("textv string length "+++toString (size (ns2cls nss))) False = undef
	// should get this from document:contentString (provided we update it on time...)

	#!	errorHdl		= malloc 40
		errorHdl		= writeInt errorHdl 0 0
		(ok,env)		= msgIPPPP_I nss "writeToURL:atomically:encoding:error:\0" absoluteURL YES NSUTF8StringEncoding errorHdl env
		
		(error,env)		= getError errorHdl env

		string			= ns2cls nss
	| trace_n ("Ok: '"+++toString ok+++"'") False = undef
	| trace_n ("Error: '"+++error+++"'") False = undef
//	| trace_n ("String: '"+++string+++"'") False = undef
*/
	= YES

// makeWindowControllers

foreign export makeProjWindowControllers

imp_makeWindowControllers :: Int
imp_makeWindowControllers = code {
		pushLc makeProjWindowControllers
	}

makeProjWindowControllers :: !Int !Int -> Int
makeProjWindowControllers self cmd
	#!	world				= newWorld
		(wctrl,world)		= makeProjWindowController self world
		world				= msgIP_V self "addWindowController:\0" wctrl world
	= force world 42


///// model DEFS

// need objC type to return for instances instead of idx
// elem :== {idx,str}

lookStr :: !Pointer -> Pointer
lookStr elm
	#!	(str,env)		= object_getInstanceVariable elm "string\0" newWorld
	= str

lookPth :: !Pointer -> Pointer
lookPth elm
	#!	(str,env)		= object_getInstanceVariable elm "path\0" newWorld
	= str

lookIsGroup :: !Pointer -> Bool
lookIsGroup elm
	#!	(isg,env)		= object_getInstanceVariable elm "isgroup\0" newWorld
	= isg==YES

lookNumChildren :: !Pointer -> Pointer
lookNumChildren elm
	#!	(chs,env)		= object_getInstanceVariable elm "children\0" newWorld
		(num,env)		= msgI_I chs "count\0" env
	= num

lookChild :: !Int !Pointer -> Pointer
lookChild child elm
	#!	(chs,env)		= object_getInstanceVariable elm "children\0" newWorld
		(chd,env)		= msgII_P chs "objectAtIndex:\0" child env
	= chd

lookElem :: !Pointer -> (!String,!String,!Bool,![Pointer])
lookElem elm
	#!	env				= newWorld
		(str,env)		= object_getInstanceVariable elm "string\0" env
		(pth,env)		= object_getInstanceVariable elm "path\0" env
		(isg,env)		= object_getInstanceVariable elm "isgroup\0" env
		(chs,env)		= object_getInstanceVariable elm "children\0" env
		(num,env)		= msgI_I chs "count\0" env
		(children,env)	= seqList [getChild elm i \\ i <- [0..num]] env
	= force env (ns2cls str,ns2cls pth,isg==YES,children)
where
	getChild chs i env
		= msgII_P chs "objectAtIndex:\0" i env

makeElem :: !String !String !Bool !{#Int} -> Pointer
makeElem str pth isgroup children
	#!	(cls,env)		= objc_getClass "MyElement\0" newWorld
		(ins,env)		= class_createInstance cls 0 env
		(_,env)			= object_setInstanceVariable ins "string\0" (p2ns str) env
		(_,env)			= object_setInstanceVariable ins "path\0" (p2ns pth) env
		(_,env)			= object_setInstanceVariable ins "isgroup\0" (if isgroup YES NO) env

		(arr,env)		= makeArray children env
		(_,env)			= object_setInstanceVariable ins "children\0" arr env
		env				= msgI_V ins "retain\0" env
	= force env ins

makeArray :: !{#Int} !*a -> (!Pointer,!*a)
makeArray cs env
	#!	(arr,env)	= msgC_P "NSMutableArray\0" "array\0" env
		env			= addObjects 0 cs arr env
		env			= msgI_V arr "retain\0" env
	= (arr,env)

addObjects :: !Int !{#Int} !Pointer !*a -> *a
addObjects idx cs arr env
	| idx >= size cs
		= env
	#!	elm			= cs.[idx]
		env			= msgIP_V arr "addObject:\0" elm env
	= addObjects (inc idx) cs arr env
	
:: TreeView	:== {TreeElement}
:: TreeElement
	= Node String [Int]		// dirname		child idcs
	| Leaf String String	// modulename	path

isNode (Node _ _)	= True
isNode _			= False

//	= msgIPI_V outl "reloadItem:reloadChildren:\0" NIL YES env

from PmProject			import :: Project, ReadProjectFile, PR_GetPaths, PR_GetModuleStuff, PR_GetTarget
from PmPath				import :: Pathname, :: Modulename, GetModuleName, symPath
from UtilStrictLists	import :: List, StrictListToList
from PmEnvironment		import :: Target(..), :: Processor, :: CompileMethod, openEnvironments, EnvsFileName
import StdStrictLists

instance FileEnv Files where
	accFiles accfun io = accfun io
	appFiles appfun io = appfun io

readEnvironment :: !String !*World -> (![String],!*World)
readEnvironment env world
	#!	(home, world)			= cleanhome world
	#	envspath				= home +++ "/etc/" +++. EnvsFileName
		(envs, world)			= openEnvironments home envspath world
		senv				 	= [ e \\ e <- envs | e.target_name == env ]
	| isEmpty senv
		= ([], world)
	#	envPaths				= [ p \\ p <|- (hd senv).target_path ]
	= (envPaths, world)

readProject :: !String !String !String !*World -> (TreeView,!*World)
readProject prj prjPath appPath world
		#!	path						= prjPath +++. prj
			((proj,succ,errmsg), world)	= accFiles (ReadProjectFile path appPath) world
		| not succ && trace_n ("failed to read project file: '"+++path+++"' with error: '"+++errmsg+++"'") True
			= ({}, world)
		= projectToTreeView proj (symPath prjPath appPath) world

projectToTreeView proj symbolisize world
		#!	modules						= PR_GetModuleStuff proj
			mods						= [ (mod,pth) \\ (mod,pth,_,_) <|- modules ]
		| isEmpty mods
			= ({}, world)
		#!	(envPaths, world)			= readEnvironment (PR_GetTarget proj) world
		#!	ppaths						= StrictListToList (PR_GetPaths proj)
			srcpaths					= ppaths ++ envPaths // list of project search paths ++ environment search paths
			[(root,rootdir):mods]		= mods
			mods						= sortBy (\(a,b) (c,d) -> less a b c d srcpaths) mods
//			mods						= doHierMods mods
		#!	moditems					= makenice 2 mods
			rootitem					= GetModuleName root
			nodeidcs					= [idx \\ idx <- [2..] & (Node _ _) <- moditems]
			items						= [ Node ("dummy top") [1:nodeidcs]
										  , Leaf rootitem rootdir 
										  : moditems
										  ]
		= ({i \\ i <- items}, world)
where
	less a b c d srcpaths
		| before b d srcpaths
			= True		// use < -ordening of searchpaths...
		| b == d
			= a < c
		= False

	before x y [] = False
	before x y [p:r]
		| p == y = False
		| p == x = True
		= before x y r

	makenice idx [] = []
	makenice idx l=:[(module,b):r]
		#	dir				= symbolisize b
			idx				= inc idx
			(elems,rest)	= span (\(_,d)->d==b) l
			numelems		= length elems
		= [ Node ( dir) [idx..idx+numelems-1] 
		  : map (\(m,_) -> Leaf (GetModuleName m) b) elems
		  ] ++ makenice (idx+numelems) rest

/* doHierMods:
  transform [(modname,modpath,n,n)] where modnames are hierarchical to simple modname & hierarchical paths...

  so extract modname prefix & path 
  group where same prefix & path -> 
  note that prefix can be multi-level...
*/
doHierMods mods
	= []

initProjectTree :: !String !String !String !*World -> (!Pointer,!*World)
initProjectTree prj prjPath appPath env
	#!	(treeItems,env)	= readProject prj prjPath appPath env
	| size treeItems == 0
		= (0,env)
	#!	root			= treeElem treeItems 0
	= (root,env)
where
	treeElem :: !{TreeElement} !Int -> Pointer
	treeElem treeItems idx
		#	elem	= treeItems.[idx]
		= case elem of
			(Node s c)	-> makeElem s s True {# treeElem treeItems e \\ e <- c}
			(Leaf s p)	-> makeElem s p False {}

//updateDummy	(-> #0 Node: #1 ++ all Node elements as children)
//updateMain	(-> #1 Leaf: main module)
//updateNodeLeafs

updateVar ptr name value env
	#!	(str,env)		= object_getInstanceVariable ptr name env
		env				= msgI_V str "release\0" env
		(_,env)			= object_setInstanceVariable ptr name (p2ns value) env
	= env

updateLeaf ptr str pth env
	#!	env				= updateVar ptr "string\0" str env
		env				= updateVar ptr "path\0" pth env
	= env

//updateNode ptr str children env
//...

removeLeaf ptr env
	#!	(var,env)		= object_getInstanceVariable ptr "string\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "path\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "isgroup\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "children\0" env
		env				= msgI_V var "release\0" env
	= env
removeNode ptr env
	#!	(var,env)		= object_getInstanceVariable ptr "string\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "path\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "isgroup\0" env
		env				= msgI_V var "release\0" env
		(var,env)		= object_getInstanceVariable ptr "children\0" env
		(num,env)		= msgI_I var "count\0" env
		env				= removeChildren (dec num) var env
		env				= msgI_V var "release\0" env
	= env
removeChildren idx ptr env
	| idx < 0
		= env
	#!	(chd,env)		= msgII_P ptr "objectAtIndex:\0" idx env
		env				= removeNode chd env
	= removeChildren (dec idx) ptr env

insertNode = undef

// how to maintain current selection in list??
updProjectTree :: !Pointer !{TreeElement} !*env -> *env
updProjectTree root tree env
	#	(rs,rp,rg,rc)					= lookElem root			// fetch dummy
		(Node _ dc)						= tree.[0]
// update main
		(Leaf str pth)					= tree.[1]
		env								= updateLeaf rc.[0] str pth env
// update NLs
		(acc,env)						= updateProjectDir (tl rc) (tl dc) env 
// update dummy
	= undef		// <== TODO
where
	updateProjectDir [] [] acc env
		= (reverse acc,env)
	updateProjectDir [root:roots] [] acc env
		#	env		= removeNode root env
		= updateProjectDir roots [] acc env
	updateProjectDir [] [(Node n cs):nodes] acc env
		#	(root,env)		= insertNode env
		= updateProjectDir [] nodes [root:acc] env
//	updateProjectDir [root:roots] [(Node ...):nodes] acc env

//updProjectTree roots tree idxs env	// root is pointer to tree.[idx]
// we now at top level [Node (dummy),Leaf (main),mods] where mods = (Node;Leaf+)*
updProjectTree___ [root:roots] tree [idx:idxs]	
	#	elem							= tree.[idx]
		(s,p,g,c)						= case elem of
			(Node s c)	-> ( s, s, True, c)
			(Leaf s p)	-> ( s, p, False, [])
		(string,path,isgroup,children)	= lookElem root
	| s == string
	&& p == path
	&& g == isgroup
		#	rs	= updProjectTree children tree c
	//		..	= children		// we kinda want to update the nsarray rather than remove & realloc
		= undef		// <== TODO
//	&& c/children?
updProjectTreeNode root tree idx	
	#	elem							= tree.[idx]
		(s,p,g,c)						= case elem of
			(Node s c)	-> ( s, s, True, c)
			(Leaf s p)	-> ( s, p, False, [])
		(string,path,isgroup,children)	= lookElem root
	| s == string
	&& p == path
	&& g == isgroup
		#	rs	= updProjectTree children tree c
//			..	= children		// we kinda want to update the nsarray rather than remove & realloc
		= undef		// <== TODO
//	&& c/children?

/*
how to update?
if path (=Node/isgroup)

cur		expect
N		N
					before cur expect -> remove cur & children; else insert expect & children
L		L
					assert paths should match
					cur < expect -> remove cur; else insert expect;
N		L			insert expect
L		N			remove cur
-		N			insert expect & children
-		L			insert expect
N		-			remove cur & children
L		-			remove cur
*/


// build:
// buildAndRun:
// run:
import Clyde.projactions

impBuild :: IMP
impBuild = code {
		pushLc 	Build
	}
impBuildAndRun :: IMP
impBuildAndRun = code {
		pushLc 	BuildAndRun
	}
impRun :: IMP
impRun = code {
		pushLc 	Run
	}

foreign export Build
foreign export BuildAndRun
foreign export Run
import IdeState, messwin

Build :: !Int !Int !Int -> Int
Build self cmd notification
	#!	env						= newWorld
		(pathN,env)				= object_getInstanceVariable self "ppath\0" env
		path					= ns2cls pathN
		(ret,env)				= build False path cont env
	= ret
where
	cont exepath linked ok ps
		| trace_n ("cont\t"+++exepath+++"\t"+++toString linked+++"\t"+++toString ok) False = undef
		| linked || not ok
			= closeInfo ps
		= showInfo (Level1 "Project is up to date") ps

//REFRESH PROJECT WINDOW AFTER BUILD

BuildAndRun :: !Int !Int !Int -> Int
BuildAndRun self cmd notification
	#!	env						= newWorld
//	#!	(ret,env)				= buildAndRun env
		(pathN,env)				= object_getInstanceVariable self "ppath\0" env
		path					= ns2cls pathN
		(ret,env)				= build False path cont env
	= ret
where
	cont :: !String !Bool !Bool !*GeneralSt -> *GeneralSt
	cont execpath linked ok ps
		#!	ps					= closeInfo ps
		| not ok
			= ps
		= RunProcess execpath ps

RunProcess :: !String !*GeneralSt -> *GeneralSt
RunProcess execpath ps=:{gst_world}
		#!	world				= case True of
										True	-> LaunchApplication execpath gst_world
										_		-> RunWithRedirect execpath gst_world	
//		#!	world				= RunWithRedirect execpath gst_world
	= {ps & gst_world = world}

RunWithRedirect :: !String !*World -> *World
RunWithRedirect execpath env
// :-) also need to be able to redirect stdin...
// and subclass keyDown on the NSTextView (or NSWindow or...)

//console setup notifications...
	#!	env	= openConsoleWindow execpath env
/*	#!	(res,env)				= runProcessWithRedirect execpath [] Nothing
									(Just con_stdin_remote) 
									(Just con_stdout_remote) 
									(Just con_stdout_remote) 
//									(Just con_stderr_remote) 
									env	
*/
	= env

import PmProject, PmPath, Clyde.Process, Clyde.Console
import Clyde.ClydeApplicationController
// Run should check with project whether to run standalone or with redirected console...
// for now default to standalone launch
// http://stackoverflow.com/questions/5048677/launching-an-mac-app-with-objective-c-cocoa

// open in Terminal kind of works... but isn't great

LaunchApplication path env
	| trace_n ("Enter LaunchApplication... "+++path) False = undef
	#	(nsws,env)				= msgC_P "NSWorkspace\0" "sharedWorkspace\0" env
//		(ret,env)				= msgIP_I nsws "launchApplication:\0" (p2ns path) env
//		(ret,env)				= msgIPPI_I nsws "openFile:withApplication:andDeactivate:\0" (p2ns path) (p2ns "Terminal") YES env
		(ret,env)				= msgIPPP_P nsws "openFile:withApplication:andDeactivate:\0" (p2ns path) (p2ns "Terminal") YES env
	| trace_n ("Returned: "+++toString ret) False = undef
	| ret == 0
		#	env					= openLogWindow env
		= appendLogWindow ("failed to launch " +++ path) env
	= env

Run :: !Int !Int !Int -> Int
Run self cmd notification
	| trace_n ("Enter Run...") False = undef
	#!	env						= newWorld
		(pathN,env)				= object_getInstanceVariable self "ppath\0" env
		proj_path				= ns2cls pathN
	| trace_n ("Path: "+++proj_path) False = undef

	#	(app_path,env)			= cleanhome env
		((proj,ok,err), env)	= accFiles (ReadProjectFile proj_path app_path) env

		prj_path`				= PR_GetRootDir proj
		execpath				= PR_GetExecPath proj
		execpath				= fulPath app_path prj_path` execpath
	| trace_n ("Exec: "+++execpath) False = undef

	#	env						= case True of
									True	-> LaunchApplication execpath env
									_		-> RunWithRedirect execpath env	
// gives out/err in Clyde out/err.. (for console apps)
// runs process as child process, maybe not what we really want?
	= force env 42

/*

// open in project window...

foreign export project

imp_project :: IMP
imp_project = code {
		pushLc 	project
	}

project :: !Int !Int !Int -> Int
project self cmd notification
	#!	env			= newWorld
		ret		= 42

		(rep,env)	= msgI_P notification "representedObject\0" env
		str			= ns2cls rep
	| trace_n ("entering project\t"+++object_getClassName self+++"\t"+++sel_getName cmd+++"\t"+++object_getClassName notification+++"\t"+++object_getClassName rep+++"\t'"+++str+++"'") False = 17

	#!	proj		= "/" +++ str
		prjPath		= "/Users/dvanarkelmaccom/Documents/CleanLab"
		appPath		= "/usr/local/Cellar/clean-itasks/20151022"
		envPaths	= ["/usr/local/Cellar/clean-itasks/20151022/lib/StdEnv"]		// get from IDEEnvs...

		env			= initProjectTree proj prjPath appPath envPaths env
	= force env ret
*/

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x
//	= x
	= code inline {
	.o 2 0
		pop_a 1
	.d 1 0
	}

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

