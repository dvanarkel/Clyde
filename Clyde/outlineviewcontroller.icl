implementation module Clyde.outlineviewcontroller

import StdEnv
import StdDebug
import Cocoa.objc
import Cocoa.msg
import Cocoa.dyncall
import Cocoa.Foundation
import System._Pointer, System._Unsafe
import stringutil
import PmProject, PmPath, UtilStrictLists
import Clyde.controls

// outline view callbacks

addOVDataSource :: !Class !*a -> *a
addOVDataSource adc world
	#!	selname			= "outlineView:numberOfChildrenOfItem:\0"
		(sel,world)		= sel_getUid selname world
		imp				= impOutlineViewNummberOfChildrenOfItem
		(ok,world)		= class_addMethod adc sel imp "i@:@@\0" world		// lying about return type here...
	
	#!	selname			= "outlineView:isItemExpandable:\0"
		(sel,world)		= sel_getUid selname world
		imp				= impOutlineViewIsItemExpandable
		(ok,world)		= class_addMethod adc sel imp "i@:@@\0" world		// lying about return type here...
	
	#!	selname			= "outlineView:child:ofItem:\0"
		(sel,world)		= sel_getUid selname world
		imp				= impOutlineViewChildOfItem
		(ok,world)		= class_addMethod adc sel imp "@@:@i@\0" world		// lying about return type here...
	
	#!	selname			= "outlineView:objectValueForTableColumn:byItem:\0"
		(sel,world)		= sel_getUid selname world
		imp				= impOutlineViewObjectValueForTableColumnByItem
		(ok,world)		= class_addMethod adc sel imp "i@:@@@\0" world		// lying about return type here...
	
	#!	selname			= "openIcl:\0"
		(sel,world)		= sel_getUid selname world
		imp				= imp_openIcl
		(ok,world)		= class_addMethod adc sel imp "i@:@\0" world		// lying about return type here...

	#!	selname			= "openDcl:\0"
		(sel,world)		= sel_getUid selname world
		imp				= imp_openDcl
		(ok,world)		= class_addMethod adc sel imp "i@:@\0" world		// lying about return type here...
	
	= world

foreign export openIcl

imp_openIcl :: Int
imp_openIcl = code {
		pushLc openIcl
	}

openIcl :: !Int !Int !Int -> Int
openIcl cls sel outl`
	= openModule True newWorld

foreign export openDcl

imp_openDcl :: Int
imp_openDcl = code {
		pushLc openDcl
	}

openDcl :: !Int !Int !Int -> Int
openDcl cls sel outl`
	= openModule False newWorld


openModule isImplementation env	
	#!	env				= newWorld
		outl			= readInt projectOutlineview 0
		(row,env)		= msgI_I outl "selectedRow\0" env
		(item,env)		= msgII_P outl "itemAtRow:\0" row env
		idx		= lookIdx item
		str		= lookStr item
		pth		= lookPth item
 	#!	sdc				= sharedDocumentController
	 	path			= parts2path (ns2cls str) (ns2cls pth)
		(url,env)		= msgCP_P "NSURL\0" "fileURLWithPath:\0" (p2ns path) env
//	#!	env				= msgIPPP_V sdc "openDocumentWithContentsOfURL:display:completionHandler:\0" url YES NIL env
	#!	(_,env)			= msgIPPP_P sdc "openDocumentWithContentsOfURL:display:completionHandler:\0" url YES NIL env
	= force env 42
where
	parts2path module path
		= path +++. "/" +++. module` +++. if isImplementation ".icl" ".dcl"
	where
		module` = {xvert c \\ c <-: module}
	
		xvert '.' = '/'
		xvert c   = c


NIL	:== 0

sharedDocumentController :: Pointer
sharedDocumentController =: sharedDocumentController
where
	sharedDocumentController :: Pointer
	sharedDocumentController
		#!	(app,world) 	= msgC_P "NSDocumentController\0" "sharedDocumentController\0" newWorld
		= app

foreign export outlineViewNummberOfChildrenOfItem
foreign export outlineViewIsItemExpandable

impOutlineViewNummberOfChildrenOfItem :: Int
impOutlineViewNummberOfChildrenOfItem = code {
		pushLc outlineViewNummberOfChildrenOfItem
	}
	
impOutlineViewIsItemExpandable :: Int
impOutlineViewIsItemExpandable = code {
		pushLc outlineViewIsItemExpandable
	}
	
impOutlineViewChildOfItem :: Int
impOutlineViewChildOfItem
	= dcbNewCallback "iiiii)i\0" addrcbHandlerPPPPP_P 0

	
impOutlineViewObjectValueForTableColumnByItem :: Int
impOutlineViewObjectValueForTableColumnByItem
	= dcbNewCallback "iiiii)i\0" addrcbHandlerPPPPP_P 1
	
outlineViewNummberOfChildrenOfItem :: !Int !Int !Int !Int -> Int
outlineViewNummberOfChildrenOfItem self cmd ov item
	#!	idx		= lookIdx item
		item`	= treeItems.[idx]
		result	= case item` of
			(Node _ l)	= length l
			_			= 0
	= result

outlineViewIsItemExpandable :: !Int !Int !Int !Int -> Int
outlineViewIsItemExpandable self cmd ov item
	| item == 0		= 0		// ???
	#!	idx		= lookIdx item
		item`	= treeItems.[idx]
		result	= case item` of
			(Node _ l)	= 1
			_			= 0
	= result
	
outlineViewChildOfItem :: !Int !Int !Int !Int !Int -> Int
outlineViewChildOfItem self cmd ov child item
	#!	idx		= lookIdx item
		item`	= treeItems.[idx]
		result	= case item` of
			(Node _ l)	= l!!child
			_			= abort "\nunexpected child request\n\n"
	= treeElems.[result]
	
outlineViewObjectValueForTableColumnByItem :: !Int !Int !Int !Int !Int -> Int
outlineViewObjectValueForTableColumnByItem self cmd ov column item
	= lookStr item

cbHandlerPPPPP_P :: !Pointer !Pointer !Pointer !Pointer -> Int
cbHandlerPPPPP_P cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(ov,env)	= dcbArgPointer args_ env
		(nn,env)	= dcbArgPointer args_ env
		(it,env)	= dcbArgPointer args_ env
		result		= case userdata_ of
						0	-> outlineViewChildOfItem self cmd ov nn it
						1	-> outlineViewObjectValueForTableColumnByItem self cmd ov nn it
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'p')

foreign export cbHandlerPPPPP_P

addrcbHandlerPPPPP_P :: !Int
addrcbHandlerPPPPP_P = code {
		pushLc 	cbHandlerPPPPP_P
	}

///// model DEFS

// need objC type to return for instances instead of idx
// elem :== {idx,str}

lookIdx elm
	#!	(idx,env)		= object_getInstanceVariable elm "index\0" newWorld
	= idx

lookStr elm
	#!	(str,env)		= object_getInstanceVariable elm "string\0" newWorld
	= str

lookPth elm
	#!	(str,env)		= object_getInstanceVariable elm "path\0" newWorld
	= str

makeElem :: !Int !String !String -> Int
makeElem idx str pth
	#!	(cls,env)		= objc_getClass "MyElement\0" newWorld
		(ins,env)		= class_createInstance cls 0 env
		(_,env)			= object_setInstanceVariable ins "index\0" idx env
		(_,env)			= object_setInstanceVariable ins "string\0" (p2ns str) env
		(_,env)			= object_setInstanceVariable ins "path\0" (p2ns pth) env
	= force env ins

elemClass :: !*a -> *a
elemClass env
	#!	(cls,env)		= objc_getClass "NSObject\0" env
		(mye,env)		= objc_allocateClassPair cls "MyElement\0" 0 env
		(ok,env)		= class_addIvar mye "index\0" 8 8 "\0" env
		(ok,env)		= class_addIvar mye "string\0" 8 8 "\0" env
		(ok,env)		= class_addIvar mye "path\0" 8 8 "\0" env
		env				= objc_registerClassPair mye env
	= env

:: TreeView	:== {TreeElement}
:: TreeElement
	= Node String [Int]		// dirname		child idcs
	| Leaf String String	// modulename	path

initClydeTree :: !*a -> *a
initClydeTree env
	#!	proj		= "/Clyde.prj"
		prjPath		= "/Users/dvanarkelmaccom/Documents/CleanLab"
		appPath		= "/usr/local/Cellar/clean-itasks/20151022"
		envPaths	= ["/usr/local/Cellar/clean-itasks/20151022/lib/StdEnv"]		// get from IDEEnvs...
	= makeTree proj prjPath appPath envPaths env
	// ??? how to get outlineview to update itself ???

initProjectTree :: !String !String !String ![String] !*a -> *a
initProjectTree proj prjPath appPath envPaths env
	| trace_n ("initProjectTree\t"+++proj) False = undef
	#!	env		= makeTree proj prjPath appPath envPaths env
		outl	= readInt projectOutlineview 0
	= msgIPI_V outl "reloadItem:reloadChildren:\0" NIL YES env
		

makeTree prj prjPath appPath envPaths env
	= set_global (treeElems,treeItems) env
where
	treeItems :: {TreeElement}
	treeItems = accUnsafe (accFiles readProject)
	
	treeElems :: {#Int}
	treeElems = {makeElem idx (getStr elm) (getPth elm) \\ elm <-: treeItems & idx <- [0..]}
	
	getStr (Node s _)	= s
	getStr (Leaf s _)	= s
	
	getPth (Node s _)	= s
	getPth (Leaf _ s)	= s

	readProject :: !*Files -> (TreeView,!*Files)
	readProject files
		#!	path						= prjPath +++. prj
			((proj,succ,errmsg),files)	= ReadProjectFile path appPath files
			items						= ffi_items proj
		= ({i \\ i <- items},files)

	ffi_items proj
		#!	ppaths						= StrictListToList (PR_GetPaths proj)
			srcpaths					= ppaths ++ envPaths // list of project search paths ++ environment search paths
			modules						= PR_GetModuleStuff proj
			mods						= StrictListToList modules
		| isEmpty mods
			= []
		#!	[(root,rootdir,_,_):mods]	= mods
			mods						= sortBy (\(a,b,_,_) (c,d,_,_) -> less a b c d srcpaths) mods
			moditems					= makenice 2 mods
			rootitem					= GetModuleName root
			nodeidcs					= [idx \\ idx <- [2..] & (Node _ _) <- moditems]
		= [Node ( "dummy top") [1:nodeidcs], Leaf rootitem rootdir : moditems]
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
			makenice idx l=:[(module,b,_,_):r]
				#	dir				= symPath appPath prjPath b
					idx				= inc idx
					(elems,rest)	= span (\(_,d,_,_)->d==b) l
					numelems		= length elems
				= [ Node ( dir) [idx..idx+numelems-1] 
				  : map (\(m,_,_,_) -> Leaf (GetModuleName m) b) elems
				  ] ++ makenice (idx+numelems) rest

// how to tie this to our viewcontroller & allow swapping to a new project???
treeElems :: {#Int}
treeElems => fst get_global
/*
treeElems =: {makeElem idx (getStr elm) (getPth elm) \\ elm <-: treeItems & idx <- [0..]}
where
	getStr (Node s _)	= s
	getStr (Leaf s _)	= s
	
	getPth (Node s _)	= s
	getPth (Leaf _ s)	= s
*/
treeItems :: {TreeElement}
treeItems => snd get_global
/*
treeItems =: accUnsafe (accFiles readProject)
where
	prjPath								= "/Users/dvanarkelmaccom/Documents/CleanLab"
	appPath								= "/usr/local/Cellar/clean-itasks/20151022"
	
	readProject :: !*Files -> (TreeView,!*Files)
	readProject files
		#!	path						= prjPath +++. "/Clyde.prj"
			((proj,succ,errmsg),files)	= ReadProjectFile path appPath files
			items						= ffi_items proj
		= ({i \\ i <- items},files)
	
	ffi_items proj
		#!	ppaths						= StrictListToList (PR_GetPaths proj)
			epaths						= ["/usr/local/Cellar/clean-itasks/20151022/lib/StdEnv"]		// get from IDEEnvs...
			srcpaths					= ppaths ++ epaths // list of project search paths ++ environment search paths
			modules						= PR_GetModuleStuff proj
			mods						= StrictListToList modules
		| isEmpty mods
			= []
		#!	[(root,rootdir,_,_):mods]	= mods
			mods						= sortBy (\(a,b,_,_) (c,d,_,_) -> less a b c d srcpaths) mods
			moditems					= makenice 2 mods
			rootitem					= GetModuleName root
//		= [Node (p2ns "dummy top") [1..length moditems+1], Leaf (p2ns rootitem) : moditems]
			nodeidcs					= [idx \\ idx <- [2..] & (Node _ _) <- moditems]
		= [Node ( "dummy top") [1:nodeidcs], Leaf rootitem rootdir : moditems]
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
			makenice idx l=:[(module,b,_,_):r]
				#	dir				= symPath appPath prjPath b
					idx				= inc idx
					(elems,rest)	= span (\(_,d,_,_)->d==b) l
					numelems		= length elems
				= [ Node ( dir) [idx..idx+numelems-1] 
				  : map (\(m,_,_,_) -> Leaf (GetModuleName m) b) elems
				  ] ++ makenice (idx+numelems) rest
*/
traceItems :: String
traceItems	= concatn itemToString [(x,i,p) \\ i <-: treeItems & x <- [0..] & p <-: treeElems]
where
	itemToString (idx, Node s l,p)
		= toString idx +++ " (" +++ toString p +++ "):\tNode " +++ s +++ "\t" +++ concatc toString l
			+++ "\n\t" +++ toString (lookIdx p) +++ "\t" +++ ns2cls (lookStr p)
	itemToString (idx, Leaf s pth ,p)
		= toString idx +++ " (" +++ toString p +++ "):\tLeaf " +++ s
			+++ "\n\t" +++ toString (lookIdx p) +++ "\t" +++ ns2cls (lookStr p)

///// create the project view

createPOView :: !Pointer !Pointer !*a -> *a
createPOView delegate container env
	#!	(bounds,env)	= getBounds container env
//	#!	bounds			= cgRect 10.0 10.0 300.0 300.0
		(scroll,env)	= msgC_P "NSScrollView\0" "alloc\0" env
		(scroll,env)	= msgIS_P scroll "initWithFrame:\0" NSRectType bounds env
		env				= msgII_V scroll "setBorderType:\0" NSBezelBorder env
		env				= msgII_V scroll "setHasVerticalScroller:\0" YES env
		env				= msgII_V scroll "setHasHorizontalScroller:\0" YES env
//		env				= msgII_V scroll "setAutohidesScrollers:\0" NO env
		env				= msgII_V scroll "setAutohidesScrollers:\0" YES env
    //This allows the view to be resized by the view holding it 
//    [tableContainer setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		env				= msgII_V scroll "setAutoresizingMask:\0" (NSViewWidthSizable + NSViewHeightSizable) env
// TODO: Hmmmm... now horizontal scrollbar appears if I resize column... but not if I resize window???

		(cont,env)		= msgI_P scroll "contentView\0" env
		(bounds1,env)	= getBounds cont env
//		env = trace_n ("cont "+++toString cont) env
		origin_x		= readReal8 bounds1 0
		origin_y		= readReal8 bounds1 8
		size_w			= readReal8 bounds1 16
		size_h			= readReal8 bounds1 24
//		env = trace_n (toString origin_x+++"\t"+++toString origin_y) env
//		env = trace_n (toString size_w+++"\t"+++toString size_h) env
	#!	bounds1			= cgRect 0.0 0.0 298.0 298.0

		(outl,env)		= msgC_P "NSOutlineView\0" "alloc\0" env
//		(outl,env)		= msgC_P "NSTableView\0" "alloc\0" env
		(outl,env)		= msgIS_P outl "initWithFrame:\0" NSRectType bounds1 env
		env	= force (writeInt projectOutlineview 0 outl) env
		env = trace_n ("outl "+++toString outl +++ toString (readInt projectOutlineview 0)) env
// - (void)setUsesAlternatingRowBackgroundColors:(BOOL)useAlternatingRowColors; // FOR Profile viewers...

		env				= msgII_V outl "setAutoresizesOutlineColumn:\0" NO env
//		env				= msgII_V outl "columnAutoresizingstyle:\0" 3 env
		env				= msgI_V outl "sizeLastColumnToFit\0" env
		
		(ocol1,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol1,env)		= msgIP_P ocol1 "initWithIdentifier:\0" (c2ns "columnOne\0") env
		(header,env)	= msgI_P ocol1 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "One\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env				// can we set a min width that propagates upwards?
		env			 	= msgII_V ocol1 "setEditable:\0" NO env
		env				= msgIP_V outl "addTableColumn:\0" ocol1 env

		env				= msgIP_V outl "setOutlineTableColumn:\0"  ocol1 env
		
		(ocol2,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol2,env)		= msgIP_P ocol2 "initWithIdentifier:\0" (c2ns "columnTwo\0") env
		(header,env)	= msgI_P ocol2 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "Two\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env
		env			 	= msgII_V ocol2 "setEditable:\0" NO env
		env				= msgIP_V outl "addTableColumn:\0" ocol2 env
		
		
	// set outl delegate & datasource...
		env				= msgIP_V outl "setDelegate:\0" delegate env		// ==> ahh.. probably arrives at app delegate by default so for proper view controller _do_ want to set
		env				= msgIP_V outl "setDataSource:\0" delegate env
		env = trace_n ("outl delegate "+++toString delegate) env

	// set outl double-click response
		(asel,env)		= sel_getUid "openIcl:\0" env
		env				= msgIP_V outl "setDoubleAction:\0" asel env

	// create outl context menu
//		env				= msgII_V outl "setAllowsTypeSelect:\0" NO env		// didn't help...
		(_,env)			= addContextmenu outl delegate env
	
	
	
		env				= msgIP_V scroll "setDocumentView:\0" outl env
		
		env				= msgIP_V container "addSubview:\0" scroll env
		env				= msgI_V scroll "release\0" env
		env				= msgI_V outl "reloadData\0" env
	= env

import System._Posix
projectOutlineview =: malloc 8

import Clyde.menus

// Programmatically, the table view methods for enabling and disabling these options are
// set using the following methods: setAllowsMultipleSelection: and setAllowsEmptySelection:.


addContextmenu outl delegate env
	#!	(menu,env) 	= msgC_P "NSMenu\0" "alloc\0" env
		(menu,env) 	= msgIP_P menu "initWithTitle:\0" (c2ns "ContexMenu\0") env
		(menu,env) 	= msgI_P menu "autorelease\0" env
		
//		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz implementation" "openIcl:\0" "" env
		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz implementation" "openIcl:\0" "\n" env
		env			= msgIP_V item "setKeyEquivalentModifierMask:\0" (0) env
		env			= setTarget item delegate env

//		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz definition" "openDcl:\0" "" env
		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz definition" "openDcl:\0" "\n" env
		env			= msgIP_V item "setKeyEquivalentModifierMask:\0" (NSShiftKeyMask) env
		env			= setTarget item delegate env
//		env			= msgII_V item "setAlternate:\0" YES env
		
//		(item,env)	= addItemWith_title_action_keyEquivalent menu "cmenu C" "doubleClick:" "I" env
//		env			= setTarget item delegate env
//		env			= msgIP_V item "setKeyEquivalentModifierMask:\0" (0) env
		
		env			= msgIP_V outl "setMenu:\0" menu env
	= (menu,env)

/*
	2 issues:
		- how to bind 'Enter' to first & 'Shift Enter' to 2nd
		- how to dynamically update the menu entry?
		
		for b) add delegate & implement validate...
	
*/
/*
In the action method for a menu item, determine whether the clicked table row is in the 
set of indexes returned by selectedRowIndexes. If it is, apply the action to all indexes 
in the set; otherwise, apply the action only to the clicked row. Here’s a convenience 
method that checks the selected row indexes and returns the set of indexes the action 
method should process:

- (NSIndexSet *)_indexesToProcessForContextMenu {
    NSIndexSet *selectedIndexes = [_tableViewMain selectedRowIndexes];
    // If the clicked row is in selectedIndexes, then process all selectedIndexes. Otherwise, process only clickedRow.
    if ([_tableViewMain clickedRow] != -1 && ![selectedIndexes containsIndex:[_tableViewMain clickedRow]]) {
        selectedIndexes = [NSIndexSet indexSetWithIndex:[_tableViewMain clickedRow]];
    }
    return selectedIndexes;
}
*/

///// updateable global...

CbList :: {![a]}
CbList =: {[]}

set_global :: !a !*env -> *env
set_global cb env
	= force (updateArray0 (cast CbList) [cb]) env
where
	updateArray0 :: !*{!.e} !.e -> *{!.e} | Array {!} e
	updateArray0 a e = {a & [0] = e}
	
get_global :: a
get_global => hd CbList.[0]

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x


///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

cast :: !.a -> .b
cast _ = code inline {
		pop_a	0
	}
