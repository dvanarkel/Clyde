implementation module Clyde.projwindowcontroller

import StdEnv
import System._Posix
import System._Pointer
import Cocoa.objc
import Cocoa.msg
import Cocoa.dyncall
import Cocoa.Foundation
import Clyde.controls
import Clyde.menus
import Clyde.projdocument

import StdDebug

NIL	:== 0

/////

sharedDocumentController :: Pointer
sharedDocumentController =: sharedDocumentController
where
	sharedDocumentController :: Pointer
	sharedDocumentController
		#!	(app,world) 	= msgC_P "NSDocumentController\0" "sharedDocumentController\0" newWorld
		= app

/////

makeProjWindowControllerClass :: !*a -> *a
makeProjWindowControllerClass env
	#!	(cls,env)		= objc_getClass "NSWindowController\0" env
		(adc,env)		= objc_allocateClassPair cls "ProjWindowController\0" 0 env

//	#!	(sel,env)		= sel_getUid "init\0" env
//		(ok,env)		= class_addMethod adc sel imp_init "@@:\0" env

	#!	(sel,env)		= sel_getUid "setDocument:\0" env
		(ok,env)		= class_addMethod adc sel imp_setdocument "i@:@\0" env

		env				= addOVDataSource adc env
		
	#!	(ok,env)		= class_addIvar adc "outlineview\0" 8 3 "@\0" env

	#!	env				= objc_registerClassPair adc env
	= env

// outline view callbacks, needs to be in subclass of NSWindowController

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

/////

foreign export setDocument

imp_setdocument :: Int
imp_setdocument = code {
		pushLc setDocument
	}

setDocument :: !Int !Int !Int -> Int
setDocument self cmd document
//	super setDocument document
	| trace_n ("setDocument\t"+++object_getClassName self +++ "\t" +++ sel_getName cmd+++"\t"+++object_getClassName document) False = undef
	| trace_n ("["+++toString self+++"\t"+++toString document+++"]") False = undef
	#!	env				= newWorld

	#!	(cls,env)		= objc_getClass "NSWindowController\0" env
		ptr				= malloc 16
		ptr				= writeInt ptr 0 self
		ptr				= writeInt ptr 8 cls
	| trace_n ("before super setDocument:\t"+++toString ptr+++"\t"+++toString cls) False = undef
	#!	(ret,env)		= msgSP_I ptr cmd document env
	| trace_n ("after super setDocument:\t"+++toString ret) False = undef

	#!	(outl,env)		= object_getInstanceVariable self "outlineview\0" env
		env				= msgIPI_V outl "reloadItem:reloadChildren:\0" NIL YES env
	= force env ret

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

foreign export outlineViewNummberOfChildrenOfItem
foreign export outlineViewIsItemExpandable

zeroToRoot ov 0
	#!	env				= newWorld
		(ovdel,env)		= msgI_P ov "delegate\0" env
		(doc,env)		= msgI_P ovdel "document\0" env
	| doc == 0
		= 0
	#!	(root,env)		= object_getInstanceVariable doc "root\0" env
	= root
zeroToRoot ov item
	= item

outlineViewNummberOfChildrenOfItem :: !Int !Int !Int !Int -> Int
outlineViewNummberOfChildrenOfItem self cmd ov item
	| trace_n ("outlineViewNummberOfChildrenOfItem\t"+++toString self+++"\t"+++toString ov+++"\t"+++toString item) False = undef
	#!	item	= zeroToRoot ov item
	| item == 0
		= 0
	= lookNumChildren item

/*
// 0 == root
	| item == 0
//		#!	(root,env)		= object_getInstanceVariable elm "root\0" newWorld
		#!	env	= newWorld
			selfclass		= object_getClassName self
			ovclass			= object_getClassName ov
			cmdname			= sel_getName cmd
		| trace_n ("selfclass\t"+++selfclass+++"\t"+++toString self) False = undef
		| trace_n ("ovclass\t"+++ovclass) False = undef
		| trace_n ("cmd\t"+++cmdname) False = undef
		#!	(ovdel,env)		= msgI_P ov "delegate\0" env
		| trace_n ("ov delegate\t"+++toString ovdel+++"\t"+++object_getClassName ovdel) False = undef
		#!	(doc,env)		= msgI_P ovdel "document\0" env
		| trace_n ("document\t"+++toString doc+++"\t"+++object_getClassName doc) False = undef
		| doc == 0
			= 0
		#!	(root,env)		= object_getInstanceVariable doc "root\0" env
//		#!	(root,env)		= msgI_P doc "root\0" env
		| trace_n ("root\t"+++toString root+++"\t"+++object_getClassName root) False = undef
//		= abort "\nchildren of nil\n\n"
		#!	num	= lookNumChildren root
		= trace_n ("root number of children: "+++toString num) num
	= lookNumChildren item
*/
outlineViewIsItemExpandable :: !Int !Int !Int !Int -> Int
outlineViewIsItemExpandable self cmd ov item
	| item == 0		= 0		// ???
	| lookIsGroup item
		= 1
		= 0
	
outlineViewChildOfItem :: !Int !Int !Int !Int !Int -> Int
outlineViewChildOfItem self cmd ov child item
//	= lookChild child item
	#!	item	= zeroToRoot ov item
	| item == 0
		= 0
	#!	newelm	= lookChild child item
	| trace_n ("outlineViewChildOfItem\t"+++toString item+++"\t"+++toString child+++"\t-> "+++toString newelm) False = undef
	= newelm
	
outlineViewObjectValueForTableColumnByItem :: !Int !Int !Int !Int !Int -> Int
outlineViewObjectValueForTableColumnByItem self cmd ov column item
	#!	item	= zeroToRoot ov item
	| item == 0
		= 0
	= lookStr item

foreign export openIcl

imp_openIcl :: Int
imp_openIcl = code {
		pushLc openIcl
	}

openIcl :: !Int !Int !Int -> Int
openIcl cls sel outl`
	= openModule True cls newWorld

foreign export openDcl

imp_openDcl :: Int
imp_openDcl = code {
		pushLc openDcl
	}

openDcl :: !Int !Int !Int -> Int
openDcl cls sel outl`
	= openModule False cls newWorld


openModule isImplementation cls env	
	#!	env				= newWorld
//		outl			= readInt projectOutlineview 0
		(outl,env)		= object_getInstanceVariable cls "outlineview\0" env
		(row,env)		= msgI_I outl "selectedRow\0" env
		(item,env)		= msgII_P outl "itemAtRow:\0" row env
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

addrcbHandlerPPPPP_P :: Int
addrcbHandlerPPPPP_P = code {
		pushLc 	cbHandlerPPPPP_P
	}


// ???
// where do delegate menu items go?
// window controller sounds correct...

import Clyde.windows

makeProjWindowController :: !Pointer !*World -> (!Pointer,!*World)
makeProjWindowController document/*appdelegate*/ env
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" NSRectType rect style backing False env
/*
		(screen,env)	= msgI_P wind "screen\0" env
		(vis,env)		= visibleFrame screen env
		env = trace_n ("screen "+++toString screen+++"\t"+++rect2string vis) env
		(frm,env)		= getFrame wind env
		shgt			= readReal8 vis 24
		fhgt			= (readReal8 frm 24) - (readReal8 frm 8)
		top				= shgt - fhgt
		env				= msgIS_V wind "setFrameOrigin:\0" NSSizeType (NSMakeSize 0.0 top) env	//NSPoint
*/
		env				= cascade wind env

		env				= msgIP_V wind "setTitle:\0" (c2ns "Project Window\0") env

		(view,env)		= msgC_P "NSView\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 400.0 400.0
//		(cont_,env)		= msgI_P wind "contentView:\0" env
//		(rect,env)		= getBounds cont_ env
		(view,env)		= msgIS_P view "initWithFrame:\0" NSRectType rect env
//		(flip,env)		= msgI_I view "isFlipped\0" env
//		env = trace_n ("root is flipped: "+++toString flip) env

		(wctrl,env)		= msgC_P "ProjWindowController\0" "alloc\0" env
		env				= msgIB_V wctrl "setShouldCascadeWindows:\0" True env

		(wctrl,env)		= msgIP_P wctrl "initWithWindow:\0" wind env
		env				= msgIB_V wctrl "setShouldCascadeWindows:\0" True env
		env = trace_n ("project window controller: "+++toString wctrl) env
		
		env				= msgIP_V wind "setContentView:\0" view env
		(vw,env)		= msgI_P wind "contentView\0" env
//		(should,env)	= msgI_B wctrl "shouldCascadeWindows\0" env
		(should,env)	= msgI_I wctrl "shouldCascadeWindows\0" env
		env				= trace_n ("should\t"+++toString (should<>0)) env
		env				= createPOView wctrl view env		
		wind			= msgIS_V "setContentRect:\0" rect env
//		(w,env)			= msgI_P wind "becomeFirstResponder\0" env	
//		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env

	= (wctrl,env)

traceVisible i wind env
	#!	(vis,env)		= msgI_I wind "isVisible\0" env
	| trace_n ("visible ("+++toString i+++"): "+++ (if (vis==0) "NO" "YES")) False = undef
	= env

// create the project view

//projectOutlineview =: malloc 8	// belongs in subclass of window or windowcontroller/delegate

createPOView :: !Pointer !Pointer !*a -> *a
createPOView delegate container env
	| trace_n ("createPOView delegate: "+++toString delegate+++"\tcontainer: "+++toString container) False = undef
	#!	(bounds,env)	= getBounds container env
		(scroll,env)	= msgC_P "NSScrollView\0" "alloc\0" env
		(scroll,env)	= msgIS_P scroll "initWithFrame:\0" NSRectType bounds env
		env				= msgII_V scroll "setBorderType:\0" NSBezelBorder env
		env				= msgII_V scroll "setHasVerticalScroller:\0" YES env
		env				= msgII_V scroll "setHasHorizontalScroller:\0" NO env
		env				= msgII_V scroll "setAutohidesScrollers:\0" YES env
		env				= msgII_V scroll "setAutoresizingMask:\0" (NSViewWidthSizable + NSViewHeightSizable) env

		bounds1			= cgRect 0.0 0.0 5000.0 5000.0	//298.0 298.0
		(outl,env)		= msgC_P "NSOutlineView\0" "alloc\0" env
		(outl,env)		= msgIS_P outl "initWithFrame:\0" NSRectType bounds1 env
		env				= msgII_V outl "setColumnAutoresizingStyle:\0" NSTableViewUniformColumnAutoresizingStyle env
		(_,env)			= object_setInstanceVariable delegate "outlineview\0" outl env
		
		(ocol1,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol1,env)		= msgIP_P ocol1 "initWithIdentifier:\0" (c2ns "columnOne\0") env
		env				= msgIR_V ocol1 "setWidth:\0" 5000.0 env
		(header,env)	= msgI_P ocol1 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "One\0") env

		env				= msgII_V ocol1 "setResizingMask:\0" NSTableColumnAutoresizingMask env
		env			 	= msgII_V ocol1 "setEditable:\0" NO env

		env				= msgIP_V outl "addTableColumn:\0" ocol1 env
		env				= msgIP_V outl "setOutlineTableColumn:\0"  ocol1 env
//		env				= msgI_V outl "sizeLastColumnToFit\0" env

		env				= msgII_V outl "setAutoresizesOutlineColumn:\0" NO env

	// set outl delegate & datasource...
		env				= msgIP_V outl "setDelegate:\0" delegate env		// ==> ahh.. probably arrives at app delegate by default so for proper view controller _do_ want to set
		env				= msgIP_V outl "setDataSource:\0" delegate env

	// set outl double-click response
	#!	(asel,env)		= sel_getUid "openIcl:\0" env
		env				= msgIP_V outl "setDoubleAction:\0" asel env
		(_,env)			= addContextmenu outl delegate env

		env				= msgIP_V scroll "setDocumentView:\0" outl env
		env				= msgIP_V container "addSubview:\0" scroll env
		env				= msgI_V scroll "release\0" env
//		env				= msgI_V outl "reloadData\0" env
//		env = msgI_V container "invalidateIntrinsicContentSize\0" env
//		env				= msgI_V outl "sizeLastColumnToFit\0" env
	= env

NSTableViewUniformColumnAutoresizingStyle	:== 1
NSTableColumnAutoresizingMask				:== 1
/*
[tableView  setColumnAutoresizingStyle:NSTableViewUniformColumnAutoresizingStyle];
[tableColumn setResizingMask:NSTableColumnAutoresizingMask];
[tableView sizeLastColumnToFit];
*/
addContextmenu outl delegate env
	#!	(menu,env) 	= msgC_P "NSMenu\0" "alloc\0" env
		(menu,env) 	= msgIP_P menu "initWithTitle:\0" (c2ns "ContexMenu\0") env
		(menu,env) 	= msgI_P menu "autorelease\0" env
		
		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz implementation" "openIcl:\0" "\n" env
		env			= msgIP_V item "setKeyEquivalentModifierMask:\0" (0) env
		env			= setTarget item delegate env

		(item,env)	= addItemWith_title_action_keyEquivalent menu "Open xyz definition" "openDcl:\0" "\n" env
		env			= msgIP_V item "setKeyEquivalentModifierMask:\0" (NSShiftKeyMask) env
		env			= setTarget item delegate env
		
		env			= msgIP_V outl "setMenu:\0" menu env
	= (menu,env)

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

