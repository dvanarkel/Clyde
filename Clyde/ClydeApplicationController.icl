implementation module Clyde.ClydeApplicationController

import StdEnv
import StdDebug

import System.Time, System._Unsafe
import Text

import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation

import Clyde.tableviewcontroller
import Clyde.textdocument
import Clyde.DebugClyde
import Clyde.projactions

swizzleAboutPanel :: !*a -> *a
swizzleAboutPanel world
	#!	(_,world)		= swizzleMethod "NSApplication" ("orderFrontStandardAboutPanel:", imp_orderFrontStandardAboutPanel,"i@:@\0") world
	= world

clydeInfoString home
	#	redirects	= join " " ["I/O isatty":map toString [stdin,stdout,stderr]]
	// would like to add start datetime of app...
	= concat [applicationName,"@",applicationPath,"\n\n",redirects,"\n","Started: ",startTime,"\n","CLEAN_HOME: ",home,"\n"]
where
	[stdin,stdout,stderr:_]	= ttyStandardDescriptors

startTime =: toString (accUnsafe localTime)
/*
#include <sys/sysctl.h>
#include <sys/types.h>

- (IBAction)printRunningTime:(id)sender
{
    pid_t pid = [[NSProcessInfo processInfo] processIdentifier];
    int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PID, pid };
    struct kinfo_proc proc;
    size_t size = sizeof(proc);
    sysctl(mib, 4, &proc, &size, NULL, 0);

    NSDate* startTime = [NSDate dateWithTimeIntervalSince1970:proc.kp_proc.p_starttime.tv_sec];
    NSLog(@"Process start time for PID:%d in UNIX time %@", pid, startTime);

    NSDateFormatter* dateFormatter = [[NSDateFormatter alloc] init];
    [dateFormatter setDateStyle:NSDateFormatterMediumStyle];
    [dateFormatter setTimeStyle:NSDateFormatterLongStyle];
    [dateFormatter setLocale:[NSLocale currentLocale]];
    NSLog(@"Process start time for PID:%d in local time %@", pid, [dateFormatter stringFromDate:startTime]);
*/

///// orderFrontStandardAboutPanel:

foreign export orderFrontStandardAboutPanel

imp_orderFrontStandardAboutPanel :: Int
imp_orderFrontStandardAboutPanel = code {
		pushLc orderFrontStandardAboutPanel
	}

orderFrontStandardAboutPanel :: !ID !SEL !ID -> BOOL
orderFrontStandardAboutPanel self cmd arg
	| trace_n ("entering orderFrontStandardAboutPanel") False = undef
	#!	env			= newWorld

		(dict,env)	= allocObject "NSMutableDictionary" env
		(dict,env)	= initObject dict env
		key			= p2ns "Credits"
		(home,env)	= cleanhome env
		credits		= p2ns (clydeInfoString home)
		(cras,env)	= allocObject "NSAttributedString" env
		(cras,env)	= msgIP_P cras "initWithString:\0" credits env
		(_,env)		= msgIPP_P dict "setObject:forKey:\0" cras key env	// Note: setObject:forKey: has void return

		(ns,env)	= msgIP_P dict "valueForKey:\0"  key env
//	| trace_n ("credits: "+++ ns2cls ns) False = undef	// can't go directly to ns2cls since it's an _attributed_ string

//	#!	env			= msgIP_V self "orderFrontStandardAboutPanelWithOptions:\0" 0 env	// NULL dict for default behaviour
	#!	env			= msgIP_V self "orderFrontStandardAboutPanelWithOptions:\0" dict env

		ret			= 0
	= force env ret

///////// class AppDelegate

initAppDelegate :: !Int !*World -> *World
initAppDelegate app world
	#!	(ado,world)		= allocObject "AppDelegate" world
		(ado,world) 	= initObject ado world
		world			= retain ado world
		world			= msgIP_V app "setDelegate:\0" ado world
	| trace_n ("app delegate: " +++ toString ado +++ "\n") False = undef
	= world

createAppDelegate :: !*World -> *World
createAppDelegate world
	#!	world			= force startTime world		// capture process start...
		world			= createClass "NSObject" "AppDelegate" appDelegateMethods [] world
	= world

appDelegateMethods	= 
	[ ("applicationDidFinishLaunching:",		impAppDelDidFinishLaunching,	"i@:@\0")
	, ("logwindows:", 							impAppDelLogWindows,			"v@:@\0")	// lying about return type...
	, ("applicationShouldOpenUntitledFile:",	imp_should,						"v@:@\0")	// lying about return type...
	, ("test:",									impTest,						"v@:@\0")
	, ("test2:",								impTest2,						"v@:@\0")
	, ("hideLogWindow:",						impHideL,						"v@:@\0")
	, ("hideTypeWindow:",						impHideT,						"v@:@\0")
	, ("getPath:",								impGetPath,						"v@:@\0")
	, textStorageDidProcess 
	: tableViewControllerMethods 
	]


import Clyde.textwindowcontroller

mylogwindow =: accUnsafe (swizzledTextWindow "Log" "hideLogWindow:\0")
mytypwindow =: accUnsafe (swizzledTextWindow "Types" "hideTypeWindow:\0")

swizzledTextWindow :: !String !String !*World -> (!Int,!*World)
swizzledTextWindow title close env
	#!	(delegate,env)	= applicationDelegate env
	#!	(wind,env)		= populateTextWindow delegate title env		// send title as type so we can specialise background colour
		(but,env)		= msgII_P wind "standardWindowButton:\0" NSWindowCloseButton env
		env				= setAction but close env
		env				= msgIP_V but "setTarget:\0" delegate env
		env				= msgIP_V wind "setTitle:\0" (p2ns title) env
	= (wind,env)

NSWindowCloseButton	:== 0

impHideT :: IMP
impHideT = code {
		pushLc doHideT
	}

impHideL :: IMP
impHideL = code {
		pushLc doHideL
	}

foreign export doHideT
foreign export doHideL

doHideL :: !ID !SEL !ID -> BOOL
doHideL self cmd notification
	= force (doHide mylogwindow newWorld) NO

doHideT :: !ID !SEL !ID -> BOOL
doHideT self cmd notification
	= force (doHide mytypwindow newWorld) NO

doHide :: !Int !*World -> *World
doHide window env
	#!	env = msgII_V window "setIsVisible:\0" NO env
	= env

openLogWindow :: !*World -> *World
openLogWindow  env
// clear contents
	#!	(app,env) 		= sharedApplication env
		(cont,env)		= msgI_P mylogwindow "contentView\0" env
		(txtv,env)		= msgI_P cont "documentView\0" env
		(stor,env)		= msgI_P txtv "textStorage\0" env
		(mstr,env)		= msgI_P stor "mutableString\0" env
		env				= msgIP_V mstr "setString:\0" (p2ns "") env
		env				= msgIP_V mylogwindow "makeKeyAndOrderFront:\0" application env
		env				= msgII_V mylogwindow "setIsVisible:\0" YES env
	= env

openTypeWindow :: !*World -> *World
openTypeWindow  env
// clear contents
	#!	(app,env) 		= sharedApplication env
		(cont,env)		= msgI_P mytypwindow "contentView\0" env
		(txtv,env)		= msgI_P cont "documentView\0" env
		(stor,env)		= msgI_P txtv "textStorage\0" env
		(mstr,env)		= msgI_P stor "mutableString\0" env
		env				= msgIP_V mstr "setString:\0" (p2ns "") env
		env				= msgIP_V mytypwindow "makeKeyAndOrderFront:\0" application env
		env				= msgII_V mytypwindow "setIsVisible:\0" YES env
	= env

appendLogWindow :: !String !*a -> *a
appendLogWindow message env
	#!	(cont,env)	= msgI_P mylogwindow "contentView\0" env
		(txtv,env)	= msgI_P cont "documentView\0" env
		(stor,env)	= msgI_P txtv "textStorage\0" env
		(mstr,env)	= msgI_P stor "mutableString\0" env
		env			= msgIP_V mstr "appendString:\0" (p2ns (message+++"\n")) env
// scroll to visible...
		env			= msgIII_V txtv "scrollRangeToVisible:\0" (inc (size message)) 0 env
	= env

appendTypeWindow :: !String !*a -> *a
appendTypeWindow message env
	#!	(cont,env)	= msgI_P mytypwindow "contentView\0" env
		(txtv,env)	= msgI_P cont "documentView\0" env
		(stor,env)	= msgI_P txtv "textStorage\0" env
		(mstr,env)	= msgI_P stor "mutableString\0" env
		env			= msgIP_V mstr "appendString:\0" (p2ns (message+++"\n")) env
// scroll to visible...
		env			= msgIII_V txtv "scrollRangeToVisible:\0" (inc (size message)) 0 env
	= env

// shouldOpenUntitledFile:

imp_should :: IMP
imp_should = code {
		pushLc shouldOpenUntitledFile
	}

foreign export shouldOpenUntitledFile

shouldOpenUntitledFile :: !ID !SEL !ID -> BOOL
shouldOpenUntitledFile self cmd notification
	= NO

// logwindows:

foreign export AppDelLogWindows

impAppDelLogWindows :: IMP
impAppDelLogWindows = code {
			pushLc 	AppDelLogWindows
		}

AppDelLogWindows :: !ID !SEL !ID -> BOOL
AppDelLogWindows self cmd notification
	| traceMsg "AppDelLogWindows" self cmd notification	= undef
	#!	(ret,world)		= callback newWorld
	= ret
where
	callback :: !*World -> (!Int,!*World)
	callback env
		#!	(app,env) 		= sharedApplication env
			(windows,env)	= msgI_P app "windows\0" env
			(count,env)		= count windows env
			env				= trace_n ("# windows\t"+++toString count) env
			env				= logwindows 0 count windows env
		= (0,env)
	
logwindows :: !Int !Int !Pointer !*a -> *a
logwindows index count windows env
	| index >= count
		#!	env	= printPools env
		= env
	#!	(window,env)		= objectAtIndex windows index env
		(titlez,env)		= msgI_P window "title\0" env
		tclass				= object_getClassName window
		(frame,env)			= getFrame window env
	| trace_n ("window#: "+++toString index) False = undef
	| trace_n ("\t"+++ns2cls titlez) False = undef
	| trace_n ("\t"+++tclass) False = undef
	| trace_n ("\t"+++rect2string frame) False = undef
	#!	(content,env)		= msgI_P window "contentView\0" env
		(frame,env)			= msgI_P content "superview\0" env
	| trace_n ("frame\t"+++ hex64 frame) False = undef
	| trace_n ("frame\t"+++object_getClassName frame) False = undef
	#!	env					= logviews 2 frame env
	| trace_n ("\t"+++ hex64 content) False = undef
	| trace_n ("\t"+++object_getClassName content) False = undef
	#!	env					= logviews 2 content env
	= logwindows (inc index) count windows env

logviews :: !Int !Pointer !*a -> *a
logviews nest view env
	| trace_n (indent +++. object_getClassName view) False = undef
	#!	(frame,env)		= getFrame view env
	| trace_n (indent +++. rect2string frame) False = undef
	#!	(subs,env)		= subviews view env
		(count,env)		= count subs env
	| trace_n (indent +++. "# subviews\t"+++toString count) False = undef
	#!	env				= logsubs 0 count subs env
	= env
where
	indent :: String
	indent	= createArray nest '\t'
	
	logsubs :: !Int !Int !Pointer !*a -> *a
	logsubs index count subs env
		| index >= count
			= env
		#!	(view,env)		= objectAtIndex subs index env
		| trace_n (indent +++. "# "+++. toString index +++. "\t" +++ hex64 view) False = undef
		#!	env		= logviews (inc nest) view env
		= logsubs (inc index) count subs env

printPools :: !*a -> *a
printPools env = snd (printPools env)
where
	printPools :: !*a -> (!Int,!*a)
	printPools _ = code {
			ccall _CFAutoreleasePoolPrintPools "G:I:A"
		}

// applicationDidFinishLaunching:

foreign export AppDelDidFinishLaunching

impAppDelDidFinishLaunching :: IMP
impAppDelDidFinishLaunching = code {
		pushLc 	AppDelDidFinishLaunching
	}

AppDelDidFinishLaunching :: !ID !SEL !ID -> BOOL
AppDelDidFinishLaunching self cmd notification
	| traceMsg "AppDelDidFinishLaunching" self cmd notification	= undef
	#!	(ret,world)		= callback newWorld
	= ret
where
	callback :: !*World -> (!Int,!*World)
	callback env
		#!	(ret,env)		= testWind self env
		= (YES,env)


// test:

import Clyde.controls
import Cocoa.layout
import Cocoa.tabview
import Cocoa.toolbar

impTest :: IMP
impTest = code {
		pushLc doTest
	}

impTest2 :: IMP
impTest2 = code {
		pushLc doTest2
	}

foreign export doTest

doTest :: !ID !SEL !ID -> BOOL
doTest self cmd notification
	| traceMsg "doTest" self cmd notification	= undef
	#!	(ret,world)		= testWind self newWorld
	= ret

foreign export doTest2

import Clyde.menus
doTest2 :: !ID !SEL !ID -> BOOL
doTest2 self cmd notification
	#	env			= newWorld
/*	| traceMsg "doTest" self cmd notification	= undef
	#	(menu,env)	= msgI_P application "mainMenu\0" env
		(bvis,env)	= msgC_P "NSMenu\0" "menuBarVisible\0" env
		(mbht,env)	= msgI_R menu "menuBarHeight\0" env
		(test,env)	= addSubmenu menu "Test" env
	| trace_n ("main: "+++hex64 menu) False = undef
	| trace_n ("bvis: "+++hex64 bvis) False = undef
	| trace_n ("mbht: "+++toString mbht) False = undef
	| trace_n ("test: "+++hex64 test) False = undef
*/
//addSubmenu :: !Menu !String !*a -> (!Menu,!*a)
//addItemWith_title :: !Menu !Title !*a -> (!MenuItem,!*a)
		env			= populateFourthWindow self env
	= force env YES

import Cocoa.toolbar
populateFourthWindow :: !Pointer !*World -> *World
populateFourthWindow self env
	#!	(wind,env)		= allocObject "NSWindow" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained
		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env

		(bounds1,env)	= contentLayoutRect wind env

		env				= msgIP_V wind "setTitle:\0" (c2ns "Profile Window\0") env

		(view,env)		= msgC_P "NSView\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 400.0 400.0
//		(cont_,env)		= msgI_P wind "contentView:\0" env
//		(rect,env)		= getBounds cont_ env
		(view,env)		= msgIS_P view "initWithFrame:\0" rectT rect env

//		(vw,env)		= msgI_P wind "contentView\0" env
//		(bounds1,env)	= getBounds vw env
//		env = trace_n ("root "+++toString wind+++"\t"+++toString bounds1) env

//		env				= createPLView self view env
//		env				= addDocumentVersionsButton view env
//		env				= makeTool wind env
//		env				= msgII_V wind "setShowsToolbarButton:\0" YES env
		(wctrl,env)		= allocObject "NSWindowController" env
		env				= msgIP_V wctrl "initWithWindow:\0" wind env
		env				= testWTab view wind wctrl env
		
//		env				= msgIP_V wind "setContentView:\0" view env
		(w,env)			= msgI_P wind "becomeFirstResponder\0" env
		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env
	= env

createBoxVC color env
	#	(ctrl,env)		= createBox` color env
		env				= msgII_V ctrl "setAutoresizingMask:\0" (NSViewWidthSizable + NSViewHeightSizable) env
		(vc,env)		= allocObject "NSViewController" env
		(vc,env)		= initObject vc env
		env				= msgIP_V vc "setView:\0" ctrl env
		env				= msgIP_V vc "setTitle:\0" (p2ns "my title") env
	= (vc,env)

testWTab :: !Int !Int !Int !*env -> *env
testWTab view wind wctrl env
	#	(tc,env)		= allocObject "NSTabViewController" env
		(tc,env)		= initObject tc env
		(NSTabView tv,env)	= tabView (NSTabViewController tc) env
		env				= msgIP_V wctrl "setContentViewController:\0" tc env

		env				= setTabStyle (NSTabViewController tc) NSTabViewControllerTabStyleToolbar env
		(vc,env)		= createBoxVC (0.2,0.0,0.2,1.0) env
		(ti,env)		= tabViewItemWithViewController vc env
		env				= setIdentifier ti "tab 1" env

		env				= addTabViewItem (NSTabViewController tc) ti env
		(vc,env)		= createBoxVC (1.0,1.0,0.0,0.1) env
		(ti,env)		= tabViewItemWithViewController vc env
		env				= setIdentifier ti "tab 2" env
		env				= addTabViewItem (NSTabViewController tc) ti env
	= env

setIdentifier (NSTabViewItem tab) identifier env
	= msgIP_V tab "setIdentifier:\0" (p2ns identifier) env

setTitle (NSTabViewItem tab) identifier env
	= msgIP_V tab "setTitle:\0" (p2ns identifier) env

import Cocoa.Foundation, System._Pointer, System._Posix
findView name view env
	#!	(subs,env)		= msgI_P view "subviews\0" env
		(count,env)		= msgI_I subs "count\0" env
	= logsubs 0 count subs env
where
	logsubs :: !Int !Int !Pointer !*a -> (!Pointer,!*a)
	logsubs index count subs env
		| index >= count
			= (0,env)
		#!	(view,env)		= msgII_P subs "objectAtIndex:\0" index env
		| object_getClassName view == name
			= (view,env)
//		#!	env				= logviews (inc nest) view env
		= logsubs (inc index) count subs env

import Clyde.timeprofile

createPLView :: !Pointer !Pointer !*a -> *a
createPLView delegate container env
	#!	(bounds,env)	= getBounds container env
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

		(outl,env)		= msgC_P "NSTableView\0" "alloc\0" env
		(outl,env)		= msgIS_P outl "initWithFrame:\0" NSRectType bounds1 env
		
		env				= msgII_V outl "setUsesAlternatingRowBackgroundColors:\0" YES env
// - (void)setUsesAlternatingRowBackgroundColors:(BOOL)useAlternatingRowColors; // FOR Profile viewers...

/*
		(ocol1,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol1,env)		= msgIP_P ocol1 "initWithIdentifier:\0" (c2ns "columnOne\0") env
		(header,env)	= msgI_P ocol1 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "One\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env				// can we set a min width that propagates upwards?
		env				= msgIP_V outl "addTableColumn:\0" ocol1 env

		(ocol2,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol2,env)		= msgIP_P ocol2 "initWithIdentifier:\0" (c2ns "columnTwo\0") env
		(header,env)	= msgI_P ocol2 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "Two\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env
		env				= msgIP_V outl "addTableColumn:\0" ocol2 env
*/		
//		env				= addTableColumn outl "columnOne" "One" env
//		env				= addTableColumn outl "columnTwo" "Two" env
		env				= addColumnHeaders outl headerline env
		
	// set outl delegate & datasource...
		env				= msgIP_V outl "setDelegate:\0" delegate env		// ==> ahh.. probably arrives at app delegate by default so for proper view controller _do_ want to set
		env				= msgIP_V outl "setDataSource:\0" delegate env

		env				= msgIP_V scroll "setDocumentView:\0" outl env
		
		env				= msgIP_V container "addSubview:\0" scroll env
		env				= msgI_V scroll "release\0" env
		env				= msgI_V outl "reloadData\0" env
	= env

addColumnHeaders table [] env	= env
addColumnHeaders table [col:cols] env
	#!	env				= addTableColumn table col col env
	= addColumnHeaders table cols env

addTableColumn table col_identifier col_label env
	#!	(tcol,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(tcol,env)		= msgIP_P tcol "initWithIdentifier:\0" (p2ns col_identifier) env
		(header,env)	= msgI_P tcol "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (p2ns col_label) env
//		env				= msgIR_V tcol "setWidth:\0" 200.0 env				// can we set a min width that propagates upwards?
		env				= msgIP_V table "addTableColumn:\0" tcol env
	= env

NSWindowDocumentVersionsButton = 6

addDocumentVersionsButton view env
	#!	(vbut,env)		= msgCII_P "NSWindow\0" "standardWindowButton:forStyleMask:\0" NSWindowDocumentVersionsButton 0 env
		(titb,env)		= msgI_P view "superview\0" env
		(tcont,env)		= findView "NSTitlebarContainerView" titb env
		(ttitb,env)		= findView "NSTitlebarView" tcont env
		(ttext,env)		= findView "NSTextField" ttitb env
		(bounds,env)	= getBounds ttext env
		origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
//		env				= msgIRR_V vbut "setFrameOrigin:" (origin_x + size_w) origin_y env
		env				= msgIRR_V vbut "setFrameOrigin:" (459.0 + 124.0) 3.0 env
// fiddling with frame...
//		env				= msgIP_V titb "_addKnownSubview:\0" vbut env
		env				= msgIP_V ttitb "addSubview:\0" vbut env
	= env

contentLayoutRect :: !Pointer !*World -> (!Pointer,!*World)
contentLayoutRect self env
	#!	(sel,env)		= sel_getUid "contentLayoutRect\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

readRect :: !Pointer !*a -> (!(!Real,!Real),!(!Real,!Real),!*a)
readRect bounds env
	#!	origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
	= ((origin_x,origin_y),(size_w,size_h),env)

visibleFrame :: !Pointer !*a -> (!Pointer,!*a)
visibleFrame self env
	#!	(sel,env)		= sel_getUid "visibleFrame\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
//		env = trace_n ("getFrame\t"+++toString self+++"\t"+++toString sel+++"\t"+++toString ptr+++"\t"+++toString ptr`) env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

/*
path button & path list
tab control
*/
testWind :: !Int !*World -> (!Int,!*World)
testWind self env
	#!	(wind,env)		= makeWind "Test project options" env
//		env				= msgII_V wind "setTranslatesAutoresizingMaskIntoConstraints:\0" NO env
		(view,env)		= makeView env
		env				= msgII_V view "setTranslatesAutoresizingMaskIntoConstraints:\0" NO env
		env				= msgIP_V wind "setContentView:\0" view env

		env				= makeTool wind env

		top				= 460.0

		(ctrl,env)		= createButton "Test" "test2:\0" env
		env				= msgIRR_V ctrl "setFrameOrigin:\0" 200.0 (top - 40.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env
/*
		(ctrl,env)		= createCheckbox "My checkbox" "null:\0" env
		env				= msgIRR_V ctrl "setFrameOrigin:\0" 10.0 (top - 20.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createCheckbox "My checkbox" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 40.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createCheckbox "My checkbox" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 60.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createRadioButton "Button 1" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 90.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createRadioButton "Button 2" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 110.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createRadioButton "Button 3" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 130.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createTextField "a textfield" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 160.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createTextField "another textfield" "null:\0" env
		env				= setFrameOrigin ctrl 10.0 (top - 190.0) env
		env				= msgII_V ctrl "setEditable:\0" NO env
		env				= msgIP_V view "addSubview:\0" ctrl env

		(ctrl,env)		= createButton "Set path" "getPath:\0" env
		env				= setFrameOrigin ctrl 140.0 (top - 200.0) env
		env				= msgIP_V view "addSubview:\0" ctrl env
*/
		(ctrl,env)		= createBox (5.0,5.0,0.0,0.5) env
		env				= msgIP_V view "addSubview:\0" ctrl env
		env				= setConstraints view ctrl env
//		env	= msgI_V view "layoutSubtreeIfNeeded\0" env
//		env = debugConstraints wind view env

		(w,env)			= msgI_P wind "becomeFirstResponder\0" env
		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env
	| trace_n (hex32 wind+++"\t"+++hex32 view+++"\t"+++hex32 ctrl) False = undef
	= (YES,env)

debugConstraints wind view env
	#!	(constraints,env)	= msgI_P view "constraints\0" env
		env					= msgIP_V wind "visualizeConstraints:\0" constraints env

//		env					= msgI_V view "subtreeDescription\0" env
	= env

//debugLayout view orientation env	// 0 = horizontal, 1 = vertical
//	#	(arr,env)	= msgII_P view "constraintsAffectingLayoutForOrientation:\0" orientation env

//setCV view env
setConstraints :: !Int !Int !*env -> *env	// inlining this caused compiler to crash
setConstraints view ctrl env
	#	view` = NSView view
		ctrl` = NSView ctrl
		(wc,env)		= constraintEqualToConstant (widthAnchor ctrl`) 200.0 env
		(hc,env)		= constraintEqualToConstant (heightAnchor ctrl`) 200.0 env
		(lc,env)		= constraintEqualToAnchorConstant (leftAnchor ctrl`) (leftAnchor view`) 100.0 env
		(tc,env)		= constraintEqualToAnchorConstant (topAnchor ctrl`) (topAnchor view`) 100.0 env
		env				= activate lc env
		env				= activate wc env
		env				= activate tc env
		env				= activate hc env

		env	= msgII_V ctrl "setNeedsUpdateConstraints:\0" YES env

		(NSLayoutConstraint wc`)	= wc
	#	(ns,env)	= msgI_P wc` "description\0" env
	| trace_n ("desc: "+++ns2cls ns) False = undef
	= env
where
	object_getInstanceVariable` :: !ID !CString !*a -> (!Int,!Int,!*a)
	object_getInstanceVariable` obj name env = code {
			ccall object_getInstanceVariable "ps:pp:A"
		}

makeWind title env
	#!	(wind,env)		= allocObject "NSWindow" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained
		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env
		env				= msgIP_V wind "setTitle:\0" (p2ns title) env
	= (wind,env)

makeView env
	#!	(view,env)		= allocObject "NSView" env
		rect			= cgRect 0.0 0.0 320.0 480.0			// TODO: free...
		(view,env)		= initWithFrame view rect env
	= (view,env)

zeroRect	= cgRect 0.0 0.0 0.0 0.0

createTextField :: !String !ZString !*a -> (!NSControl,!*a)
createTextField content action env
	#!	(but,env)		= allocObject "NSTextField" env
		(but,env)		= initWithFrame but zeroRect env
//		env				= setButtonType but NSRadioButton env
//		env				= setBezelStyle but NSRoundedBezelStyle env
//		env				= setTitle but title env			// no set title on an nstextfield!
//		env				= setAction but action env
		env				= msgIP_V but "setStringValue:\0" (p2ns content) env
		env				= sizeToFit but env
		env				= setFrameOrigin but 10.0 10.0 env
	= (but,env)

createBox :: (Real,Real,Real,Real) *a -> (!NSControl,!*a)
createBox (r,g,b,a) env
	#!	(ctrl,env)		= allocObject "NSBox" env
		rect			= cgRect 0.0 0.0 200.0 200.0			// TODO: free...
		(ctrl,env)		= initWithFrame ctrl rect env
//		env				= msgI_V ctrl "init\0" env
		env				= msgII_V ctrl "setTranslatesAutoresizingMaskIntoConstraints:\0" NO env
		env				= msgII_V ctrl "setBoxType:\0" 4 env	// custom
		env				= msgIR_V ctrl "setBorderWidth:\0" 0.0 env
		(col,env)		= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" r g b a env
//		env				= msgI_V col "retain\0" env
		env				= msgIP_V ctrl "setFillColor:\0" col env
	= (ctrl,env)

createBox` :: (Real,Real,Real,Real) *a -> (!NSControl,!*a)
createBox` (r,g,b,a) env
	#!	(ctrl,env)		= allocObject "NSBox" env
		rect			= cgRect 0.0 0.0 200.0 200.0			// TODO: free...
		(ctrl,env)		= initWithFrame ctrl rect env
//		env				= msgI_V ctrl "init\0" env
//		env				= msgII_V ctrl "setTranslatesAutoresizingMaskIntoConstraints:\0" NO env
		env				= msgII_V ctrl "setBoxType:\0" 4 env	// custom
		env				= msgIR_V ctrl "setBorderWidth:\0" 0.0 env
		(col,env)		= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" r g b a env
//		env				= msgI_V col "retain\0" env
		env				= msgIP_V ctrl "setFillColor:\0" col env
	= (ctrl,env)

//

impGetPath :: IMP
impGetPath = code {
		pushLc doGetPath
	}

foreign export doGetPath

doGetPath :: !ID !SEL !ID -> BOOL
doGetPath self cmd notification
	| traceMsg "doGetPath" self cmd notification	= undef
	#!	(ret,world)		= getPath self newWorld
	= ret

getPath :: !Int !*World -> (!Int,!*World)
getPath self env
	#!	(pane,env)		= allocObject "NSOpenPanel" env
		env				= msgI_V pane "init\0" env
		env				= msgII_V pane "setCanChooseDirectories:\0" YES env
		env				= msgII_V pane "setCanChooseFiles:\0" NO env
		env				= msgII_V pane "setAllowsMultipleSelection:\0" YES env
		env				= msgI_V pane "runModal\0" env

		(urls,env)		= msgI_P pane "URLs\0" env
	| trace_n ("URLs: "+++hex32 urls) False = undef
	= (YES,env)

/// hex

// Show an integer as an 8-digit hexadecimal number with leading zeroes.

hex64 :: !Int -> String
hex64 i =
    let w :: Int
        w = i //bitand 0xffffff
        s = showHex w ""
    in createArray (16 - size s) '0' +++ s

hex32 :: !Int -> String
hex32 i =
    let w :: Int
//        w = fromIntegral i
        w = i bitand 0xffffff
        s = showHex w ""
//    in take (8 - length s) (repeat '0') +++ s
    in createArray (8 - size s) '0' +++ s

hex16 :: !Int -> String
hex16 i =
    let w :: Int
        w = i bitand 0xffff
        s = showHex w ""
    in createArray (4 - size s) '0' +++ s

// Show a byte as a 2-digit hexadecimal number with leading zeroes.

hex8 :: !Int -> String
hex8 i =
    let s = showHex i ""
//    in take (2 - length s) ['0','0'] +++ s
    in createArray (2 - size s) '0' +++ s

showHex 0 s	= "0" +++ s
showHex i s
	| i < 0
//		| trace_n ("showHex huh? "+++toString i) False = undef
//		= "!!-1!!"+++s
		= abort ("\n!!-1!!"+++s+++"\n\n")
	# nibs	= reverse (nibbleIt i)
	= {# showNibble n \\ n <- nibs} +++ s
//	= abort "showHex undefined\n"

nibbleIt i
	| i < 0		= abort ("nibbleIt huh? "+++toString i+++"\n\n")
	| i == 0	= []
	# n	= i bitand 0xF
	  i = i >> 4
	= [n:nibbleIt i]
	
showNibble 0	= '0'
showNibble 1	= '1'
showNibble 2	= '2'
showNibble 3	= '3'
showNibble 4	= '4'
showNibble 5	= '5'
showNibble 6	= '6'
showNibble 7	= '7'
showNibble 8	= '8'
showNibble 9	= '9'
showNibble 10	= 'a'
showNibble 11	= 'b'
showNibble 12	= 'c'
showNibble 13	= 'd'
showNibble 14	= 'e'
showNibble 15	= 'f'

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

