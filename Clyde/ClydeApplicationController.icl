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

foreign export doTest

doTest :: !ID !SEL !ID -> BOOL
doTest self cmd notification
	| traceMsg "doTest" self cmd notification	= undef
	#!	(ret,world)		= testWind self newWorld
	= ret

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

