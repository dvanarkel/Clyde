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

orderFrontStandardAboutPanel :: !Int !Int !Int -> Int
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
/*
		(sel,world)		= sel_getUid "build:\0" world
		(ok,world)		= class_addMethod adc sel impBuild "v@:@\0" world		// lying about return type here...

		(sel,world)		= sel_getUid "buildAndRun:\0" world
		(ok,world)		= class_addMethod adc sel impBuildAndRun "v@:@\0" world		// lying about return type here...

		(sel,world)		= sel_getUid "run:\0" world
		(ok,world)		= class_addMethod adc sel impRun "v@:@\0" world		// lying about return type here...

		(sel,world)		= sel_getUid "project:\0" world
		(ok,world)		= class_addMethod adc sel imp_project "v@:@\0" world		// lying about return type here...
*/
	#!	world			= force startTime world		// capture process start...
//		world			= createClass` "NSObject" "AppDelegate" appDelegateMethods world
		world			= createClass "NSObject" "AppDelegate" appDelegateMethods [] world
	= world

appDelegateMethods	= 
	[ ("applicationDidFinishLaunching:",		impAppDelDidFinishLaunching,	"i@:@\0")
	, ("logwindows:", 							impAppDelLogWindows,			"v@:@\0")	// lying about return type...
	, ("applicationShouldOpenUntitledFile:",	imp_should,						"v@:@\0")	// lying about return type...
	, ("test:",									impTest,						"v@:@\0")
	, ("hideLogWindow:",						impHideL,						"v@:@\0")
	, ("hideTypeWindow:",						impHideT,						"v@:@\0")
	, textStorageDidProcess 
	: tableViewControllerMethods 
	]


// test:

impTest :: IMP
impTest = code {
		pushLc doTest
	}

foreign export doTest

doTest :: !Int !Int !Int -> Int
doTest self cmd notification
	| traceMsg "doTest" self cmd notification	= undef
	#!	(ret,world)		= callback newWorld
	= ret
where
	callback :: !*World -> (!Int,!*World)
	callback env
		#!	env = msgII_V mylogwindow "setIsVisible:\0" YES env
		= (NO,env)

import Clyde.textwindowcontroller

mylogwindow =: accUnsafe (swizzledTextWindow "Log")
mytypwindow =: accUnsafe (swizzledTextWindow "Types")

swizzledTextWindow :: !String !*World -> (!Int,!*World)
swizzledTextWindow title env
	#!	(delegate,env)	= applicationDelegate env
	#!	(wind,env)	= populateTextWindow delegate "" env
		(but,env)	= msgII_P wind "standardWindowButton:\0" NSWindowCloseButton env
		env			= setAction but "hideLogWindow:\0" env
		env			= msgIP_V but "setTarget:\0" delegate env
		env			= msgIP_V wind "setTitle:\0" (p2ns title) env
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

doHideL :: !Int !Int !Int -> Int
doHideL self cmd notification
	= force (doHide mylogwindow newWorld) NO

doHideT :: !Int !Int !Int -> Int
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
		env				= msgIP_V stor "setString:\0" (p2ns "") env
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
		env				= msgIP_V stor "setString:\0" (p2ns "") env
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

shouldOpenUntitledFile :: !Int !Int !Int -> Int
shouldOpenUntitledFile self cmd notification
	= NO

// logwindows:

foreign export AppDelLogWindows

impAppDelLogWindows :: IMP
impAppDelLogWindows = code {
			pushLc 	AppDelLogWindows
		}

AppDelLogWindows :: !Int !Int !Int -> Int
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
			env	= trace_n ("# windows\t"+++toString count) env
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
	| trace_n ("frame\t"+++toString frame) False = undef
	| trace_n ("frame\t"+++object_getClassName frame) False = undef
	#!	env					= logviews 2 frame env
	| trace_n ("\t"+++toString content) False = undef
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
		| trace_n (indent +++. "# "+++. toString index +++. "\t" +++ toString view) False = undef
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

//AppDelDidFinishLaunching :: !ID !SEL !ID -> BOOL
AppDelDidFinishLaunching :: !Int !Int !Int -> Int
AppDelDidFinishLaunching self cmd notification
	| traceMsg "AppDelDidFinishLaunching" self cmd notification	= undef
	#!	(ret,world)		= callback newWorld
	= ret
where
	callback :: !*World -> (!Int,!*World)
	callback env
//		#!	env				= populateMainMenu env
//			env				= trace_n "populateMainMenu done" env
/*		#!	env				= populateWindow self env
			env				= trace_n "populateWindow done" env
			env				= populateSecondWindow self env
			env				= trace_n "populateSecondWindow done" env
			env				= populateThirdWindow self env
			env				= trace_n "populateThirdWindow done" env
			env				= populateFourthWindow self env
			env				= trace_n "populateFourthWindow done" env
*/		= (1,env)	// YES


///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

