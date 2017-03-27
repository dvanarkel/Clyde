implementation module Clyde.Console

import StdEnv
import System._Unsafe
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Clyde.textdocument
import Clyde.textwindowcontroller

/*
 * build the window controller.. 
 * with implementations for
 *		hideConsoleWindow
 *		keyDown
 *		...
 */

createConsoleWindowControllerClass :: !*World -> *World
createConsoleWindowControllerClass env
	= createClass "NSWindowController" "ConsoleController" consoleMethods consoleIvars env

consoleMethods	=
	[ ("hideConsoleWindow:",					impHideC,						"v@:@\0")
	, ("keyDown:",								impKeyDown,						"v@:@\0")
	, ("didRead:",								impDidRead,						"v@:@\0")
	, textStorageDidProcess 
	]
consoleIvars	=
	[
	]

myconwindow =: accUnsafe consoleWindow

consoleWindow :: !*World -> (!Int,!*World)
consoleWindow env
	#!	(delegate,env)	= applicationDelegate env
		(wctrl,env)		= msgC_P "ConsoleController\0" "alloc\0" env
	| trace_n ("wctrl (0): "+++toString wctrl) False = undef
//		env				= msgIB_V wctrl "setShouldCloseDocument:\0" True env
	#!	(wind,env)		= populateTextWindow wctrl "Console" env		// send title as type so we can specialise background colour
		(but,env)		= msgII_P wind "standardWindowButton:\0" NSWindowCloseButton env
		env				= setAction but "hideConsoleWindow:\0" env
		env				= msgIP_V but "setTarget:\0" wctrl env
		env				= msgIP_V wind "setTitle:\0" (p2ns "Console") env
		(wctrl,env)		= msgIP_P wctrl "initWithWindow:\0" wind env

		(w,env)			= msgI_P wctrl "window\0" env
		(c,env)			= msgI_P wind "windowController\0" env
	| trace_n ("wind: "+++toString wind) False = undef
	| trace_n ("wctrl: "+++toString wctrl) False = undef
	| trace_n ("w: "+++toString w) False = undef
	| trace_n ("c: "+++toString c) False = undef
	#!	env	= msgI_V wctrl "retain\0" env
//		env
	= (wind,env)

NSWindowCloseButton	:== 0

strlen :: !Pointer -> Int
strlen _ = code {
		ccall strlen "P:I"
	}

pipe :: !Pointer -> Int
pipe _ = code {
		ccall pipe "P:I"
	}

con_stdin_pipe	=: accUnsafe (msgC_P "NSPipe\0" "pipe\0")
con_stdout_pipe	=: accUnsafe (msgC_P "NSPipe\0" "pipe\0")
con_stderr_pipe	=: accUnsafe (msgC_P "NSPipe\0" "pipe\0")

con_stdin_remote :: Int
con_stdin_remote	=: accUnsafe (getFileHandle con_stdin_pipe "fileHandleForReading\0")
con_stdout_remote :: Int
con_stdout_remote	=: accUnsafe (getFileHandle con_stdout_pipe "fileHandleForWriting\0")
con_stderr_remote :: Int
con_stderr_remote	=: accUnsafe (getFileHandle con_stderr_pipe "fileHandleForWriting\0")

getFileHandle pipe getter env
	#!	(nsfh,env)	= msgI_P pipe getter env
	= msgI_I nsfh "fileDescriptor\0" env

impKeyDown :: IMP
impKeyDown = code {
		pushLc keyDown
	}

foreign export keyDown

keyDown :: !Int !Int !Int -> Int
keyDown self cmd event
	| trace_n "keyDown:" False = undef
	#!	env				= newWorld
		(chrs,env)		= msgI_P event "characters\0" env
		(str,env)		= msgI_P chrs "UTF8String\0" env
		(data,env)		= msgCPI_P "NSData\0" "dataWithBytes:length:\0" str (strlen str) env
		(stdin,env)		= msgI_P con_stdin_pipe "fileHandleForWriting\0" env
		env				= msgIP_V stdin "writeData:\0" data env
	= force env NO

impDidRead :: IMP
impDidRead = code {
		pushLc didRead
	}

foreign export didRead

didRead :: !Int !Int !Int -> Int
didRead self cmd noty
	| trace_n "didRead:" False = undef
	#!	env				= newWorld
		(ui,env)		= msgI_P noty "userInfo\0" env
		(data,env)		= msgII_P ui "objectForKey:\0" NSFileHandleNotificationDataItem env
		(length,env)	= msgI_I data "length\0" env
	| length == 0
		= NO
	#!	(str,env)		= msgC_P "NSString\0" "alloc\0" env
		(str,env)		= msgIPP_P str "initWithData:encoding:\0" data NSUTF8StringEncoding env

		(cont,env)		= msgI_P myconwindow "contentView\0" env
		(txtv,env)		= msgI_P cont "documentView\0" env
		(stor,env)		= msgI_P txtv "textStorage\0" env
		(mstr,env)		= msgI_P stor "mutableString\0" env
		env				= msgIP_V mstr "appendString:\0" str env
		(len,env)		= msgI_I mstr "length\0" env
		env				= msgIII_V txtv "scrollRangeToVisible:\0" len 0 env

		env				= msgI_V str "release\0" env
		(obj,env)		= msgI_P noty "object\0" env
		env				= msgI_V obj "readInBackgroundAndNotify\0" env
	= force env NO

import Cocoa.dyncall
NSFileHandleNotificationDataItem :: Int
NSFileHandleNotificationDataItem
	=: dlsym -2 "NSFileHandleNotificationDataItem\0"
//	= code {
//		pushLc NSFileHandleNotificationDataItem
//	}

NSFileHandleReadCompletionNotification :: Int
NSFileHandleReadCompletionNotification
	= p2ns "NSFileHandleReadCompletionNotification"
NSFileHandleReadCompletionNotification` :: Int
NSFileHandleReadCompletionNotification`
	=: dlsym -2 "NSFileHandleReadCompletionNotification\0"
//	= code {
//		pushLc NSFileHandleReadCompletionNotification
//	}

NSUTF8StringEncoding :== 4

impHideC :: IMP
impHideC = code {
		pushLc doHideC
	}

foreign export doHideC

doHideC :: !Int !Int !Int -> Int
doHideC self cmd notification
	| trace_n "doHideC" False = undef
	#!	env				= newWorld
		env 			= msgII_V myconwindow "setIsVisible:\0" NO env
	= force env NO

/* setup console for notifications
    [[NSNotificationCenter defaultCenter]
                addObserver:self
                   selector:@selector(didRead:)
                       name:NSFileHandleReadCompletionNotification
                     object:[pty_ masterFileHandle]];

    [[pty_ masterFileHandle] readInBackgroundAndNotify];
*/
import StdDebug, StdMaybe, Clyde.Process
openConsoleWindow :: !String !*World -> *World
openConsoleWindow execpath env
	#!	(wctrl,env)		= msgI_P myconwindow "windowController\0" env
		(sel,env)		= sel_getUid "didRead:\0" env
		(stdout,env)	= msgI_P con_stdout_pipe "fileHandleForReading\0" env
	| trace_n ("conwindow: "+++toString myconwindow) False = undef
	| trace_n ("windowController: "+++toString wctrl) False = undef
	| trace_n ("stdout: "+++toString stdout) False = undef
	| trace_n ("stdout class: "+++ object_getClassName stdout) False = undef
	| trace_n ("sel: "+++toString sel) False = undef
	#!	(w,env)			= msgI_P wctrl "window\0" env
	| trace_n ("sanity window: "+++toString w) False = undef

	#!	(dnc,env)		= msgC_P "NSNotificationCenter\0" "defaultCenter\0" env
	| trace_n ("dnc: "+++toString dnc) False = undef
	#!	not1			= ns2cls NSFileHandleReadCompletionNotification
	| trace_n ("not1: '"+++not1+++"'\t"+++toString (size not1)) False = undef
	#!	not2			= ns2cls NSFileHandleReadCompletionNotification
	| trace_n ("not2: '"+++not2+++"'\t"+++toString (size not2)) False = undef
	| trace_n ("osno\t"+++toString wctrl+++"\t"+++toString sel+++"\t"+++toString NSFileHandleReadCompletionNotification+++"\t"+++toString stdout) False = undef
	#!	name	= 0// NSFileHandleReadCompletionNotification`	// causes crash in CFEqual when registering observer?!
		object	= stdout
	#!	env				= msgIPPII_V dnc "addObserver:selector:name:object:\0" wctrl sel name object env
	| trace_n ("xxx: "+++toString dnc) False = undef
	#!	env				= msgI_V stdout "readInBackgroundAndNotify\0" env
	| trace_n ("yyy: "+++toString dnc) False = undef

	#!	(app,env) 		= sharedApplication env
		(cont,env)		= msgI_P myconwindow "contentView\0" env
		(txtv,env)		= msgI_P cont "documentView\0" env
		(stor,env)		= msgI_P txtv "textStorage\0" env
		(mstr,env)		= msgI_P stor "mutableString\0" env
//		env				= msgIP_V mstr "setString:\0" (p2ns "") env
		env				= msgIP_V mstr "setString:\0" (c2ns "\0") env
		env				= msgIP_V myconwindow "makeKeyAndOrderFront:\0" app env
		env				= msgII_V myconwindow "setIsVisible:\0" YES env

	| trace_n ("about to redirect... ") False = undef
	#!	(res,env)		= runProcessWithRedirect execpath [] Nothing
									(Just con_stdin_remote) 
									(Just con_stdout_remote) 
									(Nothing)//(Just con_stdout_remote) 
//									(Just con_stderr_remote) 
									env	
	| trace_n ("redirect?" +++ toString (isOk res)) False = undef

	| close con_stdout_remote <> 0
		= abort "close failed"
	#!	(res,env)		= checkProcess (fromOk res) env
	| trace_n ("check?: " +++ checkOutcome res) False = undef
	= env
where
	checkOutcome (Ok (Nothing))	= "Running..."
	checkOutcome (Ok (Just c))	= "Exited: "+++toString c
	checkOutcome (Error (c,s))	= "Error: "+++toString c+++ "("+++s+++")"

appendConsoleWindow :: !String !*a -> *a
appendConsoleWindow message env
	#!	(cont,env)	= msgI_P myconwindow "contentView\0" env
		(txtv,env)	= msgI_P cont "documentView\0" env
		(stor,env)	= msgI_P txtv "textStorage\0" env
		(mstr,env)	= msgI_P stor "mutableString\0" env
		env			= msgIP_V mstr "appendString:\0" (p2ns (message+++"\n")) env
// scroll to visible...
		env			= msgIII_V txtv "scrollRangeToVisible:\0" (inc (size message)) 0 env
	= env

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

close :: !Int -> Int;
close fd = code {
	ccall close "I:I"
}

