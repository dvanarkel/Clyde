implementation module Cocoa.windowcontroller

import StdEnv
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Cocoa.viewcontroller

:: NSWindow				=: NSWindow Int
:: NSWindowController	=: NSWindowController Int
:: NSStoryboard	=: NSStoryboard Int
//:: NSCoder	=: NSCoder Int

// - (instancetype)initWithWindow:(NSWindow *)window;

initWithWindow :: !NSWindowController !NSWindow !*env -> (!NSWindowController,!*env)
initWithWindow (NSWindowController ctrl) (NSWindow window) env
	#	(r,env)	= msgIP_P ctrl "initWithWindow:\0" window env
	= (NSWindowController r,env)

// - (instancetype)initWithWindowNibName:(NSString *)windowNibName;

initWithWindowNibName :: !NSWindowController !String !*env -> (!NSWindowController,!*env)
initWithWindowNibName (NSWindowController ctrl) windowNibName env
	#	(r,env)	= msgIP_P ctrl "initWithWindowNibName:\0" (p2ns windowNibName) env
	= (NSWindowController r,env)

// - (instancetype)initWithWindowNibName:(NSString *)windowNibName owner:(id)owner;

initWithWindowNibNameOwner :: !NSWindowController !String !ID !*env -> (!NSWindowController,!*env)
initWithWindowNibNameOwner (NSWindowController ctrl) windowNibName owner env
	#	(r,env)	= msgIPP_P ctrl "initWithWindowNibName:owner:\0" (p2ns windowNibName) owner env
	= (NSWindowController r,env)

// - (instancetype)initWithWindowNibPath:(NSString *)windowNibPath owner:(id)owner;

initWithWindowNibPathOwner :: !NSWindowController !String !ID !*env -> (!NSWindowController,!*env)
initWithWindowNibPathOwner (NSWindowController ctrl) windowNibPath owner env
	#	(r,env)	= msgIPP_P ctrl "initWithWindowNibPath:owner:\0" (p2ns windowNibPath) owner env
	= (NSWindowController r,env)

// - (void)loadWindow;

loadWindow :: !NSWindowController !*env -> *env
loadWindow (NSWindowController ctrl) env
	= msgI_V ctrl "loadWindow\0" env

// - (IBAction)showWindow:(id)sender;

showWindow :: !NSWindowController !ID !*env -> *env
showWindow (NSWindowController ctrl) sender env
	= msgIP_V ctrl "showWindow:\0" sender env

// @property(getter=isWindowLoaded, readonly) BOOL windowLoaded;

isWindowLoaded :: !NSWindowController !*env -> (!Bool,!*env)
isWindowLoaded (NSWindowController ctrl) env
	#	(m,env)	= msgI_I ctrl "isWindowLoaded\0" env
	= (m<>0,env)

// @property(strong) NSWindow *window;

window (NSWindowController ctrl) env
	#	(w,env)	= msgI_P ctrl "window\0" env
	= (NSWindow w,env)

setWindow (NSWindowController ctrl) (NSWindow window) env
	= msgIP_V ctrl "setWindow:\0" window env

// - (void)windowDidLoad;		(notification)
// - (void)windowWillLoad;

// @property(assign) id document;

document :: !NSWindowController !*env -> (!ID,!*env)
document (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "document\0" env
	= (d,env)

setDocument :: !NSWindowController !ID !*env -> *env
setDocument (NSWindowController ctrl) document env
	= msgIP_V ctrl "setDocument:\0" document env

// - (void)setDocumentEdited:(BOOL)dirtyFlag;

setDocumentEdited :: !NSWindowController !Bool !*env -> *env
setDocumentEdited (NSWindowController ctrl) dirtyFlag env
	= msgII_V ctrl "setDocumentEdited:\0" (if dirtyFlag YES NO) env

// - (void)close;

close :: !NSWindowController !*env -> *env
close (NSWindowController ctrl) env
	= msgI_V ctrl "close\0" env

// @property BOOL shouldCloseDocument;

shouldCloseDocument :: !NSWindowController !*env -> (!Bool,!*env)
shouldCloseDocument (NSWindowController ctrl) env
	#	(d,env)	= msgI_I ctrl "shouldCloseDocument\0" env
	= (d<>0,env)

setShouldCloseDocument :: !NSWindowController !Bool !*env -> *env
setShouldCloseDocument (NSWindowController ctrl) should env
	= msgII_V ctrl "setShouldCloseDocument:\0" (if should YES NO) env

// @property(assign, readonly) id owner;

owner :: !NSWindowController !*env -> (!ID,!*env)
owner (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "owner\0" env
	= (d,env)

// @property(readonly, strong) NSStoryboard *storyboard;

storyboard :: !NSWindowController !*env -> (!NSStoryboard,!*env)
storyboard (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "storyboard\0" env
	= (NSStoryboard d,env)

// @property(copy, readonly) NSString *windowNibName;

windowNibName :: !NSWindowController !*env -> (!String,!*env)
windowNibName (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "windowNibName\0" env
	= (ns2cls d,env)

// @property(copy, readonly) NSString *windowNibPath;

windowNibPath :: !NSWindowController !*env -> (!String,!*env)
windowNibPath (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "windowNibPath\0" env
	= (ns2cls d,env)

// @property BOOL shouldCascadeWindows;

shouldCascadeWindows :: !NSWindowController !*env -> (!Bool,!*env)
shouldCascadeWindows (NSWindowController ctrl) env
	#	(d,env)	= msgI_I ctrl "shouldCascadeWindows\0" env
	= (d<>0,env)

setShouldCascadeWindows :: !NSWindowController !Bool !*env -> *env
setShouldCascadeWindows (NSWindowController ctrl) should env
	= msgII_V ctrl "setShouldCascadeWindows:\0" (if should YES NO) env

// @property(copy) NSString *windowFrameAutosaveName;

windowFrameAutosaveName :: !NSWindowController !*env -> (!String,!*env)
windowFrameAutosaveName (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "windowFrameAutosaveName\0" env
	= (ns2cls d,env)

setWindowFrameAutosaveName :: !NSWindowController !String !*env -> *env
setWindowFrameAutosaveName (NSWindowController ctrl) name env
	= msgIP_V ctrl "setWindowFrameAutosaveName:\0" (p2ns name) env

// - (void)synchronizeWindowTitleWithDocumentName;

synchronizeWindowTitleWithDocumentName :: !NSWindowController !*env -> *env
synchronizeWindowTitleWithDocumentName (NSWindowController ctrl) env
	= msgI_V ctrl "synchronizeWindowTitleWithDocumentName\0" env

// - (NSString *)windowTitleForDocumentDisplayName:(NSString *)displayName;

windowTitleForDocumentDisplayName :: !NSWindowController !*env -> (!String,!*env)
windowTitleForDocumentDisplayName (NSWindowController ctrl) env
	#	(s,env)	= msgI_P ctrl "windowTitleForDocumentDisplayName\0" env
	= (ns2cls s,env)

// @property(strong) NSViewController *contentViewController;

contentViewController :: !NSWindowController !*env -> (!NSViewController,!*env)
contentViewController (NSWindowController ctrl) env
	#	(d,env)	= msgI_P ctrl "contentViewController\0" env
	= (NSViewController d,env)

setContentViewController :: !NSWindowController !NSViewController !*env -> *env
setContentViewController (NSWindowController ctrl) (NSViewController view) env
	= msgIP_V ctrl "setContentViewController:\0" view env

// - (IBAction)dismissController:(id)sender;

dismissController :: !NSWindowController !ID !*env -> *env
dismissController (NSWindowController ctrl) sender env
	= msgIP_V ctrl "dismissController:\0" sender env

// - (instancetype)initWithCoder:(NSCoder *)coder;

initWithCoder :: !NSWindowController !NSCoder !*env -> *env
initWithCoder (NSWindowController ctrl) (NSCoder coder) env
	= msgIP_V ctrl "initWithCoder:\0" coder env
