definition module Clyde.ClydeApplicationController

swizzleAboutPanel	:: !*a -> *a					// replace default About panel with our own

createAppDelegate	:: !*World -> *World			// create the AppDelegate class
initAppDelegate		:: !Int !*World -> *World		// attach AppDelegate to Application


openLogWindow		:: !*World -> *World			// clear and open the log window
appendLogWindow		:: !String !*a -> *a			// append to log window
openTypeWindow		:: !*World -> *World			// clear and open Types window
appendTypeWindow	:: !String !*a -> *a			// append to Types window

// exports for foreign...
from Cocoa.objc import :: ID, :: SEL, :: BOOL, :: Pointer

orderFrontStandardAboutPanel	:: !ID !SEL !ID -> BOOL
doHideT							:: !ID !SEL !ID -> BOOL
doHideL							:: !ID !SEL !ID -> BOOL
shouldOpenUntitledFile			:: !ID !SEL !ID -> BOOL
AppDelLogWindows				:: !ID !SEL !ID -> BOOL
AppDelDidFinishLaunching		:: !ID !SEL !ID -> BOOL
doTest							:: !ID !SEL !ID -> BOOL
doGetPath						:: !ID !SEL !ID -> BOOL
