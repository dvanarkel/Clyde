definition module Clyde.menus

from StdEnv import <<

populateMainMenu :: !*World -> *World
populateApplicationMenu :: !Menu !*World -> *World
populateFileMenu :: !Menu !*World -> *World
populateEditMenu :: !Menu !*World -> *World
populateFindMenu :: !Menu !*World -> *World
populateSpellingMenu :: !Menu !*World -> *World
populateViewMenu :: !Menu !*World -> *World
populateWindowMenu :: !Menu !*World -> *World
populateHelpMenu :: !Menu !*World -> *World
populateDebugMenu :: !Menu !*World -> *World
populateProjectMenu :: !Menu !*World -> *World

// Cocoa menus...

from Cocoa.objc import :: Pointer, :: CString

:: Menu		:== Pointer		// pointer to NSMenu instance
:: MenuItem	:== Pointer		// pointer to NSMenuItem instance
:: Title	:== String
:: Action	:== CString		// still zero terminated for now...
:: Action`	:== Pointer		// pointer to ...
:: KeyEquivalent	:== String
:: Object	:== Pointer		// pointer to NSObject instance

NSAlphaShiftKeyMask         			:== 1 << 16
NSShiftKeyMask              			:== 1 << 17
NSControlKeyMask            			:== 1 << 18
NSAlternateKeyMask          			:== 1 << 19
NSCommandKeyMask            			:== 1 << 20
NSNumericPadKeyMask         			:== 1 << 21
NSHelpKeyMask               			:== 1 << 22
NSFunctionKeyMask           			:== 1 << 23
NSDeviceIndependentModifierFlagsMask    :== 0xffff0000

removeItemWithTitle :: !Menu !Title !*a -> *a

setMainMenu :: !Menu !*a -> (!Int,!*a)

setAppleMenu :: !Menu !*a -> *a
setWindowsMenu :: !Menu !*a -> *a

addMainmenu :: !*a -> (!Menu,!*a)
addSubmenu :: !Menu !String !*a -> (!Menu,!*a)
addSeparator :: !Menu !*a -> *a
addItemWith_title :: !Menu !Title !*a -> (!MenuItem,!*a)
addItemWith_title_keyEquivalent :: !Menu !Title !KeyEquivalent !*a -> (!MenuItem,!*a)
addItemWith_title_action_keyEquivalent :: !Menu !Title !Action !KeyEquivalent !*a -> (!MenuItem,!*a)
//addItemWithTitle_action_keyEquivalent :: !Menu !Title !Action` !KeyEquivalent !*a -> (!MenuItem,!*a)
//insertItemWith_title_action_keyEquivalent_atIndex :: !Menu !Title !Action !KeyEquivalent !Int !*a -> (!MenuItem,!*a)

removeItem :: !Menu !MenuItem !*a -> *a
removeItemAtIndex :: !Menu !Int !*a -> *a

itemWithTag :: !Menu !Int !*a -> (!MenuItem,!*a)
itemWithTitle :: !Menu !Title !*a -> (!MenuItem,!*a)
itemAtIndex :: !Menu !Int !*a -> (!MenuItem,!*a)

setTag :: !MenuItem !Int !*a -> *a
setTarget :: !MenuItem !Object !*a -> *a
