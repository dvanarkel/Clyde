definition module Cocoa.toolbar

from Cocoa.objc import :: BOOL

makeTool :: !Int !*World -> *World

:: NSToolbar			=: NSToolbar Int
:: NSToolbarItem		=: NSToolbarItem Int
:: NSToolbarItemGroup	=: NSToolbarItemGroup Int
:: NSToolbarDelegate	=: NSToolbarDelegate Int

initWithIdentifier :: !NSToolbar !String !*env -> *env

insertItemWithItemIdentifierAtIndex :: !NSToolbar !String !Int !*env -> *env

visible :: !NSToolbar !*env -> (!BOOL,!*env)
setVisible :: !NSToolbar !BOOL !*env -> *env
