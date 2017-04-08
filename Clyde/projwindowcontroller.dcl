definition module Clyde.projwindowcontroller

from Cocoa.objc import :: Pointer

makeProjWindowControllerClass :: !*a -> *a					// create class ProjWindowController
makeProjWindowController :: !*World -> (!Pointer,!*World)	// create and init instance of ProjWindowController

// exported for foreign export
cbHandlerPPPPP_P :: !Pointer !Pointer !Pointer !Pointer -> Int
outlineViewNummberOfChildrenOfItem :: !Int !Int !Int !Int -> Int
outlineViewIsItemExpandable :: !Int !Int !Int !Int -> Int
openIcl :: !Int !Int !Int -> Int
openDcl :: !Int !Int !Int -> Int
setDocument :: !Int !Int !Int -> Int
