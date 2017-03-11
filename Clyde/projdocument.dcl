definition module Clyde.projdocument

import Cocoa.objc

// document holds
// - document model content
// - read/write content
// - window controllers

createProjDocumentClass :: !*a -> *a

lookStr :: !Pointer -> Pointer
lookPth :: !Pointer -> Pointer
lookIsGroup :: !Pointer -> Bool
lookNumChildren :: !Pointer -> Pointer
lookChild :: !Int !Pointer -> Pointer

// exports for foreign callbacks
initProjDocument :: !Int !Int -> Int
readProjFromURL :: !Pointer !Pointer !Pointer !Pointer -> Int
writeProjToURL :: !Pointer !Pointer !Pointer !Pointer -> Int
makeProjWindowControllers :: !Int !Int -> Int

Build :: !Int !Int !Int -> Int
BuildAndRun :: !Int !Int !Int -> Int
Run :: !Int !Int !Int -> Int

/*
project :: !Int !Int !Int -> Int
*/



/*
 * store project in document
 * move project actions to projectdocument
 * move outlineview setup to projwindowcontroller


*/
