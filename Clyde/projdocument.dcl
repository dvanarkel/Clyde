definition module Clyde.projdocument

import Cocoa.objc

// document holds
// - document model content
// - read/write content
// - window controllers

createProjDocumentClass :: !*a -> *a		// create class ProjDocument and MyElement

// accessors for MyElement instances
lookStr :: !Pointer -> Pointer
lookPth :: !Pointer -> Pointer
lookIsGroup :: !Pointer -> Bool
lookNumChildren :: !Pointer -> Pointer
lookChild :: !Int !Pointer -> Pointer

// exports for foreign callbacks
initProjDocument			:: !ID !SEL -> VOID
readProjFromURL				:: !Pointer !Pointer !Pointer !Pointer -> Int
writeProjToURL				:: !Pointer !Pointer !Pointer !Pointer -> Int
makeProjWindowControllers	:: !ID !SEL -> VOID

Build		:: !ID !SEL !ID -> BOOL
BuildAndRun	:: !ID !SEL !ID -> BOOL
Run			:: !ID !SEL !ID -> BOOL

/*
 * store project in document
 * move project actions to projectdocument
 * move outlineview setup to projwindowcontroller
*/
