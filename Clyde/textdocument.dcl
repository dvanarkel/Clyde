definition module Clyde.textdocument

/* LINE NUMBERS...
http://www.noodlesoft.com/blog/2008/10/05/displaying-line-numbers-with-nstextview/
https://github.com/MrNoodle/NoodleKit/blob/master/NoodleLineNumberView.m
*/

from Cocoa.objc import :: Pointer

createTextDocument :: !*a -> *a					// create class TextDocument
textStorageDidProcess :: (!String,!Int,!String)	// implementation for textStorageDidProcessEditing:
setMyParagraphStyle :: !Int !Int !*a -> *a		// 


// exports for foreign callbacks
init_ :: !Int !Int -> Int
dealloc :: !Int !Int -> Int
myCBHandler2 :: !Pointer !Pointer !Pointer !Pointer -> Int
myCBHandler3 :: !Pointer !Pointer !Pointer !Pointer -> Int
//readFromURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
//writeToURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
makeWindowControllers :: !Int !Int -> Int
textStorageDidProcessEditing :: !Int !Int !Int -> Int
shouldShow :: !Int !Int !Int -> Int

