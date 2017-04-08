definition module Clyde.tableviewcontroller

// probably only used for unfinished profile viewer

from Cocoa.objc import :: Pointer, :: Class

tableViewControllerMethods :: [(!String,!Int,!String)]		// exported for reuse in application delegate...

//createTableViewController :: !Pointer !*World -> *World
//createPLView :: !Pointer !Pointer !*a -> *a

// for foreign export...
tableView_isGroupRow :: !Int !Int !Int !Int -> Int
tableView_shouldSelectRow :: !Int !Int !Int !Int -> Int
myCBHandler :: !Pointer !Pointer !Pointer !Pointer -> Int
numberOfRowsInTableView :: !Int !Int !Int -> Int
