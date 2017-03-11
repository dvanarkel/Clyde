definition module Clyde.outlineviewcontroller

xxx

from Cocoa.objc import :: Pointer, :: Class

elemClass :: !*a -> *a

addOVDataSource :: !Class !*a -> *a
initClydeTree :: !*a -> *a


// now we need a document type?
initProjectTree :: !String !String !String ![String] !*a -> *a
/*
	proj		= "/Clyde.prj"
	prjPath		= "/Users/dvanarkelmaccom/Documents/CleanLab"
	appPath		= "/usr/local/Cellar/clean-itasks/20151022"
	envPaths	= ["/usr/local/Cellar/clean-itasks/20151022/lib/StdEnv"]		// get from IDEEnvs...
*/

:: TreeElement
treeElems :: {#Int}
treeItems :: {TreeElement}

//traceItems :: String

createPOView :: !Pointer !Pointer !*a -> *a

// exported for foreign export
cbHandlerPPPPP_P :: !Pointer !Pointer !Pointer !Pointer -> Int
outlineViewNummberOfChildrenOfItem :: !Int !Int !Int !Int -> Int
outlineViewIsItemExpandable :: !Int !Int !Int !Int -> Int
openIcl :: !Int !Int !Int -> Int
openDcl :: !Int !Int !Int -> Int
