definition module PmDirCache

from StdMaybe			import :: Maybe
from StdFile			import :: Files
from UtilStrictLists	import :: List
from PmTypes			import :: Modulename
from StdPathname		import :: Pathname
from Directory			import :: DateTime, :: Date{..}, :: Time{..}

:: DirCache

:: Warn = Warn String String [(String,String,DateTime)]

DC_Setup	:: !(List Pathname) !*Files -> (!(![String],![Warn],!.DirCache),!*Files)
// Initialise directory cache

DC_Search	:: !Modulename !*DirCache -> *(!Bool,!Pathname,!DateTime,!*DirCache)
// Find file in directory cache
DC_HSearch :: !Modulename !String !*DirCache !*Files -> *(!Bool,!Pathname,!DateTime,!*DirCache,!*Files)

DC_Update	:: !(!String,!String,!DateTime) !*DirCache -> *DirCache
// Update directory cache
DC_HUpdate :: !(!String,!String,!DateTime) !String !*DirCache !*Files -> (!*DirCache,!*Files)

SearchDisk	:: !Modulename !(List Pathname) !*Files -> (!(!Bool,!Pathname),!*Files)
// Find file in paths (no caching)
FindHModule :: !Modulename !{#Char} !(List Pathname) !*Files -> (!(!Bool,!Pathname),!*Files)
