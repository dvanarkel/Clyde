definition module logfile

import StdString, StdFile

openLogfile		:: !String !*f -> (!Bool,!*File,!*f)	| FileSystem f
closeLogfile	:: !*File !*f -> (!Bool,!*f)	| FileSystem f
writeLogfile	:: !String !*File -> *File
