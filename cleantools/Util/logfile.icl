implementation module logfile

import StdFile, StdPathname, StdBool, StdString

openLogfile :: !String !*f -> (!Bool,!*File,!*f)	| FileSystem f
openLogfile prj_name env
	# log_name = RemoveSuffix prj_name +++. ".log"
	= fopen log_name FWriteText env

closeLogfile :: !*File !*f -> (!Bool,!*f)	| FileSystem f
closeLogfile file env
	# (ok1,file)	= ferror file
	# (ok2,env)		= fclose file env
	= (ok1 && ok2,env)

writeLogfile :: !String !*File -> *File
writeLogfile s file = fwrites (s+++."\n") file
