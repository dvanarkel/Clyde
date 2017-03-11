implementation module StdPathname

import StdArray, StdBool, StdChar, StdClass, StdInt, StdString, StdList
import Platform

::	Pathname			:== String

EmptyPathname	:== ""

quoted_string :: !String -> String
quoted_string s = PlatformDependant (win_quoted_string s) (mac_quoted_string s)

win_quoted_string :: !String -> String
win_quoted_string string = "\"" +++ string +++ "\""
   
mac_quoted_string :: !String -> String
mac_quoted_string string = "\'" +++ double_quotes 0 string +++ "\'"
where
	double_quotes i string
		| i>=size string
			= string
		| string.[i]=='\''
			= double_quotes (i+2) (string % (0,i)+++"\'"+++string % (i+1,dec (size string)))
			= double_quotes (inc i) string

RemovePath	:: !Pathname -> String;
RemovePath path
	| found	= path % (inc position, last);
			= path;
	where 
		(found,position)= FindLastChar DirSeparator path last;
		last			= dec (size path);
	

RemoveSuffix :: !Pathname -> String;
RemoveSuffix path
	| not found		= path;
	| not suffix	= path;
					= path % (0, dec position);
	where 
		(found, position)	= FindLastChar '.' path last;
		suf					= path % (position, last);
//		suffix				= suf == ".dcl" || suf == ".icl" || suf == ".abc" || suf == ".o" || suf == ".obj" || suf == ".prj";
		suffix				= isMember suf [".",".dcl",".icl",".hs",".lhs",".abc",".o",".obj",".obj0",".obj1",".obj2",".xo",".cxo",".prj"];
		last				= dec (size path);

RemoveSuffix` :: !Pathname -> String;
RemoveSuffix` path
	| not found		= path;
//	| not suffix	= path;
					= path % (0, dec position);
	where 
		(found, position)	= FindLastChar '.' path last;
//		suf					= path % (position, last);
//		suffix				= suf == ".dcl" || suf == ".icl" || suf == ".abc" || suf == ".o" || suf == ".prj";
		last				= dec (size path);

RemoveFilename :: !Pathname -> Pathname;
RemoveFilename path 
	# (found,position)	= FindLastChar DirSeparator path (dec (size path));
	| found
		= path % (0, dec position);
		= path;

replace_prefix_path	:: !Pathname !Pathname !Pathname -> Pathname;
replace_prefix_path old_prefix_path new_prefix_path path
	| old_prefix_path==path
		= new_prefix_path;
	| equal_path_prefix old_prefix_path path
		= new_prefix_path +++ DirSeparatorString +++ path % (inc (size old_prefix_path), dec (size path));
		= path;

IsFullPathname :: !Pathname -> Bool;
IsFullPathname name = LastChar DirSeparator name (dec (size name)) >= 0;

MakeFullPathname :: !Pathname !String -> Pathname;
MakeFullPathname path name =  path +++ DirSeparatorString +++ name;

replace_dots_by_dir_separators :: !{#Char} -> *{#Char};
replace_dots_by_dir_separators module_name
	= {if (c=='.') DirSeparator c \\ c<-:module_name}

/* Auxilary functions */
	
equal_suffix	:: !String !String -> Bool;
equal_suffix suffix string
	| lengths < lengthsuf
		= False;
		= string % (lengths - lengthsuf,lengths) == suffix;
	where 
		lengths		= dec (size string);
		lengthsuf	= dec (size suffix);
	
	
equal_path_prefix	:: !String !String -> Bool;
equal_path_prefix prefix string
	| lengths<=lengthpre
		= False;
		= string.[inc lengthpre]==DirSeparator && string % (0,lengthpre)==prefix;
	where 
		lengths		= dec (size string);
		lengthpre	= dec (size prefix);
	
FindLastChar :: !Char !String !Int -> (!Bool, !Int);
FindLastChar c s i
	| i <= 0			= (False,0);
	| c ==  s.[i]		= (True, i);
						= FindLastChar c s (dec i);

LastChar :: !Char !String !Int -> Int;
LastChar c s i
	| i <= 0		= -1;
	| c ==  s.[i]	= i;
					= LastChar c s (dec i);

