implementation module PmPath

import StdClass,StdString, StdChar, StdBool, StdChar,StdInt, StdMisc,StdArray;
import StdPathname

import PmTypes
import Platform
import UtilStrictLists

/* The name of the system directory */
SystemDir			:== "Clean System Files";

//--

IsDefPathname :: !Pathname -> Bool;
IsDefPathname name =  equal_suffix ".dcl" name;

IsImpPathname :: !Pathname -> Bool;
IsImpPathname name =  equal_suffix ".icl" name;
	
IsABCPathname :: !Pathname -> Bool;
IsABCPathname name =  equal_suffix ".abc" name;
	
IsPrjPathname :: !Pathname -> Bool;
IsPrjPathname name =  equal_suffix ".prj" name;

MakeDefPathname	:: !String -> Pathname;
MakeDefPathname name =  RemoveSuffix name  +++ ".dcl";

MakeImpPathname	:: !String -> Pathname;
MakeImpPathname name = RemoveSuffix name  +++ ".icl";
			
MakeABCPathname	:: !String -> Pathname;
MakeABCPathname name = RemoveSuffix name  +++ ".abc";
	
MakeObjPathname	:: !Processor !String -> Pathname;
MakeObjPathname processor name
	= RemoveSuffix name +++ ProcessorSuffix processor

MakeProjectPathname	:: !String -> Pathname;
MakeProjectPathname name = RemoveSuffix name   +++ ".prj";

MakeExecPathname :: !String -> Pathname;
MakeExecPathname name
	= PlatformDependant
		(RemoveSuffix name+++".exe")	// Win
		(RemoveSuffix name)				// Mac

MakeSystemPathname :: !Pathname -> Pathname;
MakeSystemPathname pathname
	| equal_suffix SystemDir pathname
		= pathname
	| size pathname > 0 && pathname.[size pathname - 1] == DirSeparator
		= pathname +++ SystemDir
	| otherwise
		= pathname +++ sep +++ SystemDir;
where
	sep = toString DirSeparator;

ModuleDirAndNameToImpPathname :: !ModuleDirAndName -> Pathname
ModuleDirAndNameToImpPathname {mdn_dir,mdn_name}
	= MakeFullPathname mdn_dir (replace_dot_chars_by_dir_separators mdn_name +++ ".icl")

ModuleDirAndNameToDefPathname :: !ModuleDirAndName -> Pathname
ModuleDirAndNameToDefPathname {mdn_dir,mdn_name}
	= MakeFullPathname mdn_dir (replace_dot_chars_by_dir_separators mdn_name +++ ".dcl")

replace_dot_chars_by_dir_separators :: !{#Char} -> {#Char}
replace_dot_chars_by_dir_separators module_name
	# last_dot_i=find_last_dot_i module_name
	| last_dot_i<0
		= module_name
		= {if (c=='.') DirSeparator c \\ c<-:module_name & i<-[0..]}

ModuleDirAndNameToABCSystemPathname :: !ModuleDirAndName -> Pathname
ModuleDirAndNameToABCSystemPathname {mdn_dir,mdn_name}
	= make_clean_system_files_path mdn_dir mdn_name ".abc"

ModuleDirAndNameToObjSystemPathname :: !Processor !ModuleDirAndName -> Pathname
ModuleDirAndNameToObjSystemPathname processor {mdn_dir,mdn_name}
	= make_clean_system_files_path mdn_dir mdn_name (ProcessorSuffix processor)

make_clean_system_files_path dir module_name file_ext
	# last_dot_i=find_last_dot_i module_name
	| last_dot_i<0
		= directory_name_plus_system_dir_from_dir dir+++DirSeparatorString+++module_name+++file_ext
			with
				directory_name_plus_system_dir_from_dir dir
					| equal_suffix SystemDir dir
						= dir;
					| size dir>0 && dir.[size dir-1]==DirSeparator
						= dir +++ SystemDir;
					| otherwise
						= dir +++ DirSeparatorString +++ SystemDir;
		# (subdir,file_name) = subdir_and_file_name_of_hmodule_name last_dot_i module_name file_ext
		= directory_name_without_system_dir_with_separator_from_dir dir+++subdir+++DirSeparatorString+++SystemDir+++DirSeparatorString+++file_name
			with
				directory_name_without_system_dir_with_separator_from_dir dir
					| equal_suffix SystemDir dir
						= dir % (0,size dir-1-size SystemDir)+++DirSeparatorString
					| size dir>0 && dir.[size dir-1]==DirSeparator
						= dir
					| otherwise
						= dir+++DirSeparatorString;

find_last_dot_i :: !{#Char} -> Int
find_last_dot_i s
	= find_last_dot_i s (size s-1)
where
	find_last_dot_i s i
		| i>=0
			| s.[i]=='.'
				= i
				= find_last_dot_i s (i-1)
			= i

subdir_and_file_name_of_hmodule_name :: !Int !{#Char} !{#Char} -> (!*{#Char},!{#Char})
subdir_and_file_name_of_hmodule_name last_dot_i mod file_ext
	# subdir_name = {if (c=='.') DirSeparator c \\ i<-[0..last_dot_i-1] & c<-:mod}
	  file_name = mod % (last_dot_i+1,size mod-1)+++file_ext
	= (subdir_name,file_name)

MakeAssemblySystemPathname :: !Pathname -> Pathname
MakeAssemblySystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ suffix
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix abcname);
		suffix	= PlatformDependant
					".s"	// Win
					".a"	// Mac

GetModuleName :: !Pathname -> Modulename;
GetModuleName name =  RemoveSuffix (RemovePath name);

determine_dir_and_filename :: !{#Char} !(List {#Char}) -> (!ModuleDirAndName,!{#Char})
determine_dir_and_filename imp_pathname dirs
	= determine_dir_and_filename dirs ""
where
	determine_dir_and_filename (dir:!dirs) longest_dir
		# sd=size dir
		| sd>size longest_dir && sd<size imp_pathname && imp_pathname % (0,sd-1)==dir
			= determine_dir_and_filename dirs dir
			= determine_dir_and_filename dirs longest_dir
	determine_dir_and_filename Nil longest_dir
		| size longest_dir==0
			# mdn = {mdn_dir = RemoveFilename imp_pathname, mdn_name = GetModuleName imp_pathname}
			= (mdn,imp_pathname)
			# (module_name,imp_pathname) = module_name_and_pathname_with_dots imp_pathname (size longest_dir)
			# mdn = {mdn_dir = longest_dir, mdn_name = module_name}
			= (mdn,imp_pathname)

	module_name_and_pathname_with_dots pathname filename_i
		| filename_i<size pathname && pathname.[filename_i]==DirSeparator
			= module_name_and_pathname_with_dots pathname (filename_i+1)
			# module_name = replace_dir_separators_by_dots (RemoveSuffix (pathname % (filename_i,size pathname-1)))
			#! file_ext_i = filename_i+size module_name
			# pathname = {if (i<filename_i || i>=file_ext_i) c module_name.[i-filename_i] \\ c<-:pathname & i<-[0..]}
			= (module_name,pathname)

	replace_dir_separators_by_dots s
		= {if (c==DirSeparator) '.' c \\ c<-:s}

//==

symPath :: !Pathname !Pathname !Pathname -> Pathname
symPath ap pp l
	| size ap >= size pp		// generate shortest symbolic path...
		#	l = replace_prefix_path ap "{Application}" l
			l = replace_prefix_path pp "{Project}" l
		= l
	| otherwise
		#	l = replace_prefix_path pp "{Project}" l
			l = replace_prefix_path ap "{Application}" l
		= l

fulPath :: !Pathname !Pathname !Pathname -> Pathname
fulPath ap pp l
	#	l = replace_prefix_path "{Application}" ap l
		l = replace_prefix_path "{Project}" pp l
	// ensure full pathname is just that...
	| IsFullPathname l
		= l
	// if not put it in the project directory...
	= MakeFullPathname pp l

get_separator_and_convert_path :: !Pathname -> (!Char,!Pathname) 
get_separator_and_convert_path path
	# prefix="{Project}"
	# prefix_size=size prefix
	# i=first_not_equal_character_index prefix path
	| i==prefix_size && size path>prefix_size
		= replace_prefix path.[prefix_size] path
	# prefix="{Application}"
	# prefix_size=size prefix
	# i=first_not_equal_character_index prefix path
	| i==prefix_size && size path>prefix_size
		= replace_prefix path.[prefix_size] path
	= (DirSeparator,path)
where
	replace_prefix separator path
		| separator==DirSeparator
			= (separator,path)
			= (separator,replace_character_in_string separator DirSeparator path)
	
	replace_character_in_string :: !Char !Char !{#Char} -> {#Char}
	replace_character_in_string old_c new_c string
		= {if (c==old_c) new_c c \\ c<-:string}

	first_not_equal_character_index s1 s2
		#! max_index=if (size s1<=size s2) (size s1) (size s2)
		= first_not_equal_character_index 0 max_index s1 s2
		where
			first_not_equal_character_index :: !Int !Int !{#Char} !{#Char} -> Int
			first_not_equal_character_index i n s1 s2
				| i<n && s1.[i]==s2.[i]
					= first_not_equal_character_index (i+1) n s1 s2
					= i

convert_path_separators :: !Pathname -> Pathname 
convert_path_separators path
	# (separator,path) = get_separator_and_convert_path path
	= path

convert_exec_path_separators_and_extension :: !Pathname -> Pathname 
convert_exec_path_separators_and_extension path
	# (separator,path) = get_separator_and_convert_path path
	| separator==DirSeparator
		= path
	| DirSeparator=='\\'
		= path+++".exe"
	| separator=='\\'
		# l=size path
		| l>4 && path.[l-4]=='\\'
			&& (path.[l-3]=='e' || path.[l-3]=='e')
			&& (path.[l-2]=='x' || path.[l-2]=='x')
			&& (path.[l-1]=='e' || path.[l-1]=='e')
			= path % (0,l-4)
		= path
	= path

symPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
symPaths ap pp l = Map (symPath ap pp) l

fulPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
fulPaths ap pp l = Map (fulPath ap pp) l

symAppPath :: !Pathname !Pathname -> Pathname
symAppPath ap p
	= replace_prefix_path ap "{Application}" p

fulAppPath :: !Pathname !Pathname -> Pathname
fulAppPath ap p
	= replace_prefix_path "{Application}" ap p

symAppPaths :: !Pathname !(List Pathname) -> List Pathname
symAppPaths ap l = Map (symAppPath ap) l

fulAppPaths :: !Pathname !(List Pathname) -> List Pathname
fulAppPaths ap l = Map (fulAppPath ap) l
