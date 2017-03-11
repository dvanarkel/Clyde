definition module StdPathname

::	Pathname			:== String

EmptyPathname	:== ""

RemovePath			:: !Pathname -> String
RemoveSuffix		:: !Pathname -> String
RemoveSuffix`		:: !Pathname -> String
RemoveFilename		:: !Pathname -> Pathname
replace_prefix_path	:: !Pathname !Pathname !Pathname -> Pathname
equal_suffix		:: !String !String -> Bool
IsFullPathname		:: !Pathname -> Bool
MakeFullPathname	:: !Pathname !String -> Pathname

replace_dots_by_dir_separators :: !{#Char} -> *{#Char};

quoted_string		:: !String -> String
