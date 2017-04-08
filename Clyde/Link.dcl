definition module Clyde.Link

from IdeState				import :: GeneralSt
from PmTypes				import :: ApplicationOptions, :: Processor

:: Path :== String

Link ::	!String					// linker process
		!Path					// path
		!ApplicationOptions		// application options
		!Path					// optionspathname
		![!Path!]				// library_file_names
		![!Path!]				// object_file_names
		![!Path!]				// static_libraries
		!Bool					// static
		!Bool					// gen_relocs
		!Bool					// gen_symbol_table
		!Bool					// gen_linkmap
		!Bool					// link_resources
		!String					// resource_path
		!Bool					// gen_dll
		!String					// dll_syms
		!Path					// startupdir
		!String					// dynlstr
		!Processor				// 
		!Bool					// 
		!*GeneralSt				// 
		 -> (!*GeneralSt,!Bool)


from PmFileInfo			import :: FileInfoCache
from PmProject			import :: Project

MakeOptionsName :: !.String !Processor -> String
CheckObjsOutOfDate :: !Bool !Path ![!String!] !*GeneralSt -> (!Bool,!*GeneralSt)
CheckExecOutOfDate :: !Bool !Path !FileInfoCache !Project !*GeneralSt -> *(!Bool,!*GeneralSt)
