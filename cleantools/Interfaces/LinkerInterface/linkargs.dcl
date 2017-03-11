definition module linkargs

from StdFile import :: Files
from StdMaybe import :: Maybe
from UtilStrictLists import ::List

:: LPathname :== String

:: LinkInfo` =
	{ exe_path		:: !LPathname
	, res_path		:: !LPathname
	, open_console	:: !Bool
	, static_link	:: !Bool
	, gen_relocs	:: !Bool
	, gen_symbol_table :: !Bool
	, gen_linkmap	:: !Bool
	, link_resources:: !Bool
	, object_paths	:: !List LPathname
	, dynamic_libs	:: !List LPathname
	, static_libs	:: !List LPathname
	, stack_size	:: !Int
	, gen_dll		:: !Bool
	, dll_names		:: !String
	, dynamics_path :: !String
	, lib_name_obj_path :: !String
	}

emptyLinkInfo` :: LinkInfo`

/* linker commandline args
	-I linkopts path
	-O linkerrs path
*/

// Default: applicationpath "linkerrs"
ReadLinkErrors :: !String !*Files -> ((Maybe [String],[String]),*Files)
// ReadLinkErrors errors_path filesystem -> ((maybe_read_error, link_errors),filesystem)
WriteLinkErrors :: !String ![String] !*Files -> (Maybe [String],*Files)
// WriteLinkErrors errors_path link_errors filesystem -> (maybe_write_error,filesystem)

// Default: applicationpath "linkopts"
WriteLinkOpts	:: !{#Char} !LinkInfo` !*Files -> (!Maybe [String], !*Files)
// WriteLinkOpts options_path link_info filesystem -> (maybe_write_error,filesystem)
ReadLinkOpts	:: !{#Char} !*Files -> ((!LinkInfo`, !Bool, !{#Char}),!*Files)
// ReadLinkOpts options_path filesystem -> ((link_info,read_succeeded,error_string),filesystem)
