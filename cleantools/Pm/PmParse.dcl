definition module PmParse

// Clean system syntax dependant parsing

import StdString, StdFile
from UtilStrictLists import :: List

:: SearchKind
	= Definition
	| Implementation
	| Identifier

:: IdentifierPositionList
	= PosNil
	| Pos !Int !Int IdentifierPositionList
	| Cls !Int !Int IdentifierPositionList
	| Ins !Int !Int IdentifierPositionList

FindIdentifiersInFile ::
	!Bool
	!(List String)		// list of imported modules
	!String
	!String				// pathname
	!Files
	-> ((!List String,!IdentifierPositionList), !Files)

FindIdentifiersInText ::
	!Bool
	!(List String)		// list of imported modules
	!String
	!*{String}
	!Files 
	-> ((!List String,!IdentifierPositionList), !Files);

//	FindDefinitionInFile searches Clean source text (.dcl/.icl file) for an identifier.
FindDefinitionInFile ::
	!Bool						// Bool indicating whether a list of imported module names must be returned
	!(List String)				// List of module names to which modules names imported by this module must be attached
	!String						// The clean identifier to search for
	!String						// The .dcl/.icl full path name.
	!*Files						// The filesystem
	-> (
	( !List String				// List of imported module names
	, !IdentifierPositionList)	// 
	, !Files					// The filesystem
	)

FindDefinitionInText ::
	!Bool
	!(List String)		// list of imported modules
	!String
	!*{String}
	!*Files
	-> ((!List String,!IdentifierPositionList), !Files)

/*	Returns True when 1st arg. is a valid Clean identifier. */
CleanModId :: !String -> Bool;
is_h_module_name :: !String -> Bool;

/*	Returns True when 1st arg. is a type listed by the Clean compiler. */
IsTypeSpec :: !String -> Bool;

IsImportError13 :: !String -> (!Bool,!String);
IsImportError20 :: !String -> (!Bool,!String);

:: Def
	= DefFun !String !Int
	| DefType !String !Int
	| DefClass !String !Int
	| DefInst !String !String !Int
	| DefGeneric !String !Int
	| DefDerive !String !String !Int

FindDefinesInText :: !*{String} !Files -> (![Def],!Files)
