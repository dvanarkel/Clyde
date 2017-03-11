implementation module PmParse

// Clean syntax dependent parsing

import StdClass,StdString,StdInt,StdChar,StdBool,StdMisc,StdFile,StdArray
import StdList, StdMaybe
import StdPathname
import UtilStrictLists
import UtilNewlinesFile

instance toString Symbol
where
	toString {repr,string}	= "{"+++.toString repr+++.":"+++.string+++."}"

::	SymbolId			:== Int;

::	* Input *a =
	{	offside	:: !Bool	// apply layout rule?
	,	curpos	:: !Int		// current position in current line
	,	curlen	:: !Int		// length of current line
	,	line	:: !String	// current line
	,	file	:: !a		// input file
	,	linenr	:: !Int		// current line number
	,	charnr	:: !Int		// current char number
	};

::	Symbol =
	{	repr	:: !SymbolId	// symbol id
	,	string	:: !String		// symbol string
	};

EofSymID			:== 0;
ErrorSymID			:== 1;

BarSymID			:== 2;
CloseSymID			:== 3;
CloseBraceSymID		:== 4;
CommaSymID			:== 5;
IsSymID				:== 6;
OpenBraceSymID		:== 7;
OpenSymID			:== 8;
SemiColonSymID		:== 9;
SynonymSymID		:== 10;
TypeSpecSymID		:== 11;
UniqueAttrSymID		:== 12;
IdentLowerSymID		:== 13;
IdentUpperSymID		:== 14;
IdentFunnySymID		:== 15;
FromSymID			:== 16;
ImportSymID			:== 17;
InstanceSymID		:== 18;
ClassSymID			:== 19;
InfixSymID			:== 20;
DigitSymID			:== 21;
WhereSymID			:== 22;
HashSymID			:== 23;
GenericSymID		:== 24;
DeriveSymID			:== 25;

EofSym				:== {repr = EofSymID ,string = ""};
ErrorSym			:== {repr = ErrorSymID ,string = ""};

BarSym				:== {repr = BarSymID, string = ""};
CloseSym			:== {repr = CloseSymID, string = ""};
CloseBraceSym		:== {repr = CloseBraceSymID, string = ""};
CommaSym			:== {repr = CommaSymID, string = ""};
IsSym				:== {repr = IsSymID, string = ""};
OpenBraceSym		:== {repr = OpenBraceSymID, string = ""};
OpenSym				:== {repr = OpenSymID, string = ""};
SynonymSym			:== {repr = SynonymSymID, string = ""};
SemiColonSym		:== {repr = SemiColonSymID, string = ""};
TypeSpecSym			:== {repr = TypeSpecSymID, string = ""};
UniqueAttrSym		:== {repr = UniqueAttrSymID, string = "*"};
FromSym				:== {repr = FromSymID, string = ""};
ImportSym			:== {repr = ImportSymID, string = ""};
InstanceSym			:== {repr = InstanceSymID, string = ""};
ClassSym			:== {repr = ClassSymID, string = ""};
InfixSym			:== {repr = InfixSymID, string = ""};
DigitSym			:== {repr = DigitSymID, string = ""}
WhereSym			:== {repr = WhereSymID, string = ""}
HashSym				:== {repr = HashSymID, string = ""}

IdentSymId id		:== id == IdentLowerSymID || id == IdentUpperSymID || id == IdentFunnySymID || id == UniqueAttrSymID;
IsTypeSymId id		:== id == IdentUpperSymID || id == IdentFunnySymID;
ConstrSymId id		:== id == IdentUpperSymID || id == IdentFunnySymID;
CommaSymId id		:== id == CommaSymID

:: DefinesState =
	{ syms	:: ![Def] }

:: Def
	= DefFun !String !Int
	| DefType !String !Int
	| DefClass !String !Int
	| DefInst !String !String !Int
	| DefGeneric !String !Int
	| DefDerive !String !String !Int

FindDefinesInText :: !*{String} !Files -> (![Def],!Files)
FindDefinesInText text files
	# input				= StartInput` text
	# state				= {syms = []}
	# (state,input)		= FindDefsInInput state input
	= (state.syms, files)

FindDefsInInput :: !DefinesState !(Input a) -> (!DefinesState,!(Input a)) | ScanInput a
FindDefsInInput state input
	# (input,sym,line,_) = ScanGlobal input
	= find_definitions input sym line state
where
	ScanGlobal input
		# (input,sym,line,char)	= ScanInput input
		| sym.repr == EofSymID
			= (input,sym,line,char)
		| char == 0
			= (input,sym,line,char)
			= ScanGlobal input

	find_definitions :: !(Input a) !Symbol !Int !DefinesState -> (!DefinesState,!(Input a)) | ScanInput a
	find_definitions input sym=:{repr} line state
		| sym.repr == EofSymID
			= (state,input)
		| sym.repr == ClassSymID	//=> class def
			# (input,sym,line,_)	= ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				# state				= {state & syms = [DefClass sym.string line:state.syms]}
				= FindDefsInInput state input
			| sym.repr == OpenSymID	//=> infix class def
				# (input,sym,line,_)	= ScanInput input
				| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
					# state = {state & syms = [DefClass sym.string line:state.syms]}
					= FindDefsInInput state input
				= FindDefsInInput state input
			= FindDefsInInput state input
		| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID	//=> fun or macro def
			#! function_name = sym.string
			# (input,sym,line,char)	= ScanInput input
			// generic function ?
			| sym.repr==OpenBraceSymID && char<>0
				# (input,sym,line,char)	= ScanInput input
				| sym.repr==BarSymID && char<>0
					# (input,sym,line,char)	= ScanInput input
					#! type_name = sym.string
					| (sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID) && char<>0
						# (input,sym,line,char)	= ScanInput input
						| sym.repr==BarSymID && char<>0
						# (input,sym,line,char)	= ScanInput input
						| sym.repr==CloseBraceSymID && char<>0
							#! name = function_name+++"{|"+++type_name+++"|}"
							# state	= {state & syms = [DefFun name line:state.syms]}
							= FindDefsInInput state input
							# state	= {state & syms = [DefFun function_name line:state.syms]}
							= FindDefsInInput state input
						# state	= {state & syms = [DefFun function_name line:state.syms]}
						= FindDefsInInput state input
					# state	= {state & syms = [DefFun function_name line:state.syms]}
					= FindDefsInInput state input
				# state	= {state & syms = [DefFun function_name line:state.syms]}
				= FindDefsInInput state input
			# state	= {state & syms = [DefFun function_name line:state.syms]}
			= FindDefsInInput state input
		| sym.repr == TypeSpecSymID	//=> type def
			# (input,sym,line,_)	= ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				# state				= {state & syms = [DefType sym.string line:state.syms]}
				= FindDefsInInput state input
			| sym.repr == UniqueAttrSymID	//=> unique type def
				# (input,sym,line,_)	= ScanInput input
				| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
					# state				= {state & syms = [DefType ("*"+++. sym.string) line:state.syms]}
					= FindDefsInInput state input
				= FindDefsInInput state input
			= FindDefsInInput state input
		| sym.repr == OpenSymID		//=> infix operator def
			# (input,sym,line,_)	= ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				# state				= {state & syms = [DefFun sym.string line:state.syms]}
				= FindDefsInInput state input
			= FindDefsInInput state input
		| sym.repr == InstanceSymID	//=> instance def
			# (input,sym,line,_) = ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				= instance_or_derive_definition sym.string DefInst state input
			= FindDefsInInput state input
		| repr==GenericSymID
			# (input,sym,line,_) = ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				# state = {state & syms = [DefGeneric sym.string line:state.syms]}
				= FindDefsInInput state input
				= FindDefsInInput state input
		| repr==DeriveSymID
			# (input,sym,line,_) = ScanInput input
			| sym.repr == IdentLowerSymID || sym.repr == IdentUpperSymID || sym.repr == IdentFunnySymID
				= instance_or_derive_definition sym.string DefDerive state input
			= FindDefsInInput state input
			= FindDefsInInput state input

	ScanMatch :: {#Char} Int Int ({#Char},{#Char}) Int *(Input a) -> *(!*(Input a),!{#Char}) | ScanInput a;
	ScanMatch str open close (oString,cString) depth input
		# (input,sym,line,char)	= ScanInput input
		| sym.repr == EofSymID
			= (input,"")
		| sym.repr == open
			= ScanMatch (str +++. oString) open close (oString,cString) (inc depth) input
		| sym.repr == close
			| depth == 0
				= (input, str +++. cString)
			= ScanMatch (str +++. cString) open close (oString,cString) (dec depth) input
		= case sym.repr of
			BarSymID		-> ScanMatch (str +++. "|") open close (oString,cString) depth input
			CloseSymID		-> ScanMatch (str +++. ")") open close (oString,cString) depth input
			CloseBraceSymID	-> ScanMatch (str +++. "}") open close (oString,cString) depth input
			CommaSymID		-> ScanMatch (str +++. ",") open close (oString,cString) depth input
			IsSymID			-> ScanMatch (str +++. "=") open close (oString,cString) depth input
			OpenBraceSymID	-> ScanMatch (str +++. "{") open close (oString,cString) depth input
			OpenSymID		-> ScanMatch (str +++. "(") open close (oString,cString) depth input
			SynonymSymID	-> ScanMatch (str +++. ":==") open close (oString,cString) depth input
			SemiColonSymID	-> ScanMatch (str +++. ";") open close (oString,cString) depth input
			TypeSpecSymID	-> ScanMatch (str +++. "::") open close (oString,cString) depth input
			FromSymID		-> ScanMatch (str +++. "from") open close (oString,cString) depth input
			ImportSymID		-> ScanMatch (str +++. "import") open close (oString,cString) depth input
			InstanceSymID	-> ScanMatch (str +++. "instance") open close (oString,cString) depth input
			ClassSymID		-> ScanMatch (str +++. "class") open close (oString,cString) depth input
			InfixSymID		-> ScanMatch (str +++. "infix") open close (oString,cString) depth input
			DigitSymID		#  (char,input) = input!line.[char]
							-> ScanMatch (str +++. {char}) open close (oString,cString) depth input
			WhereSymID		-> ScanMatch (str +++. "where") open close (oString,cString) depth input
			HashSymID		-> ScanMatch (str +++. "#") open close (oString,cString) depth input
			GenericSymID	-> ScanMatch (str +++. "generic") open close (oString,cString) depth input
			DeriveSymID		-> ScanMatch (str +++. "derive") open close (oString,cString) depth input
			_				-> ScanMatch (str +++. sym.string) open close (oString,cString) depth input

	instance_or_derive_definition name make_def state input
		# (input,sym`,line,_) = ScanInput input
		| sym`.repr == IdentLowerSymID || sym`.repr == IdentUpperSymID || sym`.repr == IdentFunnySymID
			# state				= {state & syms = [make_def name sym`.string line:state.syms]}
			// can be a list in .dcl so need to read on for CommaSymId...
			= more_instance_or_derive_types name make_def state input
		| sym`.repr == OpenSymID || sym`.repr == OpenBraceSymID
			// save contents upto matching close sym...
			# closeSym			= case sym`.repr of
				OpenSymID			-> CloseSymID
				OpenBraceSymID		-> CloseBraceSymID
			# (oString,cString) = case sym`.repr of
				OpenSymID			-> ("(",")")
				OpenBraceSymID		-> ("{","}")
			# (input,str)		= ScanMatch oString sym`.repr closeSym (oString,cString) 0 input
			# state				= {state & syms = [make_def name str line:state.syms]}
			= more_instance_or_derive_types name make_def state input
		// or uniqueness & strictness attributes...
		//...
		= FindDefsInInput state input

	more_instance_or_derive_types name make_def state input
		# (input,sym,line,char)	= ScanInput input
		| sym.repr == CommaSymID
			= instance_or_derive_definition name make_def state input
		| sym.repr == EofSymID
			= (state,input)
		| char==0
			= find_definitions input sym line state
			= FindDefsInInput state input

//	Find the identifiers in the .dcl and the .icl files;

FindIdentifiersInFile :: !Bool !(List String) !String !String !Files 
	-> ((!List String,!IdentifierPositionList), !Files);
FindIdentifiersInFile imp imports cleanid path files
	#	(files,ok,file)								= OpenTextFile path files
	| not ok
		= ((imports,PosNil), files)
	#	(input,sym,_,_)								= ScanInput (StartInput file)
	#	(input,imports,identifier_position_list)	= FindIdentifiersInInput imp cleanid imports sym input PosNil
	= ((imports,identifier_position_list), CloseTextFile (EndInput input) files)

FindIdentifiersInText :: !Bool !(List String) !String !*{String} !Files 
	-> ((!List String,!IdentifierPositionList), !Files);
FindIdentifiersInText imp imports cleanid text files
	#	(input,sym,_,_)								= ScanInput (StartInput` text)
	#	(input,imports,identifier_position_list)	= FindIdentifiersInInput imp cleanid imports sym input PosNil
	= ((imports,identifier_position_list), files)

//	Find the definitions in the .dcl and the .icl files;

FindDefinitionInFile :: !Bool !(List String) !String !String !*Files
	-> ((!List String,!IdentifierPositionList), !Files)
FindDefinitionInFile imp imports cleanid path files
	#	(files, ok, file)			= OpenTextFile path files
	| not ok
		= ((imports,PosNil), files)
	#	(input,imports,positions)	= FindDefinitionInInput imp cleanid imports (StartInput file)
	= ((imports,positions), CloseTextFile (EndInput input) files)

FindDefinitionInText :: !Bool !(List String) !String !*{String} !*Files
	-> ((!List String,!IdentifierPositionList), !Files)
FindDefinitionInText imp imports cleanid text files
	#	(input,imports,positions)	= FindDefinitionInInput imp cleanid imports (StartInput` text)
	= ((imports,positions), files)

FindIdentifiersInInput :: !Bool !String !(List String) !Symbol !(Input a) IdentifierPositionList -> (!Input a, !List String, !IdentifierPositionList) | ScanInput a;
FindIdentifiersInInput imp cleanid imports sym=:{repr} input positions
	| repr==FromSymID && imp
		# (input,sym,imports,positions)	= FindFrom imports input positions
		= FindIdentifiersInInput imp cleanid imports sym input positions
	| repr==ImportSymID && imp
		# (input,sym,imports,positions)	= FindImport imports input positions
		= FindIdentifiersInInput imp cleanid imports sym input positions
	| repr==EofSymID
		= (input,imports,positions);
	# (input,sym,linenr,charnr) = ScanInput input;
	| IdentSymId sym.repr && sym.string==cleanid
		#	(input,imports,positions)=FindIdentifiersInInput imp cleanid imports sym input positions
		=	(input,imports,Pos linenr charnr positions);
	= FindIdentifiersInInput imp cleanid imports sym input positions
where
	FindFrom :: !(List String) !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !List String, !IdentifierPositionList) | ScanInput a;
	FindFrom imports input positions
		# (input,sym,linenr,charnr)	= scan_module_symbol_from_input input;
		| IdentSymId sym.repr
			| sym.string == cleanid
				# (input,sym,imports,positions) = FindFromIdent (sym.string :! imports) input positions
				= (input,sym,imports,Pos linenr charnr positions)
			= FindFromIdent (sym.string :! imports) input positions
		= (input,sym, imports,positions);
	where
		FindFromIdent :: !(List String) !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !List String, IdentifierPositionList) | ScanInput a;
		FindFromIdent imports input positions
			# (input,sym,_,_)	= ScanInput input;
			| sym.repr == ImportSymID
				# (input,sym,positions) = FindImport input positions
				= (input,sym,imports,positions)
			= (input, sym, imports,positions);

		FindImport :: !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !IdentifierPositionList) | ScanInput a;
		FindImport input positions
			# (input,sym,linenr,charnr)	= ScanInput input
			| IdentSymId sym.repr
				| sym.string == cleanid
					# (input,sym,positions) = FindImportIdent input positions
					= (input,sym,Pos linenr charnr positions)
				= FindImportIdent input positions
			= (input,sym,positions);
		where
			FindImportIdent :: !(Input a) IdentifierPositionList -> (!Input a, !Symbol,IdentifierPositionList) | ScanInput a;
			FindImportIdent input positions
				# (input,sym,_,_)	= ScanInput input
				| sym.repr == CommaSymID
					= FindImportIdentComma input positions
				= (input,sym, positions);
		
			FindImportIdentComma :: !(Input a) IdentifierPositionList -> (!Input a, !Symbol, IdentifierPositionList) | ScanInput a;
			FindImportIdentComma input positions
				# (input,sym,linenr,charnr)	= ScanInput input
				| IdentSymId sym.repr
					| sym.string == cleanid
						# (input,sym,positions) = FindImportIdent input positions
						= (input,sym,Pos linenr charnr positions)
					= FindImportIdent input positions
				= (input,sym, positions)

	FindImport :: !(List String) !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !List String, !IdentifierPositionList) | ScanInput a;
	FindImport imports input positions
		# (input,sym,linenr,charnr)	= scan_module_symbol_from_input input
		| IdentSymId sym.repr
			| sym.string == cleanid
				# (input,sym,imports,positions) = FindImportIdent (sym.string :! imports) input positions
				= (input,sym,imports,Pos linenr charnr positions)
			= FindImportIdent (sym.string :! imports) input positions
		= (input,sym,imports,positions);
	where
		FindImportIdent :: !(List String) !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !List String,IdentifierPositionList) | ScanInput a;
		FindImportIdent imports input positions
			# (input,sym,_,_)	= ScanInput input
			| sym.repr == CommaSymID
				= FindImportIdentComma imports input positions
			= (input,sym, imports,positions);

		FindImportIdentComma :: !(List String) !(Input a) IdentifierPositionList -> (!Input a, !Symbol, !List String,IdentifierPositionList) | ScanInput a;
		FindImportIdentComma imports input positions
			# (input,sym,linenr,charnr)	= scan_module_symbol_from_input input
			| IdentSymId sym.repr
				| sym.string == cleanid
					# (input,sym,imports,positions) = FindImportIdent (sym.string :! imports) input positions
					= (input,sym,imports,Pos linenr charnr positions)
				= FindImportIdent (sym.string :! imports) input positions
			= (input,sym, imports, positions)

FindDefinitionInInput :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
FindDefinitionInInput imp cleanid imports input
	= SkipToDefinition imp cleanid imports sym input`;
where
	(input`,sym,_,_)		= ScanInput input;

Definition1 :: !(Maybe (Symbol,Int,Int)) !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
Definition1 maybe_sym imp cleanid imports input
	| match_from
		#	(inputa,syma,importsa)	= ScanFrom imports input`;
		= SkipToDefinition imp cleanid importsa syma inputa;
	| match_import
		#	(inputb,symb,importsb)	= ScanImport imports input`;
		= SkipToDefinition imp cleanid importsb symb inputb;
	| sym.repr == TypeSpecSymID
		= TypeDef imp cleanid imports input`;
	| match_instance	= InstanceDef imp cleanid imports input`;
	| match_class		= ClassDef imp cleanid imports input`;
	| match_ident		= RuleOrMacroDef lnr cnr imp False cleanid imports input`;
	| match_open		= InfixRuleDef imp cleanid imports input`;
	| match_where		= RuleOrInfixRuleDef imp cleanid imports input`;
	| sym.repr == GenericSymID || sym.repr == DeriveSymID
		= generic_or_derive_definition imp cleanid imports input`;
		= SkipToDefinition imp cleanid imports sym input`;
where 
	(input`,sym,lnr,cnr)	= case maybe_sym of
								Just (sym,lnr,cnr)	-> (input,sym,lnr,cnr)
								_					-> ScanInput input;
	match_from				= sym.repr == FromSymID && imp;
	match_import			= sym.repr == ImportSymID && imp;
	match_instance			= sym.repr == InstanceSymID;
	match_class				= sym.repr == ClassSymID;
	match_ident				= IdentSymId sym.repr && sym.string == cleanid;
	match_open				= sym.repr == OpenSymID;
	match_where				= sym.repr == WhereSymID
	
RuleOrInfixRuleDef :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
RuleOrInfixRuleDef imp cleanid imports input
	| match_ident		= RuleOrMacroDef lnr cnr imp False cleanid imports input`;
	| match_open		= InfixRuleDef imp cleanid imports input`;
	 					= SkipToDefinition imp cleanid imports sym input`;
where 
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident				= IdentSymId sym.repr && sym.string == cleanid;
	match_open				= sym.repr == OpenSymID;
	
ClassDef :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
ClassDef imp cleanid imports input
	#	(input,sym,lnr,cnr)	= ScanInput input;
	| (IdentSymId sym.repr && sym.string == cleanid)
		= FoundClass cleanid sym lnr cnr imp imports input
	| (sym.repr == OpenSymID)
		#	(input,sym,lnr`,cnr`)	= ScanInput input
		| (IdentSymId sym.repr && sym.string == cleanid)
			#	(input,sym,_,_)	= ScanInput input
			| sym.repr == CloseSymID
				= FoundClass cleanid sym lnr` cnr` imp imports input
			// possibly do fixprec first
			= SkipToDefinition imp cleanid imports sym input
		= SkipToDefinition imp cleanid imports sym input
	# (input,sym) = dottypevar input
	| (sym.repr == BarSymID)
		= ClassContext imp cleanid imports sym input
	| (sym.repr == WhereSymID)
		= ClassMemberDef imp cleanid imports input
	= SkipToDefinition imp cleanid imports sym input
where
	dottypevar input
		# (input,sym,_,_) = ScanInput input
		| sym.repr == IdentFunnySymID && sym.string == "."
			= dottypevar input
		| sym.repr == IdentLowerSymID
			= dottypevar input
		= (input,sym)

	FoundClass cleanid sym linenr charnr imp imports input
			# (input,sym) = dottypevar input
			| (sym.repr == BarSymID)
				# (input,imports,posl)	= ClassContext imp cleanid imports sym input
				# posl = Cls linenr charnr posl		
				= (input, imports, posl)
			| sym.repr == WhereSymID
				# (input,imports,posl)	= ClassMemberDef imp cleanid imports input
				# posl = Cls linenr charnr posl		
				= (input, imports, posl)
			# (input,imports,posl)	= SkipToDefinition imp cleanid imports sym input
			# posl = Cls linenr charnr posl		
			= (input, imports, posl)

ClassContext imp cleanid imports sym input		// | X,Y,Z a & A,B,C .d
	# (input,sym,_,_) = ScanInput input
	| IdentSymId sym.repr
		// find comma
		# (input,sym,_,_) = ScanInput input
		| sym.repr == CommaSymID
			= ClassContext imp cleanid imports sym input
		= lookforclassvar sym input
	| sym.repr == WhereSymID
		= ClassMemberDef imp cleanid imports input
	= SkipToDefinition imp cleanid imports sym input
where	
	lookforclassvar sym input
		| sym.repr == IdentFunnySymID && sym.string == "."
			# (input,sym,_,_) = ScanInput input
			= lookforclassvar sym input
		| sym.repr == IdentLowerSymID
			# (input,sym,_,_) = ScanInput input
			| sym.repr == IdentFunnySymID && sym.string == "&"
				= ClassContext imp cleanid imports sym input
			| sym.repr == SemiColonSymID
				# (input,sym,lnr,cnr) = ScanInput input
				| sym.repr == WhereSymID
					= ClassMemberDef imp cleanid imports input
				= Definition1 (Just (sym,lnr,cnr)) imp cleanid imports input
			| sym.repr == WhereSymID
				= ClassMemberDef imp cleanid imports input
			= SkipToDefinition imp cleanid imports sym input
		= SkipToDefinition imp cleanid imports sym input
		
ClassMemberDef imp cleanid imports input	// =:{} need to track indent level here...
		# (input,sym,lnr,cnr)	= ScanInput input
		# match_semicolon	= sym.repr == SemiColonSymID
		| match_semicolon	= Definition1 Nothing imp cleanid imports input
		# match_openbrace	= sym.repr == OpenBraceSymID
		| match_openbrace	= ClassMemberBraces 1 input
		# match_eof			= sym.repr == EofSymID
		| match_eof			= (input,imports,PosNil)
		# match_ident		= IdentSymId sym.repr && sym.string == cleanid
		| match_ident		= RuleOrMacroDef lnr cnr imp True cleanid imports input
		# match_open		= sym.repr == OpenSymID;
		| match_open		= InfixMemberDef input
		= ClassMemberDef imp cleanid imports input
where
//	InfixMemberDef :: !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
	InfixMemberDef input
		| match_ident		= IRC lnr cnr imp cleanid imports input`;
							= ClassMemberDef imp cleanid imports input`;
	where 
		(input`,sym,lnr,cnr)	= ScanInput input;
		match_ident				= IdentSymId sym.repr && sym.string == cleanid;

	ClassMemberBraces nesting input
		| match_openbrace	= ClassMemberBraces (inc nesting) input`;
		| match_closebrace1	= ClassMemberDef imp cleanid imports input`;
		| match_closebrace2	= ClassMemberBraces (dec nesting) input`;
		| match_eof			= (input`,imports,PosNil);
							= ClassMemberBraces nesting input`;
		where 
		(input`,sym,_,_)	= ScanInput input;
		match_openbrace		= sym.repr == OpenBraceSymID;
		match_closebrace1	= match_closebrace2 && nesting == 1;
		match_closebrace2	= sym.repr == CloseBraceSymID;
		match_eof			= sym.repr == EofSymID;

InstanceDef :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
InstanceDef imp cleanid imports input
	| match_ident	= FoundInstance cleanid sym lnr cnr imp imports input`;
	| match_open
		| match_ident`
			| match_close	= FoundInstance cleanid sym`` lnr` cnr` imp imports input```;
			= SkipToDefinition imp cleanid imports sym`` input```;
		= SkipToDefinition imp cleanid imports sym` input``;
	= SkipToDefinition imp cleanid imports sym input`;
where  
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident				= IdentSymId sym.repr && sym.string == cleanid;
	match_open				= sym.repr == OpenSymID;
	(input``,sym`,lnr`,cnr`)	= ScanInput input`;
	match_ident`				= IdentSymId sym`.repr && sym`.string == cleanid;
	(input```,sym``,_,_)	= ScanInput input``;
	match_close				= sym``.repr == CloseSymID
	
RuleOrMacroDef :: !Int !Int !Bool !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
RuleOrMacroDef linenr charnr imp in_class cleanid imports input
	| sym.repr == TypeSpecSymID
		= FoundDefinition cleanid sym linenr charnr imp imports input`;
	| match_synonym		= FoundDefinition cleanid sym linenr charnr imp imports input`;
	| match_variable	= RuleOrMacroDef linenr charnr imp in_class cleanid imports input`
	| match_is			= FoundDefinition cleanid sym linenr charnr imp imports input`
	| in_class			= ClassMemberDef imp cleanid imports input`
		| sym.repr == OpenBraceSymID
			# (input``,sym,_,_) = ScanInput input`
			| sym.repr==BarSymID
				= FoundDefinition cleanid sym linenr charnr imp imports input``
				= braces_sym sym 1 imp cleanid imports input``
			= SkipToDefinition imp cleanid imports sym input`;
where 
	(input`,sym,_,_)		= ScanInput input;
	match_synonym			= sym.repr == SynonymSymID;
	match_variable			= sym.repr == IdentLowerSymID;
	match_is				= imp && (sym.repr == IsSymID || sym.repr == HashSymID || sym.repr == BarSymID);	// only find rules without types in .icl XXX

InfixRuleDef :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
InfixRuleDef imp cleanid imports input
	| match_ident		= IRC lnr cnr imp cleanid imports input`;
						= SkipToDefinition imp cleanid imports sym input`;
where 
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident				= IdentSymId sym.repr && sym.string == cleanid;
	
IRC :: !Int !Int !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
IRC l c imp cleanid imports input
	| match_close = InfixRuleDef1 l c imp cleanid imports input`
	= SkipToDefinition imp cleanid imports sym input`
where
	(input`,sym,_,_)		= ScanInput input;
	match_close				= sym.repr == CloseSymID;

InfixRuleDef1 :: !Int !Int !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
InfixRuleDef1 linenr charnr imp cleanid imports input
	| sym.repr == TypeSpecSymID
		= FoundDefinition cleanid sym linenr charnr imp imports input`;
	| match_synonym		= FoundDefinition cleanid sym linenr charnr imp imports input`;
	| match_infix		= IRD linenr charnr imp  cleanid imports input` 
	| match_variable	= InfixRuleDef1 linenr charnr imp cleanid imports input`;
						= SkipToDefinition imp cleanid imports sym input`;
where 
	(input`,sym,_,_)		= ScanInput input;
	match_synonym			= sym.repr == SynonymSymID;
	match_variable			= sym.repr == IdentLowerSymID;
	match_infix				= sym.repr == InfixSymID;
	
IRD  l c imp cleanid imports input
	| sym.repr == TypeSpecSymID
		= FoundDefinition cleanid sym l c imp imports input`
	| match_synonym			= FoundDefinition cleanid sym l c imp imports input`
	| match_digit
		| sym`.repr == TypeSpecSymID
			= FoundDefinition cleanid sym l c imp imports input``
		| match_synonym`	= FoundDefinition cleanid sym l c imp imports input``
		= SkipToDefinition imp cleanid imports sym` input``
	= SkipToDefinition imp cleanid imports sym input`
where
	(input`,sym,_,_)		= ScanInput input;
	match_synonym			= sym.repr == SynonymSymID;
	match_digit				= sym.repr == DigitSymID;
	(input``,sym`,_,_)		= ScanInput input`;
	match_synonym`			= sym`.repr == SynonymSymID;

TypeDef :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
TypeDef imp cleanid imports input
	| match_typevar1
		= FoundDefinition cleanid sym lnr cnr imp imports input`;
	| match_typevar2
		= Algebraic imp cleanid imports input`;
	| sym.repr==SemiColonSymID || sym.repr==OpenBraceSymID
		= SkipToDefinition imp cleanid imports sym input`;
	| sym.repr<>EofSymID
		= TypeDef imp cleanid imports input`;
		= (input`,imports,PosNil);
	where 
		(input`,sym,lnr,cnr)	= ScanInput input;
		match_typevar2			= IsTypeSymId sym.repr;
		match_typevar1			= match_typevar2 && sym.string == cleanid;

		Algebraic :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
		Algebraic imp cleanid imports input
			| sym.repr==IsSymID
				= Constructors imp cleanid imports input`;
			| sym.repr==SemiColonSymID || sym.repr==OpenBraceSymID
				= SkipToDefinition imp cleanid imports sym input`;
			| sym.repr<>EofSymID
				= Algebraic imp cleanid imports input`;
				= (input`,imports,PosNil);
			where 
				(input`,sym,_,_)	= ScanInput input;

Constructors :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
Constructors imp cleanid imports input
	| match_ident1		= FoundDefinition cleanid sym lnr cnr imp imports input`;
	| match_ident2		= NextConstructors imp cleanid imports input`;
	| match_open		= InfixConstructors imp cleanid imports input`;
	| match_brace		= Record imp cleanid imports input`
						= SkipToDefinition imp cleanid imports sym input`;
	where 
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident2			= ConstrSymId sym.repr;
	match_ident1			= match_ident2 && sym.string == cleanid;
	match_open				= sym.repr == OpenSymID;
	match_brace				= sym.repr == OpenBraceSymID;
	
NextConstructors :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
NextConstructors imp cleanid imports input
	| sym.repr==BarSymID
		= Constructors imp cleanid imports input1;
	| sym.repr<>SemiColonSymID /*&& sym.repr<>OpenBraceSymID*/ && sym.repr<>EofSymID
		= NextConstructors imp cleanid imports input1;
		= SkipToDefinition imp cleanid imports sym input1;
	where 
		(input1,sym,_,_)	= ScanInput input;
	
Record imp cleanid imports input
	| match_ident1		= FoundDefinition cleanid sym lnr cnr imp imports input`;
	| match_ident2		= NextFields imp cleanid imports input`;
						= SkipToDefinition imp cleanid imports sym input`;
	where 
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident2			= IdentSymId sym.repr;
	match_ident1			= match_ident2 && sym.string == cleanid;

NextFields :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
NextFields imp cleanid imports input
	| sym.repr==CommaSymID
		= Record imp cleanid imports input1;
	| sym.repr<>SemiColonSymID && sym.repr<>OpenBraceSymID && sym.repr<>EofSymID
		= NextFields imp cleanid imports input1;
		= SkipToDefinition imp cleanid imports sym input1;
	where 
		(input1,sym,_,_)	= ScanInput input;

InfixConstructors :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
InfixConstructors imp cleanid imports input
	| match_ident1	= FoundDefinition cleanid sym lnr cnr imp imports input`;
	| match_ident2	= NextConstructors imp cleanid imports input`;
					= SkipToDefinition imp cleanid imports sym input`;
	where 
	(input`,sym,lnr,cnr)	= ScanInput input;
	match_ident1			= match_ident2 && sym.string == cleanid;
	match_ident2			= ConstrSymId sym.repr;

Braces :: !Int !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
Braces nesting imp cleanid imports input
	# (input`,sym,_,_)	= ScanInput input;
	= braces_sym sym nesting imp cleanid imports input`;

braces_sym :: !Symbol !Int !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
braces_sym sym nesting imp cleanid imports input
	| match_openbrace	= Braces (inc nesting) imp cleanid imports input;
	| match_closebrace1	= Definition1 Nothing imp cleanid imports input;
	| match_closebrace2	= Braces (dec nesting) imp cleanid imports input;
	| match_eof			= (input,imports,PosNil);
						= Braces nesting imp cleanid imports input;
	where 
	match_openbrace		= sym.repr == OpenBraceSymID;
	match_closebrace1	= match_closebrace2 && nesting == 1;
	match_closebrace2	= sym.repr == CloseBraceSymID;
	match_eof			= sym.repr == EofSymID;

ScanFrom	:: !(List String) !(Input a) -> (!Input a, !Symbol, !List String) | ScanInput a;
ScanFrom imports input
	# (input`,sym,_,_)	= scan_module_symbol_from_input input;
	| IdentSymId sym.repr
		= ScanFromIdent (sym.string :! imports) input`;
		= (input`,sym, imports);
where
	ScanFromIdent	:: !(List String) !(Input a) -> (!Input a, !Symbol, !List String) | ScanInput a;
	ScanFromIdent imports input
		# (input`,sym,_,_)	= ScanInput input;
		| sym.repr == ImportSymID
			= ScanFromIdentImport imports input`;
			= (input`, sym, imports);
		
	ScanFromIdentImport	:: !(List String) !(Input a) -> (!Input a, !Symbol, !List String) | ScanInput a;
	ScanFromIdentImport imports input
		# (input`,sym,_,_)	= ScanInput input;
		= (input`, sym, imports);

ScanImport	:: !(List String) !(Input a) -> (!Input a, !Symbol, !List String) | ScanInput a;
ScanImport imports input
	# (input`,sym,_,_)		= scan_module_symbol_from_input input;
	| IdentSymId sym.repr	= ScanImportComma (sym.string :! imports) input`;
							= (input`,sym, imports);
where 
	ScanImportComma	:: !(List String) !(Input a) -> (!Input a, !Symbol, !List String) | ScanInput a;
	ScanImportComma imports input
		# (input`,sym,_,_)		= scan_module_symbol_from_input input;
		| CommaSymId sym.repr	= ScanImport imports input`;
								= (input`,sym, imports);

generic_or_derive_definition :: !Bool !String !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
generic_or_derive_definition imp cleanid imports input
	# (input`,sym,lnr,cnr) = ScanInput input;
	| IdentSymId sym.repr && sym.string == cleanid
		= FoundDefinition cleanid sym lnr cnr imp imports input`;
		= SkipToDefinition imp cleanid imports sym input`;

FoundDefinition :: !String !Symbol !Int Int !Bool !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
FoundDefinition cleanid sym linenr charnr imp imports input
	# (input`,imports`,posl)	= SkipToDefinition imp cleanid imports sym input
	# posl` = Pos linenr charnr posl
	= (input`, imports`, posl`);

FoundInstance :: !String !Symbol !Int Int !Bool !(List String) !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
FoundInstance cleanid sym linenr charnr imp imports input
	#	(input`,imports`,posl)	= SkipToDefinition imp cleanid imports sym input
	#	posl` = case posl of
					PosNil		-> Ins linenr charnr PosNil
					Ins r c l	-> Ins r c (Ins linenr charnr l)
					Cls r c l	-> Cls r c (Ins linenr charnr l)
					Pos r c l	-> Pos r c (Ins linenr charnr l)
	= (input`, imports`, posl`);


SkipToDefinition :: !Bool !String !(List String) !Symbol !(Input a) -> (!Input a, !List String,!IdentifierPositionList) | ScanInput a;
SkipToDefinition imp cleanid imports sym=:{repr} input
	# match_semicolon	= repr == SemiColonSymID
	| match_semicolon	= Definition1 Nothing imp cleanid imports input
	# match_openbrace	= repr == OpenBraceSymID
	| match_openbrace	= Braces 1 imp cleanid imports input
	# match_eof			= repr == EofSymID
	| match_eof			= (input,imports,PosNil)
	# (input,sym,_,_)	= ScanInput input
	= SkipToDefinition imp cleanid imports sym input

/* Auxilary functions which attempt to match specific symbols at the current position of the input */

FindSym :: !Int !Int !String -> (!Bool,!Int,!Symbol);
FindSym curpos curlen line
	| curpos >= curlen		= (False, curpos, ErrorSym);
	# curchar	= line.[curpos];
	  curpos` = inc curpos;
	| curchar == ':'		= FindColon1 curpos` curlen line;
	| curchar == '='		= FindIs1 curpos` curlen line;
	| curchar == '*'		= FindStar1 curpos` curlen line;
	| curchar == ','		= (True, curpos`, CommaSym);
	| curchar == '{'		= (True, curpos`, OpenBraceSym);
	| curchar == '}'		= (True, curpos`, CloseBraceSym);
	| curchar == '('		= (True, curpos`, OpenSym);
	| curchar == ')'		= (True, curpos`, CloseSym);
	| curchar == '|'		= FindBar1 curpos` curlen line;
	| curchar == '#'		= FindHash1 curpos` curlen line;
	| curchar == ';'		= (True, curpos`, SemiColonSym);
	| curchar == '_'		= FindId1 IdentLowerSymID curpos curpos` curlen line; // for the moment
	| curchar == '`'		= FindId1 IdentLowerSymID curpos curpos` curlen line; // for the moment
	| LowerCase curchar		= FindId1 IdentLowerSymID curpos curpos` curlen line;
	| UpperCase curchar 	= FindId1 IdentUpperSymID curpos curpos` curlen line;
	| SpecialChar curchar	= FindId2 IdentFunnySymID curpos curpos` curlen line;
	| Digit curchar			= (True,curpos`,DigitSym);
							= (False, curpos`, ErrorSym);	

find_module_symbol :: !Int !Int !String -> (!Bool,!Int,!Symbol);
find_module_symbol curpos curlen line
	| curpos >= curlen
		= (False, curpos, ErrorSym);
	| UpperCase line.[curpos] || LowerCase line.[curpos]
		= find_module_ident curpos (curpos+1) curlen line;
		= FindSym curpos curlen line;
  where
	find_module_ident :: !Int !Int !Int !String -> (!Bool, !Int, !Symbol);
	find_module_ident start curpos curlen str
		| curpos >= curlen
			= FindId1 IdentUpperSymID start curpos curlen str;
		# curchar = str.[curpos];
		| LowerCase curchar || UpperCase curchar || Digit curchar || Special curchar
			= find_module_ident start (curpos+1) curlen str;
		| curchar=='.'
			= find_h_module_ident start (curpos+1) curlen str;
			= FindId1 IdentUpperSymID start curpos curlen str;

find_h_module_ident :: !Int !Int !Int !String -> (!Bool, !Int, !Symbol);
find_h_module_ident start curpos curlen str
	| curpos >= curlen
		= (True,curpos,{repr=IdentUpperSymID, string=str % (start,dec curpos)});
	# curchar = str.[curpos];
	| LowerCase curchar || UpperCase curchar || Digit curchar || Special curchar || curchar=='.'
		= find_h_module_ident start (curpos+1) curlen str;
		= (True,curpos,{repr=IdentUpperSymID, string=str % (start,dec curpos)});

FindColon1 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
FindColon1 curpos curlen line
	| curpos >= curlen	= FindId2 IdentFunnySymID (dec curpos) curpos curlen line;
	# curchar = line.[curpos];
	  curpos` = inc curpos;
	| curchar == '='	= FindColon2 curpos` curlen line;
	| curchar == ':'	= (True, curpos`, TypeSpecSym);
						= FindId2 IdentFunnySymID (dec curpos) curpos curlen line;
where
	FindColon2 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
	FindColon2 curpos curlen line
		| curpos >= curlen	= FindId2 IdentFunnySymID (dec curpos) curpos curlen line;
		# curchar = line.[curpos];
		  curpos` = inc curpos;
		| curchar == '='	= (True, curpos`, SynonymSym);
							= FindId2 IdentFunnySymID (curpos-2) curpos curlen line;

FindIs1 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
FindIs1 curpos curlen line
	| curpos >= curlen		= (True, curpos, IsSym);
	| SpecialChar curchar	= FindId2 IdentFunnySymID (dec curpos) (inc curpos) curlen line;
						 	= (True, curpos, IsSym);
	where 
		curchar	= line.[curpos];

FindHash1 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
FindHash1 curpos curlen line
	| curpos >= curlen		= (True, curpos, HashSym);
	| SpecialChar curchar	= FindId2 IdentFunnySymID (dec curpos) (inc curpos) curlen line;
						 	= (True, curpos, HashSym);
	where 
		curchar	= line.[curpos];

FindBar1 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
FindBar1 curpos curlen line
	| curpos >= curlen		= (True, curpos, BarSym);
	| SpecialChar curchar	= FindId2 IdentFunnySymID (dec curpos) (inc curpos) curlen line;
						 	= (True, curpos, BarSym);
	where 
		curchar	= line.[curpos];
	
FindStar1 :: !Int !Int !String -> (!Bool, !Int, !Symbol);
FindStar1 curpos curlen line
	| curpos >= curlen		= (True, curpos, UniqueAttrSym);
	| SpecialChar curchar	= FindId2 IdentFunnySymID (dec curpos) (inc curpos) curlen line;
						 	= (True, curpos, UniqueAttrSym);
	where 
		curchar	= line.[curpos];

FindId1 :: !SymbolId !Int !Int !Int !String -> (!Bool, !Int, !Symbol);
FindId1 symid start curpos curlen str
	| curpos >= curlen		= FindReserved symid start curpos curlen str;
	| LowerCase curchar		= FindId1 symid start curpos` curlen str;
	| UpperCase curchar		= FindId1 symid start curpos` curlen str;
	| Digit curchar			= FindId1 symid start curpos` curlen str;
	| Special curchar		= FindId1 symid start curpos` curlen str;
							= FindReserved symid start curpos curlen str;
	where 
		curchar					= str.[curpos];
		curpos`					= inc curpos;

eq3chars1 s i c1 c2 c3
	:== s.[i+1]==c1 && s.[i+2]==c2 && s.[i+3]==c3
eq4chars1 s i c1 c2 c3 c4
	:== s.[i+1]==c1 && s.[i+2]==c2 && s.[i+3]==c3 && s.[i+4]==c4
eq5chars1 s i c1 c2 c3 c4 c5
	:== s.[i+1]==c1 && s.[i+2]==c2 && s.[i+3]==c3 && s.[i+4]==c4 && s.[i+5]==c5
eq6chars1 s i c1 c2 c3 c4 c5 c6
	:== s.[i+1]==c1 && s.[i+2]==c2 && s.[i+3]==c3 && s.[i+4]==c4 && s.[i+5]==c5 && s.[i+6]==c6
eq7chars1 s i c1 c2 c3 c4 c5 c6 c7
	:== s.[i+1]==c1 && s.[i+2]==c2 && s.[i+3]==c3 && s.[i+4]==c4 && s.[i+5]==c5 && s.[i+6]==c6 && s.[i+7]==c7

FindReserved :: !SymbolId !Int !Int !Int !String -> (!Bool, !Int, !Symbol);
FindReserved symid start curpos curlen str
	| start>=curpos
		= (True,curpos,{repr=symid, string=str % (start,dec curpos)});
	# first_char = str.[start];
	| first_char=='f' && start+4==curpos && eq3chars1 str start 'r' 'o' 'm'
		= (True,curpos,FromSym);
	| first_char=='i' && start+6==curpos && eq5chars1 str start 'm' 'p' 'o' 'r' 't'
		= (True,curpos,ImportSym);
	| first_char=='i' && start+8==curpos && eq7chars1 str start 'n' 's' 't' 'a' 'n' 'c' 'e'
		= (True,curpos,InstanceSym);
	| first_char=='c' && start+5==curpos && eq4chars1 str start 'l' 'a' 's' 's'
		= (True,curpos,ClassSym);
	| first_char=='i' && start+5==curpos && eq4chars1 str start 'n' 'f' 'i' 'x'
		= (True,curpos,InfixSym);
	| first_char=='i' && start+6==curpos && eq4chars1 str start 'n' 'f' 'i' 'x' && (str.[start+5]=='l' || str.[start+5]=='r')
		= (True,curpos,InfixSym);
	| first_char=='w' && start+5==curpos && eq4chars1 str start 'h' 'e' 'r' 'e'
		= (True,curpos,WhereSym);
	| first_char=='g' && start+7==curpos && eq6chars1 str start 'e' 'n' 'e' 'r' 'i' 'c'
		= (True,curpos,{repr=GenericSymID,string=""});
	| first_char=='d' && start+6==curpos && eq5chars1 str start 'e' 'r' 'i' 'v' 'e'
		= (True,curpos,{repr=DeriveSymID,string=""});
		= (True,curpos,{repr=symid, string=str % (start,dec curpos)});

FindId2 :: !SymbolId !Int !Int !Int !String -> (!Bool, !Int, !Symbol);
FindId2 symid start curpos curlen str
	| curpos >= curlen		= (True, curpos, {repr=symid, string=str % (start,dec curpos)});
	| SpecialChar curchar	= FindId2 symid start curpos` curlen str;
							= (True, curpos, {repr=symid, string=str % (start,dec curpos)});
	where 
		curchar					= str.[curpos];
		curpos`					= inc curpos;

/* Aux. functions for parsing text in a file */

OpenTextFile :: !String !Files -> (!Files, !Bool, !*File)
OpenTextFile path files
	= (files`, ok, file)
where 
	(ok,file,files`)	= fopen path FReadData files

CloseTextFile :: !*File !Files -> Files
CloseTextFile file files
	= files`
where 
	(_,files`)	= fclose file files

StartInput :: !*File -> Input *File
StartInput file
	= {	offside	= offside,
		curpos	= curpos,
		curlen	= curlen,
		line	= line,
		file	= file`,
		linenr	= linenr,
		charnr	= charnr }
where 
	(_,curpos,curlen,line,file`,linenr,charnr)	= SkipLayOut1 False 0 0 0 "" file (-1) (-1);
	offside 									= ApplyLayOutRule curpos curlen line;

StartInput` :: !*{String} -> Input *{String}
StartInput` file
	= {	offside	= offside,
		curpos	= curpos,
		curlen	= curlen,
		line	= line,
		file	= file`,
		linenr	= linenr,
		charnr	= charnr }
where 
	(_,curpos,curlen,line,file`,linenr,charnr)	= SkipLayOut1` False 0 0 0 "" file (-1) (-1);
	offside 									= ApplyLayOutRule curpos curlen line;

ApplyLayOutRule :: !Int !Int !String -> Bool
ApplyLayOutRule curpos curlen str
	| curpos >= curlen	= True
	| curchar == ';'	= False
						= ApplyLayOutRule (inc curpos) curlen str
where 
		curchar	= str.[curpos]
	

EndInput :: !(Input *File) -> *File
EndInput {Input | file} = file

/*	Checks whether string at current input position matches a symbol , if so the corresponding symbol
	is returned, together with the new input skipped over the matched part, otherwise 'ErrorSym' or 'EofSym' is returned
*/
class ScanInput a
where
	ScanInput :: !(Input *a) -> (!Input *a, !Symbol, !Int, !Int)
	scan_module_symbol_from_input :: !(Input *a) -> (!Input *a, !Symbol, !Int, !Int)

instance ScanInput File
where
	ScanInput input=:{offside,curpos,curlen,line,file,linenr,charnr}
		#	(eof,curposa,curlen`,line`,file`,linenr`,charnr`)
					= SkipLayOut1 offside 0 curpos curlen line file linenr charnr;
		#	inputa	= {	offside	= offside, 
						curpos	= curposa,
						curlen	= curlen`,
						line	= line`,
						file	= file`,
						linenr	= linenr`,
						charnr	= charnr` };
		| eof	= (inputa, EofSym, linenr`, charnr`);
		#	(ok,curposb,sym)
					= FindSym curposa curlen` line`;
			inputb	= {	Input | inputa & curpos = curposb, charnr = charnr` + (curposb - curposa) };
		| ok	= (inputb, sym, linenr`, charnr`);
				= ScanInput inputb;
	scan_module_symbol_from_input {offside,curpos,curlen,line,file,linenr,charnr}
		#	(eof,curposa,curlen`,line`,file`,linenr`,charnr`)
				= SkipLayOut1 offside 0 curpos curlen line file linenr charnr;
		#	inputa	= {	offside	= offside, curpos = curposa, curlen	= curlen`, line	= line`, file = file`, linenr = linenr`, charnr	= charnr` };
		| eof	= (inputa, EofSym, linenr`, charnr`);
		#	(ok,curposb,sym)
				= find_module_symbol curposa curlen` line`;
			inputb	= {	Input | inputa & curpos = curposb, charnr = charnr` + (curposb - curposa) };
		| ok	= (inputb, sym, linenr`, charnr`);
				= scan_module_symbol_from_input inputb;

instance ScanInput {{#Char}}
where
	ScanInput input=:{offside,curpos,curlen,line,file,linenr,charnr}
		#	(eof,curposa,curlen`,line`,file`,linenr`,charnr`)
					= SkipLayOut1` offside 0 curpos curlen line file linenr charnr;
		#	inputa	= {	offside	= offside, 
						curpos	= curposa,
						curlen	= curlen`,
						line	= line`,
						file	= file`,
						linenr	= linenr`,
						charnr	= charnr` };
		| eof	= (inputa, EofSym, linenr`, charnr`);
		#	(ok,curposb,sym)
					= FindSym curposa curlen` line`;
			inputb	= {	Input | inputa & curpos = curposb, charnr = charnr` + (curposb - curposa) };
		| ok	= (inputb, sym, linenr`, charnr`);
				= ScanInput inputb;
	scan_module_symbol_from_input {offside,curpos,curlen,line,file,linenr,charnr}
		#	(eof,curposa,curlen`,line`,file`,linenr`,charnr`)
				= SkipLayOut1` offside 0 curpos curlen line file linenr charnr;
		#	inputa	= {	offside	= offside, curpos = curposa, curlen = curlen`, line = line`, file = file`, linenr = linenr`, charnr = charnr` };
		| eof	= (inputa, EofSym, linenr`, charnr`);
		#	(ok,curposb,sym)
				= find_module_symbol curposa curlen` line`;
			inputb	= {	Input | inputa & curpos = curposb, charnr = charnr` + (curposb - curposa) };
		| ok	= (inputb, sym, linenr`, charnr`);
				= scan_module_symbol_from_input inputb;

// Skips all layout (i.e. newlines, spaces and tabs), comments, string and character denotations

SkipLayOut1 ::	!Bool !Int !Int !Int !String !*File !Int !Int
				-> (!Bool, !Int, !Int, !String, !*File, !Int, !Int);
SkipLayOut1 offside nesting curpos curlen str text linenr charnr
	| curpos >= curlen
		= SkipToNewLine offside nesting text linenr charnr;
	# curchar = str.[curpos];
	| WhiteSpace curchar
		= SkipLayOut1 offside nesting (inc curpos) curlen str text linenr (inc charnr);
	# more2 = inc curpos < curlen;
	  nextchar = str.[inc curpos];
	| curchar=='/' && more2 && nextchar=='*'	
		= SkipLayOut1 offside (inc nesting) (curpos+2) curlen str text linenr (charnr+2);
	| curchar=='*' && more2 && nextchar=='/'
		# nesting` = if (nesting==0) nesting (dec nesting);
		= SkipLayOut1 offside nesting` (curpos+2) curlen str text linenr (charnr+2);
	| nesting > 0		
		= SkipLayOut1 offside nesting (inc curpos) curlen str text linenr (inc charnr);
	| curchar == '\"'
		# (curposq,curlenq,charnrq)	= SkipQuote (inc curpos) curlen str (inc charnr);
		= SkipLayOut1 offside nesting curposq curlenq str text linenr charnrq;
	| curchar=='/' && more2 && nextchar=='/'
		= SkipToNewLine offside nesting text linenr 0;
		= (False, curpos, curlen, str, text, linenr, charnr);
where
	SkipToNewLine ::	!Bool !Int !*File !Int !Int
						-> (!Bool, !Int, !Int, !String, !*File, !Int, !Int);
	SkipToNewLine offside nesting text linenr charnr
		#	(str,text)			= readLine text
		#	curlen				= size str
		| curlen==0
			= (True, 0, 0, "", text, linenr, charnr)
		#	linenr				= inc linenr
		#	newline				= str.[0]== '\n'
		| newline
			= SkipToNewLine offside nesting text linenr 0
		#	semicolon			= nesting == 0 && offside && curlen > 0 && NoLayOut (str.[0]) && NoComment str
		| semicolon
			= (False, 0, inc curlen, ";"+++str, text, linenr, -1)
		= SkipLayOut1 offside nesting 0 curlen str text linenr 0

SkipLayOut1` ::	!Bool !Int !Int !Int !String !*{String} !Int !Int
				-> (!Bool, !Int, !Int, !String, !*{String}, !Int, !Int);
SkipLayOut1` offside nesting curpos curlen str text linenr charnr
	| curpos >= curlen
		= SkipToNewLine` offside nesting text linenr charnr;
	# curchar = str.[curpos];
	| WhiteSpace curchar
		= SkipLayOut1` offside nesting (inc curpos) curlen str text linenr (inc charnr);
	# more2 = inc curpos < curlen;
	  nextchar = str.[inc curpos];
	| curchar=='/' && more2 && nextchar=='*'	
		= SkipLayOut1` offside (inc nesting) (curpos+2) curlen str text linenr (charnr+2);
	| curchar=='*' && more2 && nextchar=='/'
		# nesting` = if (nesting==0) nesting (dec nesting);
		= SkipLayOut1` offside nesting` (curpos+2) curlen str text linenr (charnr+2);
	| nesting > 0
		= SkipLayOut1` offside nesting (inc curpos) curlen str text linenr (inc charnr);
	| curchar == '\"'
		# (curposq,curlenq,charnrq)	= SkipQuote (inc curpos) curlen str (inc charnr);
		= SkipLayOut1` offside nesting curposq curlenq str text linenr charnrq;
	| curchar=='/' && more2 && nextchar=='/'
		= SkipToNewLine` offside nesting text linenr 0;
		= (False, curpos, curlen, str, text, linenr, charnr);
where
	SkipToNewLine` ::	!Bool !Int !*{String} !Int !Int
						-> (!Bool, !Int, !Int, !String, !*{String}, !Int, !Int);
	SkipToNewLine` offside nesting text linenr charnr
		#	(siz,text)			= usize text
		# eof = dec siz == linenr
		| eof
			= (True, 0, 0, "", text, linenr, charnr)
		#	linenr				= inc linenr
		#	(str,text)			= text![linenr]
		#	curlen				= size str
		#	newline				= curlen > 0 && str.[0]== '\n'
		| newline
			= SkipToNewLine` offside nesting text linenr 0
		#	semicolon			= nesting == 0 && offside && curlen > 0 && NoLayOut (str.[0]) && NoComment str
		| semicolon
			= (False, 0, inc curlen, ";"+++str, text, linenr, -1)
		= SkipLayOut1` offside nesting 0 curlen str text linenr 0

NoLayOut :: !Char -> Bool
NoLayOut c = c <> ' ' && c <> '\t'

NoComment :: !String -> Bool
NoComment s
	| size s <= 1
		= True
	# curchar = s.[0]
	  nextchar = s.[1]
	| curchar=='/' && (nextchar=='*' || nextchar=='/')
		= False
	| curchar=='*' && nextchar=='/'
		= False
	= True

SkipQuote :: !Int !Int !String !Int -> (!Int, !Int, !Int);
SkipQuote pos len str charnr
	| pos >= len		=  (pos,len,charnr);
	| curchar == '\"'	=  (pos`,len,charnr`);
	| curchar == '\\'	=  SkipQuote2 pos` len str charnr`;
						=  SkipQuote pos` len str charnr`;
	where 
		curchar		= str.[pos];
		pos`		= inc pos;
		charnr`		= inc charnr;

SkipQuote2	:: !Int !Int !String !Int -> (!Int, !Int, !Int);
SkipQuote2 pos len str charnr
	| pos >= len	=  (pos,len,charnr);
					=  SkipQuote (inc pos) len str (inc charnr);
					
/* Aux. function for testing whether strings occur as a substring */

IsSubStr :: !Int !Int String !Int !Int !String -> Bool;
IsSubStr subpos substop substr pos stop str
	| subpos >= substop	= pos >= stop;
	| pos >= stop		= False;
	| subc == c			= IsSubStr (inc subpos) substop substr (inc pos) stop str;
						= False;
	where 
		subc	= substr.[subpos];
		c		= str.[pos];

/* Aux. functions for testing whether characters are in certain classes */

SpecialChar	:: !Char -> Bool;
SpecialChar c =  pos < speciallen;
		where
		pos			= FindChar c special speciallen 0;
		special		= "~@#$%^?!+-*<>\\/|&=:.";
		speciallen	= size special;
	
FindChar	:: !Char !String !Int !Int -> Int;
FindChar c line linelen pos
	| pos >= linelen		=  pos;
	| c ==  line.[pos]		=  pos;
							=  FindChar c line linelen (inc pos);
	
//	LowerCase	:: !Char -> Bool;
	LowerCase c	:== 'a' <= c  &&  c <= 'z' ;
		
//	UpperCase	:: !Char -> Bool;
	UpperCase c	:== 'A' <= c  &&  c <= 'Z' ;
		
//	Digit		:: !Char -> Bool;
	Digit c		:== '0' <= c  &&  c <= '9';

//	Special		:: !Char -> Bool;
	Special c	:== c == '`' || c == '_'; 


//	WhiteSpace :: !Char -> Bool;
	WhiteSpace c
	:==	c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f';
	
//	IdChar :: !Char -> Bool;
	IdChar c
	:== LowerCase c || UpperCase c || Digit c || Special c;

// Check whether a clipboard selection is a valid Clean module identifier
	                   
CleanModId	:: !String -> Bool;
CleanModId id = IsModId 0 (size id) id;

is_h_module_name :: !String -> Bool;
is_h_module_name id
	# len=size id;
	| len==0
		= False;
	# c = id.[0];
	| LowerCase c || UpperCase c || c == '_'
		=  is_h_module_name1 1 len id;
	| SpecialChar c
		=  IsModId2 1 len id;
		=  False;

is_h_module_name1	:: !Int !Int !String -> Bool;
is_h_module_name1 pos len id
	| pos >= len
		= True;
	# c = id.[pos];
	| LowerCase c || UpperCase c || Digit c || Special c || c=='.'
		= is_h_module_name1 (inc pos) len id;
		= False;

IsModId	:: !Int !Int !String -> Bool;
IsModId pos len id	| pos >= len			=  False;
					| LowerCase curchar || UpperCase curchar || curchar == '_'
											=  IsModId1 (inc pos) len id;
					| SpecialChar curchar	=  IsModId2 (inc pos) len id;

											=  False;
		where 
		curchar= id.[pos];
		
IsModId1	:: !Int !Int !String -> Bool;
IsModId1 pos len id	| pos >= len			= True;
					| LowerCase curchar		= IsModId1 (inc pos) len id;
					| UpperCase curchar		= IsModId1 (inc pos) len id;
					| Digit curchar			= IsModId1 (inc pos) len id;
					| Special curchar		= IsModId1 (inc pos) len id;
											= False;
		where 
		curchar	= id.[pos];
		
IsModId2	:: !Int !Int !String -> Bool;
IsModId2 pos len id	| pos >= len			= True;
					| SpecialChar curchar	= IsModId2 (inc pos) len id;
											= False;
		where 
		curchar	= id.[pos];

// Checks whether string is a type specification
		
IsTypeSpec :: !String -> Bool;
IsTypeSpec str
	| comment < strlen	= True; 
						= cleanid < strlen && hastype < strlen;
	where 
	strlen		= size str;
	pos1		= SkipLayOutChars str strlen 0;
	comment		= HasSubStr 2 "//" strlen str pos1;
	(_,cleanid)	= FindCleanId str strlen pos1;
	hastype		= HasSubStr 2 "::" strlen str (inc cleanid);
	
HasSubStr :: !Int !String !Int !String !Int -> Int;
HasSubStr substrlen substr strlen str pos
	| pos + dec substrlen >= strlen							= strlen;
	| IsSubStr 0 substrlen substr pos (pos+substrlen) str	= pos;
															= HasSubStr substrlen substr strlen str (inc pos);

SkipLayOutChars :: !String !Int !Int -> Int;
SkipLayOutChars str strlen pos
	| pos >= strlen							= pos;
	| curchar == ' ' || curchar == '\t'		= SkipLayOutChars str strlen (inc pos);
											= pos;
	where 
	curchar	= str.[pos];

FindCleanId :: !String !Int !Int -> (!Int, !Int);
FindCleanId id idlen pos
	| pos >= idlen			= (idlen, idlen);
	#	curchar	= id.[pos];
		pos`	= inc pos;
	| LowerCase curchar		= (pos, FindCleanId1 id idlen pos`);
	| UpperCase curchar		= (pos, FindCleanId1 id idlen pos`);
	| SpecialChar curchar	= (pos, FindCleanId1 id idlen pos`);
	| Special curchar		= (pos, FindCleanId1 id idlen pos`);
	= (idlen, idlen);
where 
	FindCleanId1 :: !String !Int !Int -> Int;
	FindCleanId1 id idlen pos
		| pos >= idlen			= dec pos;
		#	curchar	= id.[pos];
			pos`	= inc pos;
		| LowerCase curchar		= FindCleanId1 id idlen pos`;
		| UpperCase curchar		= FindCleanId1 id idlen pos`;
		| Digit curchar			= FindCleanId1 id idlen pos`;
		| SpecialChar curchar	= FindCleanId1 id idlen pos`;
		| Special curchar		= FindCleanId1 id idlen pos`;
		= dec pos;
	
// Checks whether string is an error message
	
Error :: !String !Int !Int -> Int;
Error str strlen pos
	| pos + 4 >= strlen
		= strlen;
	| is_error
		= pos;
		= Error str strlen (inc pos);
	where 
		is_error = (str.[pos]=='e' || str.[pos]=='E')
					&& str.[pos+1]=='r' && str.[pos+2]=='r' && str.[pos+3]=='o' && str.[pos+4]=='r';

// Extracts the modulename out of a '<modulename> could not be imported' message

IsImportError13 :: !String -> (!Bool,!String);
IsImportError13 str
	#!	strlen		= size str;
	#!	error		= Error str strlen 0;
	#!	open		= FindOpenChar str strlen 0;
	#!	close		= FindCloseChar str strlen (inc open);
	#!	colon		= FindColonChar str strlen (inc close);
	#!	layout		= SkipLayOutChars str strlen (inc colon);
	#!	(id1,id2)	= FindCleanId str strlen layout;
	#!	pat			= "could not be imported";
	#!	notimport	= HasSubStr (size pat) pat strlen str (inc id2);
	#!	path		= str % (id1, id2);
	| error<strlen && id2 < strlen && notimport < strlen
		= (True,path);
	= (False,EmptyPathname);
	
// Extracts the modulename out of a 'could not open <modulename>' message

IsImportError20 :: !String -> (!Bool,!String);
IsImportError20 str
	#!	strlen		= size str;
	#!	error		= Error str strlen 0;
	#!	open		= FindOpenChar str strlen 0;
	#!	close		= FindCloseChar str strlen (inc open);
	#!	colon		= FindColonChar str strlen (inc close);
	#!	layout		= SkipLayOutChars str strlen (inc colon);
	#!	pat			= "could not open";
	#!	notimport	= HasSubStr (size pat) pat strlen str layout;
	#!	(id1,id2)	= FindCleanId str strlen (notimport + size pat + 1);
	#!	path		= str % (id1, id2);
	| error<strlen
	&& notimport < strlen
	&& id2 < strlen
		= (True,path);
	= (False,EmptyPathname);

//	FindOpenChar	:: !String !Int !Int -> Int;
FindOpenChar str len pos	:==  FindChar '[' str len pos;

//	FindCloseChar	:: !String !Int !Int -> Int;
FindCloseChar str len pos	:==  FindChar ']' str len pos;

//	FindCommaChar	:: !String !Int !Int -> Int;
FindCommaChar str len pos	:==  FindChar ',' str len pos;

//	FindQuoteChar	:: !String !Int !Int -> Int;
FindQuoteChar str len pos	:== FindChar '\"' str len pos;
	
//	FindColonChar	:: !String !Int !Int -> Int;
FindColonChar str len pos	:== FindChar ':' str len pos;
