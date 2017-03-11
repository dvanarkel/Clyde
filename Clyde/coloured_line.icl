implementation module Clyde.coloured_line

import StdEnv

attributeStrings :: ![String] -> [[(Range,Style)]]
attributeStrings lines
	#!	info			= firstParse lines
		astrings		= map string2attribString info
	= astrings

:: CommentLevel
	= N Int	// normal comment nesting level
	| L		// comment till end of line...
	| S 	// in string constant...
	| C		// in char constant...
	| T Int	// in typedef
	| D Int	// in typedecl

string2attribString :: (Info,String) -> [(Range,Style)]
string2attribString ({comment_level,is_typedef,is_typedecl},string)
	| is_typedef
		= doString 0 0 (T comment_level)
	| is_typedecl
		= doString 0 0 (D comment_level)
	= doString 0 0 (N comment_level)
where
	doString start scanstart cl = doString` scanstart cl
	where
		doString` start S		= dS start 								// string literal
		doString` start C		= dC start 								// char literal
		doString` start L		= [((start,string_size - 1),Comment)]	// line comment
		doString` start (N cl)	= dL (N cl) start 						// normal	(cl == 0 = text, > 0 block comment)
		doString` start (T cl)	= dL (T cl) start 						// typedef	(cl == 0 = text, > 0 block comment)
		doString` start (D cl)	= dL (D cl) start 						// typedecl	(cl == 0 = text, > 0 block comment)

		string_size	= size string
	
		funnyChar i = isStringMember string.[i] (dec funnySize) funnyChars

		isStringMember :: !Char !Int !{#Char} -> Bool
		isStringMember x i string
			| i < 0 = False
			| string.[i] == x = True
			= isStringMember x (dec i) string

		funnyChars = "~@#$%^?!+-*<>\\/|&=:."
		funnySize = 20

		scanfunny i
			| i >= string_size = string_size
			| funnyChar i = scanfunny (inc i)
			= i

		scankeyword :: !.String !Int -> (!Bool,!Int)
		scankeyword s i
			# c = s.[i]
			| not (isAlpha c || (c == '_'))
				# j = inc i
				= (False,j)
			# j = scanalpha (inc i)
			| c == 'f'	// from
				| (j == i+4) && (s%(i,i+3)=="from") = (True,j)
				= (False,j)
			| c == 'g'	// generic
				| (j == i+7) && (s%(i,i+6)=="generic") = (True,j)
				= (False,j)
			| c == 'd'	// definition, derive, dynamic
				| j == i+10 = (s % (i,i+9)=="definition",j)
				| j == i+6  = (s % (i,i+5)=="derive",j)
				| j == i+7  = (s % (i,i+6)=="dynamic",j)
				= (False,j)
			| c == 'i'	// implementation, import, if, in, infix, infixl, infixr, instance
				| (j == i+14) && (s%(i,i+13)=="implementation") = (True,j)
				| (j == i+8) && (s%(i,i+7)=="instance") = (True,j)			// only in typedef!
				| (j == i+6) && (s%(i,i+5)=="import") = (True,j)
				| (j == i+6) && (s%(i,i+5)=="infixl") = (True,j)
				| (j == i+6) && (s%(i,i+5)=="infixr") = (True,j)
				| (j == i+5) && (s%(i,i+4)=="infix") = (True,j)
				| (j == i+2) && (s%(i,i+1)=="if") = (True,j)
				| (j == i+2) && (s%(i,i+1)=="in") = (True,j)
				= (False,j)
			| c == 'e'	// export
				| (j == i+6) && (s%(i,i+5)=="export") = (True,j)
				= (False,j)
			| c == 'm'	// module
				| (j == i+6) && (s%(i,i+5)=="module") = (True,j)
				= (False,j)
			| c == 's'	// system
				| (j == i+6) && (s%(i,i+5)=="system") = (True,j)
				= (False,j)
			| c == 'c'	// case, code, class
				| (j == i+5) && (s%(i,i+4)=="class") = (True,j)
				| (j == i+4)
					| (s%(i,i+3)=="case") = (True,j)
					| (s%(i,i+3)=="code") = (True,j)
					= (False,j)
				= (False,j)
			| c == 'l'	// let, let!
				| (j == i+4) && (s%(i,i+3)=="let!") = (True,j)				// doesn't work! (and doesn't exist in Clean anymore?)
				| (j == i+3) && (s%(i,i+2)=="let") = (True,j)
				= (False,j)
			| c == 'o'	// of
				| (j == i+2) && (s%(i,i+1)=="of") = (True,j)
				= (False,j)
			| c == 'w'	// where, with
				| (j == i+4) && (s%(i,i+3)=="with") = (True,j)
				| (j == i+5) && (s%(i,i+4)=="where") = (True,j)
				= (False,j)
			= (False,j)

		scanalpha i
			| i >= string_size = string_size
			# c = string.[i]
			| isAlphanum c = scanalpha (inc i)
			| c == '_' = scanalpha (inc i)
			| c == '`' = scanalpha (inc i)
			= i

		dS i
			#	i`	= inc i
				i``	= inc i`
			| i >= string_size
				= [((start,string_size-1),StringLit)]		// multi-line string literals?
			| string.[i] == '"'
				= [ ((start,i),StringLit) ] ++ doString i` i` (N 0)
			| string.[i] == '\\'
				| i` >= string_size
					= [((start,string_size-1),StringLit)]		// multi-line string literals?
				= dS i``
			= dS i`
	
		dC i
			#	i`	= inc i
				i``	= inc i`
			| i >= string_size
				= [((start,string_size-1),CharsLit)]
			| string.[i] == '\''
				= [((start,i),CharsLit)] ++ doString i` i` (N 0)
			| string.[i] == '\\'
				| i` >= string_size
					= [((start,string_size-1),CharsLit)]
				= dC i``
			= dC i`
	
		dL cl i
			#	i`	= inc i
				i``	= inc i`
			| i >= string_size
				= [((start,string_size-1),toStyle cl)]
			| string.[i] == '*'
				| i` >= string_size
					= [((start,string_size-1),toStyle cl)]
				| string.[i`] == '/'
					| in_comment cl
						#	cl	= dec_comment cl
						| not (in_comment cl)
							= [((start,i`),Comment)] ++ doString i`` i`` cl
						= dL cl i``
					| i`` < string_size && funnyChar i``
						// eat till end of funnyid substring...
						#	j	= scanfunny i``
						= dL cl j
					= [((start,i`),toStyle cl)] ++ doString i`` i`` (dec_comment cl)	// entering negative comment level now...
				= dL cl i`
			| string.[i] == '/'
				| i` >= string_size
					= [((start,string_size-1),toStyle cl)]
				| string.[i`] == '/'
					| i > start
						= [((start,i-1),toStyle cl) , ((i,string_size-1),Comment)]
					= [((i,string_size-1),Comment)]
				| string.[i`] == '*'
					#	cl`	= inc_comment cl
					| in_comment cl == in_comment cl`
						= dL cl` i``
					| in_comment cl
						// we were in a comment but no longer...
						= [((start,i-1),toStyle cl)] ++ doString i i`` cl`
					= [((start,i-1),toStyle cl)] ++ doString i i`` cl`
				= dL cl i`
			| string.[i] == '"' && not (in_comment cl)
				= [((start,i-1),toStyle cl)] ++ doString i i` S
			| string.[i] == '\'' && not (in_comment cl)
				= [((start,i-1),toStyle cl)] ++ doString i i` C
			| (not (in_comment cl)) && funnyChar i
				#	j	= scanfunny i
				= dL cl j
			#	(key,j)	= scankeyword string i
			| key && not (in_comment cl)
				= [((start,i-1),toStyle cl),((i,j-1),Keyword)] ++ doString j j cl
			= dL cl j
	
	toStyle (N 0)	= Normal
	toStyle (N cl)	= Comment
	toStyle (T 0)	= Typedef
	toStyle (T cl)	= Comment
	toStyle (D 0)	= Typedecl
	toStyle (D cl)	= Comment

	in_comment cl = case cl of
		N l	-> l <> 0
		T l	-> l <> 0
		D l -> l <> 0
		_	-> False

	dec_comment cl = case cl of
		N l	-> N (dec l)
		T l	-> T (dec l)
		D l -> D (dec l)

	inc_comment cl = case cl of
		N l	-> N (inc l)
		T l	-> T (inc l)
		D l -> D (inc l)

:: Info	=
	{ comment_level	:: !Int			// comment nesting level at start of line
	, is_typedef	:: !Bool		// is typedef line
	, is_typedecl	:: !Bool		// is typedecl line
	, offside_level	:: !Int			// context offside level
	, flush			:: !Bool		// flush accu
	}

:: State = 
	{ level			:: !Int		// comment nesting level at start of line
	, typedef		:: !Bool		// in typedef at start of line
	, typedecl		:: !Bool		// in typedecl at start of line
	, offside		:: !Int		// typedecl offside level
	, parse_state	:: !ParseState
	, has_content	:: !Bool
	}

:: ParseState = StartOfBlock | CleanId | OpenPar | InfixId | Precedence | Fixity | Other

iniState
	=
		{ level			= 0
		, typedef		= False
		, typedecl		= False
		, offside		= 0
		, parse_state	= StartOfBlock
		, has_content	= False
		}

instance == ParseState where
	(==) StartOfBlock	StartOfBlock	= True
	(==) CleanId		CleanId			= True
	(==) OpenPar		OpenPar			= True
	(==) InfixId		InfixId			= True
	(==) Precedence		Precedence		= True
	(==) Fixity			Fixity			= True
	(==) Other			Other			= True
	(==) _				_				= False


/*
	firstParse: textlines -> zip initial comment nesting level & textlines
*/

firstParse :: ![String] -> [(!Info,!String)]
firstParse lines
	# parsed_lines	= parse iniState lines
	= backpatch iniState id [] parsed_lines
where
	parse :: State [String] -> [(State,String)]
	parse state []
		= []
	parse state [line:lines]
		# state	= parseLine state line
		= [(state,line) : parse state lines]

	backpatch :: State ([(Info,String)] -> [(Info,String)]) [(Info,String)] [(State,String)] -> [(Info,String)]
	backpatch state res acc []
		= res acc
	backpatch state res acc old=:[(state`,line):lines]
		# flush	= state`.has_content || (not (state.typedecl) && state`.typedecl)
		#! info = {	comment_level=state.level,
					is_typedef=state`.typedef,
					is_typedecl=state`.typedecl,
					offside_level=state`.offside,
					flush=flush}
		# info_and_line	= (info,line)
		| state`.has_content
			= backpatch state` (copy res acc) [info_and_line] lines
		| not (state.typedecl) && state`.typedecl
			= backpatch state` (patch res acc) [info_and_line] lines
		= backpatch state` res (accum acc info_and_line) lines

copy res acc rest
	= res (acc ++ rest)

patch res acc rest
	= res ((map (\ (info,l) = ({info & is_typedef=False,is_typedecl=True},l)) acc) ++ rest)

accum acc info
	= acc ++ [info]

parseLine state=:{level,typedef, typedecl,offside,parse_state} line
	#! (index,indent,level)	= scanFirst level line
	#! (typedecl,offside`)	= if typedecl
								(if (index < line_size && indent >= 0 && indent <= offside) 
									(False,indent)
									(True,offside)
								)
								(False,if (indent >= 0) (case parse_state of
										OpenPar	-> offside
										InfixId	-> offside
										Fixity	-> offside
										Precedence	-> offside
										CleanId	-> offside
										_		-> indent
									)
									offside)
	#! parse_state			= if (indent==offside`)
								(case parse_state of
									OpenPar		-> OpenPar
									InfixId		-> InfixId
									Fixity		-> Fixity
									Precedence	-> Precedence
									CleanId		-> CleanId
									_			-> StartOfBlock
								)
								parse_state
	#! typedef				= if (index==0 && indent >= 0 && not (whiteChar line.[0])) False typedef
	#! has_content			= indent >= 0 && index < line_size
	#  not_double_colon		= line%(index,dec (scanfunny index line_size line)) <> "::"
	   not_typedecl_prefix	= parse_state == StartOfBlock
	#! has_content			= if (index>0)
								(has_content && not_double_colon && not_typedecl_prefix)
								has_content
	#! state				=	{state 
								& level=level
								, typedef=typedef
								, typedecl=typedecl
								, offside=offside`
								, parse_state=parse_state
								, has_content=has_content
								}
	= pL state index
where
	// e.g. need to check for where and let here...
	line_size = size line

	(arggh) :: Int -> Int
	(arggh) i = i + 1

	pL state=:{level,parse_state} i								// parse normal text
		| i >= line_size		= state
		# end					= getToken level i line line_size
		# token					= line%(i,dec end)
		= case token of
			"/*"				-> pL {state & level = inc level} end	// BC
			"*/"				-> pL {state & level = dec level} end	// EC
			"//"				-> state			// LC
			"::"				-> if (level==0)	// FI special case...
									 (case parse_state of
									 	StartOfBlock
									 		| i == 0
									 				-> pL {state & typedef = True, parse_state = Other} end
									 				-> pL {state & parse_state = Other} end
									 	CleanId		-> pL {state & typedecl = True, parse_state = Other} end
									 	Fixity		-> pL {state & typedecl = True, parse_state = Other} end
									 	Precedence	-> pL {state & typedecl = True, parse_state = Other} end
									 	_			-> pL {state & parse_state = Other} end
									 )
									 (pL state end)
			"where"				-> if (level==0)
									(pL {state & parse_state = Other} end)
									(pL state end)
			"let"				-> if (level==0)
									(pL {state & parse_state = Other} end)
									(pL state end)
			"infix"				-> if (level==0) 	// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"infixl"			-> if (level==0) 	// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"infixr"			-> if (level==0) 	// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"("					-> if (level==0)	// OP .. CP
									(if (parse_state==StartOfBlock)
										(pL {state & parse_state = OpenPar} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			")"					-> if (level==0)	// CP
									(if (parse_state==InfixId)
										(pL {state & parse_state = CleanId} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			_
							| isDigit line.[i]
								-> if (level==0)
									(if (parse_state==Fixity)
										(pL {state & parse_state = Precedence} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
							| isLower line.[i] || isUpper line.[i] || funnyChar line.[i]
								-> if (level==0)
									(if (parse_state==StartOfBlock)
										(pL {state & parse_state = CleanId} end)
										(if (parse_state==OpenPar)
											(pL {state & parse_state = InfixId} end)
											(pL {state & parse_state = Other} end)
										)
									)
									(pL state end)
							| whiteChar line.[i]
								->	(pL state end)
							// otherwise
								->	if (level==0)
										(pL {state & parse_state = Other} end)
										(pL state end)

// rework scanFirst to use getToken?!

scanFirst :: !Int !.String -> (!Int,!Int,!Int)
scanFirst level line = scanFirst level 0 0 line
where
	line_size	= size line

	scanFirst :: !Int !Int !Int !.String -> (!Int,!Int,!Int)
	// commentlevel index indent line -> (first_index,first_indent,commentlevel)
	scanFirst level index indent line
		| index >= line_size	= (index,-1,level)
		# char = line.[index]
		| char == ' '			= scanFirst level (inc index) (inc indent) line
		| char == '\t'			= scanFirst level (inc index) ((inc (indent >> 2)) << 2) line	// assumes tab=4
		| char == '\n'			= scanFirst level (inc index) indent line
		| char == '\r'			= scanFirst level (inc index) indent line
		| char == '\f'			= scanFirst level (inc index) indent line
		| char == '*'
			# index` = inc index
			  indent` = inc indent
			| index` >= line_size	= (index,indent,level)
			| line.[index`] == '/'
				# index`` = inc index`
				  indent`` = inc indent`
				| level <> 0	= scanFirst (dec level) index`` indent`` line	// try to fix problem below
				| index`` >= line_size	= (index``,-1,dec level)
				| funnyChar line.[index``]
					= (index,indent,level)					// hmmm excludes */*/ and *//*...*/
				= scanFirst (dec level) index`` indent`` line
			| level == 0
				= (index,indent,level)
			= scanFirst level index` indent` line
		| char == '/'
			# index` = inc index
			  indent` = inc indent
			| index` >= line_size	= (index,indent,level)
			# char` = line.[index`]
			| char` == '/'		= (index,-1,level)					// shouldn't we exclude funnyId's ??
			| char` == '*'		= scanFirst (inc level) (inc index`) (inc indent`) line
			| level == 0
				= (index,indent,level)
			= scanFirst level index` indent` line
		| level <> 0
			= scanFirst level (inc index) (inc indent) line
		= (index,indent,level)

////////////

isStringMember :: !Char !Int !String -> Bool
isStringMember x i s
	| i < 0 = False
	#! c = s.[i]
	| c == x = True
	= isStringMember x (dec i) s

funnyChar c = isStringMember c (dec funnySize) funnyChars
where
	funnyChars	= "~@#$%^?!+-*<>\\/|&=:."
	funnySize	= 20	// =: size funnyChars?

scanfunny :: !Int !Int !String -> Int
scanfunny i line_size line
	| i >= line_size = line_size
	| funnyChar line.[i] = scanfunny (inc i) line_size line
	= i

cleanChar c = isLower c || isUpper c || isDigit c || c == '_' || c == '`'

scanclean :: !Int !Int !String -> Int
scanclean i line_size line
	| i >= line_size = line_size
	| cleanChar line.[i] = scanclean (inc i) line_size line
	= i

whiteChar c = isStringMember c (dec whiteSize) whiteChars
where
	whiteChars	= " \t\f\n\r"
	whiteSize	= 5

scanwhite :: !Int !Int !String -> Int
scanwhite i line_size line
	| i >= line_size = line_size
	| whiteChar line.[i] = scanwhite (inc i) line_size line
	= i

getToken :: !Int !Int !String !Int -> Int
getToken level index line line_size
	| index >= line_size	= line_size
	#! char					= line.[index]
	#! i = inc index
	| char == '*'
		| i >= line_size
			= line_size
		| line.[i] == '/'
			#! i = inc i
			| level <> 0
				= i
			= scanfunny i line_size line
		= scanfunny i line_size line
	| char == '/'
		| i >= line_size
			= line_size
		#! char = line.[i]
		   i	= inc i
		| char == '/'
			= i
		| char == '*'
			= i
		= scanfunny i line_size line
	| (char == '"') && (level == 0)
		= pS i
	| (char == '\'') && (level == 0)
		= pC i
	| (level == 0) && (funnyChar char)
		= scanfunny i line_size line
	| isLower char || isUpper char
		= scanclean i line_size line
	| whiteChar char
		= scanwhite i line_size line
	= i
where
	pS i										// parse string constant
		| i >= line_size		= line_size				// unterminated string constant...
		# char = line.[i]
		| char == '"'			= (inc i)
		| char == '\\'			= pS (i + 2)
		= pS (inc i)

	pC i										// parse character constant
		| i >= line_size		= line_size				// unterminated char constant...
		# char = line.[i]
		| char == '\''			= (inc i)
		| char == '\\'			= pC (i + 2)
		= pC (inc i)



/*

/*
* showTabs: show coloured '~' where tab character is could be tricky in Cocoa text system
* 
* now want to determine [(textrange,attribute)] (efficiently) for text
*/

// based on EdLook
###
// draw lines with syntax colouring
drawLinesC firstLine lastLine fontInfo text picture
	= drawTextLines firstLine text picture
where
//		drawTextLines :: Int Text *Picture -> *Picture
	drawTextLines f text picture
	  | f > lastLine
		= picture
	  #	(line,text)		= getLineC f text
	  	picture			= tabDrawStringC {x=0,y=y} line fontInfo picture
	  = drawTextLines (inc f) text picture

// draw lines plain
drawLinesP firstLine lastLine fontInfo text picture
	#	(lines, _)	= getLines firstLine lastLine text
	= drawTextLines lines picture
where
	drawTextLines :: (StrictList String) *Picture -> *Picture
	drawTextLines SNil picture
	  = picture
	drawTextLines (SCons string strings) picture
	  # picture			= tabDrawStringP { x=0,y=y } string fontInfo picture
	  = drawTextLines strings picture


// from EdTab

:: CommentLevel
	= N Int	// normal comment nesting level
	| L		// comment till end of line...
	| S 	// in string constant...
	| C		// in char constant...
	| T Int	// in typedef
	| D Int	// in typedecl

//import ospicture				// for optimized drawfuns...
optDrawS :== pictdrawstring		// use non-optimised versions
optDrawC :== pictdrawchar		// "

tabDrawStringC :: !Point2 !(!Info,!String) !FontInfo !*Picture -> *Picture
tabDrawStringC point ({comment_level=clevel,is_typedef=typedef,is_typedecl=typedecl},string)
	{tabSize,charWidth,thefont, showTabs, syntaxColours={textColour, backgroundColour,tabColour, commentColour, stringColour, charColour, keywordColour, typedefColour, typedeclColour}}
	picture
	#! strings = splitAtTabs string
	| typedef
		= tabDrawString` /*True*/ (T clevel) point strings picture
	| typedecl
		= tabDrawString` (D clevel) point strings picture
	= tabDrawString` /*True*/ (N clevel) point strings picture
where	  
	tabDrawString` :: /*!Bool*/ !CommentLevel !Point2 !.[String] !*Picture -> *Picture
	// hmm, need to get if column 0 into local funs...
	tabDrawString` /*_*/ _ _ [] picture
		= picture

	tabDrawString` /*ini*/ clevel point [string : []] picture
		#! picture					 = setPenPos point picture
		#! (_,picture)				 = drawC /*ini*/ clevel string picture
		= picture
	  
	tabDrawString` /*ini*/ clevel point [string : strings] picture
		#! picture					 = setPenPos point picture
		#! (clevel,picture)			 = drawC /*ini*/ clevel string picture
		#! (newPoint,picture)		 = getPenPos picture
		#! newX						 = alignAtTab` newPoint.x tabSize charWidth
		| not showTabs
		 	= tabDrawString` /*False*/ clevel {point & x = newX} strings picture
		#! picture					 = setPenColour tabColour picture
		#! picture					 = optDrawC '~' picture
		#! picture					 = setPenColour textColour picture
		= tabDrawString` /*False*/ clevel {point & x = newX} strings picture

	drawC :: /*!Bool*/ !CommentLevel !.String !*Picture -> (!CommentLevel,!*Picture)
	drawC /*ini*/ c s pic
		= drawC c pic
	where
		drawC :: !CommentLevel !*Picture -> (!CommentLevel,!*Picture)
		drawC S pic	// string literal
			# pic = setPenColour stringColour pic
			= dS 0 pic
		drawC C pic	// char literal
			# pic = setPenColour charColour pic
			= dC 0 pic
		drawC L pic	// line comment
			# pic = setPenColour commentColour pic
			# pic = optDrawS s pic
			= (L,pic)
		drawC (N cl) pic	// normal
			# pic = (if (cl==0) (setPenColour textColour) (setPenColour commentColour)) pic
			= dL /*ini*/ (N cl) 0 pic
		drawC (T cl) pic
			# pic = (if (cl==0) (setPenColour typedefColour) (setPenColour commentColour)) pic
			= dL /*ini*/ (T cl) 0 pic
		drawC (D cl) pic
			# pic = (if (cl==0) (setPenColour typedeclColour) (setPenColour commentColour)) pic
			= dL (D cl) 0 pic

		l = size s
		funnyChar i = isStringMember s.[i] (dec funnySize) funnyChars

		isStringMember :: !Char !Int !{#Char} -> Bool
		isStringMember x i s
			| i < 0 = False
			| s.[i] == x = True
			= isStringMember x (dec i) s

		funnyChars = "~@#$%^?!+-*<>\\/|&=:."
		funnySize = 20

		dL :: /*!Bool*/ !CommentLevel !.Int !*Picture -> (!CommentLevel,!*Picture)
		dL /*ini*/ cl i pic
			| i >= l
				= (cl,pic)
/*
			| ini && s.[i] == ':' && not (in_comment cl)
				# i` = inc i
				| i` >= l
					# (cl,pic) = normalise ini i cl pic
					# pic = optDrawC ':' pic
					= (N 0,pic)
				| s.[i`] == ':'
					# i`` = inc i`
					| i`` < l && funnyChar i``
						# j = scanfunny i
						# r = s%(i,dec j)
						# pic = optDrawS r pic
						= dL False (N 0) j pic
					# pic = setPenColour typedefColour pic
					# pic = optDrawS "::" pic
					= dL False (T 0) i`` pic
				| s.[i`] == '='
					# i`` = inc i`
					| i`` < l && funnyChar i``
						# j = scanfunny i
						# r = s%(i,dec j)
						# (cl,pic) = case r of
							":=="	-> (cl,pic)
							_		-> normalise ini i cl pic
						# pic = optDrawS r pic
						= dL False cl j pic
					# (cl,pic) = normalise ini i cl pic
					# pic = optDrawS ":=" pic
					= dL False (N 0) i`` pic
				# (cl,pic) = normalise ini i cl pic
				# pic = optDrawC ':' pic
				= dL False (N 0) i` pic
*/
			| s.[i] == '*'
				# i` = inc i
				| i` >= l
//					# (cl,pic) = normalise ini i cl pic
					# pic = optDrawC '*' pic
					= (cl,pic)
				| s.[i`] == '/'
					# i`` = inc i`
					| in_comment cl	//cl <> 0
						# pic = setPenColour commentColour pic	// idiot proof for trickery at start of text...
						# pic = optDrawS "* /" pic
						# cl = dec_comment cl
						| not (in_comment cl)	//cl == 0
							# pic = setPenColour (non_comment_colour cl)/*textColour*/ pic
							= dL /*False*/ cl i`` pic
						= dL /*False*/ cl i`` pic
						
					| i`` < l && funnyChar i``
						// eat till end of funnyid substring...
						# j = scanfunny i``
						# r = s%(i``,dec j)
//						# (cl,pic) = normalise ini i cl pic
						# pic = optDrawS "* /" pic
						# pic = optDrawS r pic
						= dL /*False*/ cl j pic
					# pic = setPenColour commentColour pic		// idiot proof for trickery at start of text...
					# pic = optDrawS "* /" pic
					# cl = dec_comment cl
					| not (in_comment cl)	//cl == 0
						# pic = setPenColour (non_comment_colour cl)/*textColour*/ pic
						= dL /*False*/ cl i`` pic
					= dL /*False*/ cl i`` pic
//				# (cl,pic) = normalise ini i cl pic
				# pic = optDrawC '*' pic
				= dL /*False*/ cl i` pic
			| s.[i] == '/'
				# i` = inc i
				| i` >= l
//					# (cl,pic) = normalise ini i cl pic
					# pic = optDrawC '/' pic
					= (cl,pic)
				| s.[i`] == '/'
					# pic = setPenColour commentColour pic
					# pic = optDrawS "//" pic
					# r = s%(inc i`,l)
					# pic = optDrawS r pic
					= (L,pic)
				| s.[i`] == '*'
					# pic = setPenColour commentColour pic
					# pic = optDrawS "/ *" pic
					# cl = inc_comment cl
					| in_comment cl
						= dL cl (inc i`) pic
					# pic = setPenColour (non_comment_colour cl) pic
					= dL /*False*/ cl (inc i`) pic
//				# (cl,pic) = normalise ini i cl pic
				# pic = optDrawC '/' pic
				= dL /*False*/ cl i` pic
			| (s.[i] == '"') && (not (in_comment cl))	//(cl == 0)
				# pic = setPenColour stringColour pic
				# pic = optDrawC '"' pic
				= dS (inc i) pic
			| (s.[i] == '\'') && (not (in_comment cl))	//(cl == 0)
				# pic = setPenColour charColour pic
				# pic = optDrawC '\'' pic
				= dC (inc i) pic

			| /*(cl == 0)*/ (not (in_comment cl)) && (funnyChar i)
				# j = scanfunny i
				# r = s%(i,dec j)
/*
				# (cl,pic) = case r of
								"|"	-> (cl,pic)
								"=" -> (cl,pic)
								_	-> normalise ini i cl pic
*/
				# pic = optDrawS r pic
				= dL /*False*/ cl j pic
/*
			# (cl,pic) = case WhiteSpace s.[i] of
							True -> (cl,pic)
							_	 -> normalise ini i cl pic
*/
			# (key,j) = scankeyword s i
			| key && (not (in_comment cl))	//cl == 0
				# r = s%(i,dec j)
				# (c,pic) = getPenColour pic
				# pic = setPenColour keywordColour pic
				# pic = optDrawS r pic
				# pic = setPenColour c pic
				= dL /*False*/ cl j pic
			# r = s%(i,dec j)
			# pic = optDrawS r pic
			= dL /*False*/ cl j pic
		where
/*
			normalise True 0 (T 0) pic
				# pic = setPenColour textColour pic
				= (N 0,pic)
			normalise _ _ cl pic
				= (cl,pic)
			in_typedef cl = case cl of
				T l	-> l == 0
				_	-> False
*/
			in_comment cl = case cl of
				N l	-> l <> 0
				T l	-> l <> 0
				D l -> l <> 0
				_	-> False

			dec_comment cl = case cl of
				N l	-> N (dec l)
				T l	-> T (dec l)
				D l -> D (dec l)

			inc_comment cl = case cl of
				N l	-> N (inc l)
				T l	-> T (inc l)
				D l -> D (inc l)

			non_comment_colour cl = case cl of
				N _	-> textColour
				T _	-> typedefColour
				D _ -> typedeclColour

			scankeyword :: !.String !Int -> (!Bool,!Int)
			scankeyword s i
				# c = s.[i]
				| not (isAlpha c || (c == '_'))
					# j = inc i
					= (False,j)
				# j = scanalpha (inc i)
				| c == 'f'	// from
					| (j == i+4) && (s%(i,i+3)=="from") = (True,j)
					= (False,j)
				| c == 'g'	// generic
					| (j == i+7) && (s%(i,i+6)=="generic") = (True,j)
					= (False,j)
				| c == 'd'	// definition, derive, dynamic
					| j == i+10 = (s % (i,i+9)=="definition",j)
					| j == i+6  = (s % (i,i+5)=="derive",j)
					| j == i+7  = (s % (i,i+6)=="dynamic",j)
					= (False,j)
				| c == 'i'	// implementation, import, if, in, infix, infixl, infixr, instance
					| (j == i+14) && (s%(i,i+13)=="implementation") = (True,j)
					| (j == i+8) && (s%(i,i+7)=="instance") = (True,j)			// only in typedef!
					| (j == i+6) && (s%(i,i+5)=="import") = (True,j)
					| (j == i+6) && (s%(i,i+5)=="infixl") = (True,j)
					| (j == i+6) && (s%(i,i+5)=="infixr") = (True,j)
					| (j == i+5) && (s%(i,i+4)=="infix") = (True,j)
					| (j == i+2) && (s%(i,i+1)=="if") = (True,j)
					| (j == i+2) && (s%(i,i+1)=="in") = (True,j)
					= (False,j)
				| c == 'e'	// export
					| (j == i+6) && (s%(i,i+5)=="export") = (True,j)
					= (False,j)
				| c == 'm'	// module
					| (j == i+6) && (s%(i,i+5)=="module") = (True,j)
					= (False,j)
				| c == 's'	// system
					| (j == i+6) && (s%(i,i+5)=="system") = (True,j)
					= (False,j)
				| c == 'c'	// case, code, class
					| (j == i+5) && (s%(i,i+4)=="class") = (True,j)
					| (j == i+4)
						| (s%(i,i+3)=="case") = (True,j)
						| (s%(i,i+3)=="code") = (True,j)
						= (False,j)
					= (False,j)
				| c == 'l'	// let, let!
					| (j == i+4) && (s%(i,i+3)=="let!") = (True,j)				// doesn't work!
					| (j == i+3) && (s%(i,i+2)=="let") = (True,j)
					= (False,j)
				| c == 'o'	// of
					| (j == i+2) && (s%(i,i+1)=="of") = (True,j)
					= (False,j)
				| c == 'w'	// where, with
					| (j == i+4) && (s%(i,i+3)=="with") = (True,j)
					| (j == i+5) && (s%(i,i+4)=="where") = (True,j)
					= (False,j)
				= (False,j)
			scanalpha i
				| i >= l = l
				# c = s.[i]
				| isAlphanum c = scanalpha (inc i)
				| c == '_' = scanalpha (inc i)
				| c == '`' = scanalpha (inc i)
				= i
			scanfunny i
				| i >= l = l
				| funnyChar i = scanfunny (inc i)
				= i
			
		dS :: !Int !*Picture -> (!CommentLevel,!*Picture)
		dS i pic
			| i >= l
				= (S,pic)
			| s.[i] == '"'
				# pic = optDrawC '"' pic
				# pic = setPenColour textColour pic
				= dL /*False*/ (N 0) (inc i) pic
			| s.[i] == '\\'
				# pic = optDrawC '\\' pic
				# i = inc i
				| i >= l
					= (S,pic)
				# pic = optDrawC s.[i] pic
				= dS (inc i) pic
			# pic = optDrawC s.[i] pic
			= dS (inc i) pic

		dC :: !Int !*Picture -> (!CommentLevel,!*Picture)
		dC i pic
			| i >= l
				= (C,pic)
			| s.[i] == '\''
				# pic = optDrawC '\'' pic
				# pic = setPenColour textColour pic
				= dL /*False*/ (N 0) (inc i) pic
			| s.[i] == '\\'
				# pic = optDrawC '\\' pic
				# i = inc i
				| i >= l
					= (C,pic)
				# pic = optDrawC s.[i] pic
				= dC (inc i) pic
			# pic = optDrawC s.[i] pic
			= dC (inc i) pic

WhiteSpace c
:==	c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f';

/////////////

splitAtTabs :: !String -> .[String]
splitAtTabs string
  = splitAtTabs` 0 0
where
	maxIndex = size string - 1

	splitAtTabs` startIndex currentIndex
		| currentIndex > maxIndex					// has the end been reached?
			= [ string % (startIndex, maxIndex) ]
		# newIndex = currentIndex + 1
		| string.[currentIndex] == '\t'				// is the current character a tab character?
			= [ string % (startIndex, currentIndex - 1)
			  : splitAtTabs` newIndex newIndex
			  ]
		// no, it's a normal character
		= splitAtTabs` startIndex newIndex

alignAtTab` x tabSize charWidth
	| tabSize <= 0
		= x + charWidth	// sensible result for silly value
	= tabWidth * (inc (x / tabWidth))
where
	tabWidth	= tabSize * charWidth

//////////////

tabDrawStringP :: !Point2 !String !FontInfo !*Picture -> *Picture
tabDrawStringP point string {thefont, showTabs,tabSize,charWidth} picture
	#! strings = splitAtTabs string
	= tabDrawString` point strings picture
where
	tabDrawString` :: !Point2 [String] !*Picture -> *Picture
	tabDrawString` _ [] picture
		= picture

	tabDrawString` point [ string : [] ] picture
		#	picture				= setPenPos point picture
			picture				= draw string picture
		= picture
	  
	tabDrawString` point [ string : strings ] picture
		#	picture				= setPenPos point picture
			picture				= draw string picture
			(newPoint,picture)	= getPenPos picture
			newX				= alignAtTab` newPoint.x tabSize charWidth
		| not showTabs
			= tabDrawString` { point & x = newX } strings picture
		#	picture				= setPenColour Red picture
			picture				= draw '~' picture
			picture				= setPenColour Black picture
		= tabDrawString` { point & x = newX } strings picture
*/