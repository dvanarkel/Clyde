implementation module UtilOptions

import StdArray, StdBool, StdEnum, StdFile
import UtilNewlinesFile
import UtilStrictLists

//--
::	Maybe x
	=	Just x
	|	Nothing

//----
:: Label = {label :: !{#Char}}
:: Value = {value :: !{#Char}}
:: LineNumber :== Int
:: Option = Option LineNumber Label (Maybe Value) [Option]
::	*Input	=	{ lineNumber :: !Int, lookAHead :: !String, file :: !*File}
:: Conversions a =
	{
		toValue
			:: a -> (Maybe Value, [Option]),
		fromValue
			:: (Maybe Value) [Option] a -> a
	}
// +++ remove tables (too much code)
:: OptionsTableEntry a =
	E.value:
	{
		labelName
			:: !{#Char},
		conversions
			:: !Conversions value,
		get
			:: a -> value,
		put
			:: value a -> a
	}

:: OptionsTable a :== {!OptionsTableEntry a}


//--

Simple :: Conversions a | toString, fromString a
Simple	=
	{
		toValue
			= \value -> ((Just {value = toString value}), []),
		fromValue
			= fromValue
	}
	where
		fromValue Nothing _ value
			=	value
		fromValue (Just {value}) subOptionsTable list
			=	fromString value

SimpleWithStringConversion :: ({#Char} -> {#Char}) -> (Conversions a) | toString, fromString a
SimpleWithStringConversion convert_string =
	{
		toValue
			= \value -> ((Just {value = toString value}), []),
		fromValue
			= fromValue
	}
	where
		fromValue Nothing _ value
			=	value
		fromValue (Just {value}) subOptionsTable list
			=	fromString (convert_string value)

Group :: (OptionsTable a) -> Conversions a
Group subOptionTable	=
	{
		toValue
			= \value -> (Nothing, PutOptions subOptionTable value),
		fromValue
			=	\_ subOptions value -> GetOptions subOptionTable subOptions value
	}

OptionalGroup :: (OptionsTable a) (a->Bool) -> Conversions a
OptionalGroup subOptionTable is_empty_group =
	{
		toValue
			= \value
				= if (is_empty_group value)
					(Nothing, [])
					(Nothing, PutOptions subOptionTable value),
		fromValue
			=	\_ subOptions value -> GetOptions subOptionTable subOptions value
	}

List :: (OptionsTableEntry a) a -> Conversions (List a)
List optionsTableEntry defaultValue	=
	{
		toValue
			= \list -> (Nothing, putList list),
		fromValue
			= \_ table list -> getList table
	}
	where
		putList Nil
			=	[]
		putList (h :! t)
			=	case (PutOption h optionsTableEntry) of
					Nothing
						->	 putList t
					Just option
						->	[option : putList t]

		getList []
			=	Nil
		getList [h : t]
			# h = GO optionsTableEntry h defaultValue
			# t = getList t
			= h :! t

		GO {conversions={fromValue}, get, put, labelName} (Option _ {label} value subOptions) currentValue
			| label <> labelName = currentValue
			= put (fromValue value subOptions (get currentValue)) currentValue

SimpleOption l g p :== {labelName = l, conversions = Simple, get = g, put = p}
GroupedOption  l s g p :== {labelName = l, conversions = Group s, get = g, put = p}
ListOption  l s d g p :== {labelName = l, conversions = List s d, get = g, put = p}

//--

EndOfInput :: *Input -> (!Bool, !*Input)
EndOfInput input=:{lookAHead}
	=	(lookAHead == "", input)

LookAHead :: *File -> *Input
LookAHead file
	=	{ lineNumber = 1, lookAHead = nextLine, file = nextFile }
where
		(nextLine, nextFile)
			=	readLine file

NextLine :: *Input -> *Input
NextLine input=:{file, lineNumber}
	=	{ input & lineNumber = lineNumber+1, lookAHead = nextLine, file = nextFile}
where
		(nextLine, nextFile)
			=	readLine file

ConvertToString	:: !String -> String
ConvertToString str = StripNewline (FindColon 0 (size str) str) str
where
	FindColon :: !Int !Int !String -> Int
	FindColon i len str
		| i >= len				= i
		|  str .[ i]  == ':' 	= SkipSpaces (inc i) len str
								= FindColon (inc i) len str
	SkipSpaces	:: !Int !Int !String -> Int
	SkipSpaces i len str | i >= len || str .[ i] <> ' '	= i
		                     							= SkipSpaces (inc i) len str

	StripNewline :: !Int !String -> String
	StripNewline fro string | lmin1-fro < 0	=  ""
											=  string % (fro, dec lmin1)
	where
		lmin1	= dec (size string)

CommentChar
	:==	'#'
LabelSeparatorChar
	:==	':'
TabSize
	:== 4

copy a :== {e \\ e <-: a}

RemoveWhiteSpace :: {#Char} -> {#Char}
RemoveWhiteSpace string
	| firstFound >= stringSize
		=	string
	| otherwise
		=	shiftChars firstFound (firstFound+1) (copy string)
	where
		stringSize
			=	size string

		firstFound
			=	findChar 0
		findChar i
			|  i >= stringSize || string.[i] == ' ' || string.[i] == '\t'
				=	i
			| otherwise
				=	findChar (i+1)

		shiftChars :: !Int !Int *{#Char} -> {#Char}
		shiftChars toPos fromPos string
			| fromPos >= stringSize
				=	string % (0, toPos-1)
			#! char
				=	string.[fromPos]
			| char == ' ' || char == '\t'
				=	shiftChars fromPos (toPos+1) string
			| otherwise
				=	shiftChars (toPos+1) (fromPos+1) {string & [toPos] = char}

RemoveNewLine :: {#Char} -> {#Char}
RemoveNewLine string
	| string.[size string-1] == '\n'
		=	string % (0, size string-2)
	| otherwise
		=	string

// lfold op :== foldl (flip op)
lfold op r l
	:==	lfold r l
	where
		lfold r []		= r
		lfold r [a:x]	= lfold (op a r) x

(:-) infixl 0
(:-) f a
	:==	a f

WriteOptionsFile :: !{#Char} ![Option] !*File -> *File
WriteOptionsFile version options file
	=	file
	:-	fwrites ("Version: "+++version+++"\n")			// DvA: set to 1.4 for New IDE development
	:-	writeOptions 0 options
	where
		writeOptions _ [] file
			=	file
		writeOptions indentation [h : t] file
			=	writeOptions indentation t (writeOption indentation h file)

		writeOption :: Int Option !*File -> *File
		writeOption indentation (Option _ {label} value subOptions) file
			=	file // ->> {' ' \\ i <- [0..indentation-1]} +++ label +++ " (writeOption)\n"
			:-	fwrites {'\t' \\ i <- [0..indentation-1]}
			:-	fwrites label
			:-  writeValue value
			:-	fwritec '\n'
			:-	writeOptions (indentation+1) subOptions

		writeValue Nothing file
			=	file
		writeValue (Just {value}) file
			=	file
			:-	fwrites ":\t"
			:-	fwrites value

ReadOptionsFile :: *File -> ([Option], *File)
ReadOptionsFile file
	# (options, input)
		=	parseOptions 0 (LookAHead file)
	=	(options, input.file)
	where
		parseLabelAndOption :: !Int !Int !Int !{#Char} -> (Label, Maybe Value)
		parseLabelAndOption begin end size string
			| end >= size
				=	({label = RemoveWhiteSpace (string % (begin, end-1))}, Nothing)
			| string.[end] == LabelSeparatorChar
				=	({label = RemoveWhiteSpace (string % (begin, end-1))}, Just {value = skipSpaces (end+1)})
				with
					skipSpaces position
						| position >= size
							=	""
						| string.[position] == ' ' || string.[position] == '\t'
							=	skipSpaces (position+1)
						| otherwise
							=	string % (position, size-1)
			| otherwise
				=	parseLabelAndOption begin (end+1) size string	

		parseOption :: !Int !Int !Int !*Input -> (!Option, !*Input)
		parseOption indentation currentIndentation offset input=:{lookAHead, lineNumber}
			| currentIndentation > indentation
				# (subOptions, input)
					=	parseOptions currentIndentation input
				=	(Option lineNumber {label=""} Nothing subOptions, input)
			| otherwise
				# (label, value)
					=	parseLabelAndOption offset offset (size strippedLookAHead) strippedLookAHead
				  		with
				  			strippedLookAHead
							  	=	(RemoveNewLine lookAHead)
				# (subOptions, input)
					=	parseOptions (indentation+TabSize) (NextLine input)
				=	(Option lineNumber label value subOptions, input)

		parseOptions :: !Int !*Input -> ([Option], !*Input)
		parseOptions indentation input
			# (currentIndentation, offset, input)
				=	parseIndentation input
			# (eof, input)
				=	EndOfInput input
			| eof || currentIndentation < indentation
				=	([], input)
			# (option, input)
				=	parseOption indentation currentIndentation offset input
			# (options, input)
				=	parseOptions indentation input
			=	([option : options], input)

		indentation :: {# Char } ->  (!Int, !Int)
		indentation string
			=	indentation 0 0 string
			where
				stringSize
					=	size string - 1 

				indentation :: !Int !Int {# Char } ->  (!Int, !Int)
				indentation offset indent string
					| offset >= stringSize || string.[offset] == CommentChar
						=	(-1, offset)
					| string.[offset] == ' '
						=	indentation (offset+1) (indent+1) string
					| string.[offset] == '\t'
						=	indentation (offset+1) (indent + (TabSize - (indent rem TabSize))) string
					| otherwise
						=	(indent, offset)

		parseIndentation :: *Input -> (!Int, !Int, *Input)
		parseIndentation input=:{lookAHead}
			| lookAHead == ""
				=	(0, 0, input)
			# (indentation, offset)
				=	indentation lookAHead
			| indentation == (-1)
				=	parseIndentation (NextLine input)
			| otherwise
				=	(indentation, offset, input)

ReadVersion :: !*File -> (!{#Char}, !*File)
ReadVersion file
	# (string, file)
		=	readLine file
	| EqualPrefix "Version:" string
		=	(ConvertToString string, file)
	| otherwise
		=	("", file)
where
	EqualPrefix :: String String -> Bool
	EqualPrefix prefix string
		=	prefix == string % (0, size prefix - 1)
//--

GetOptions :: !(OptionsTable a) ![Option] !a -> a
GetOptions table [] value
	=	value
GetOptions table [option : options] value
	=	GetOptions table options (GetOption table option value)

GetOption :: (OptionsTable a) Option !a -> a
GetOption table (Option _ {label=labelName} value subOptions) currentValue
	=	case [optionEntry \\ optionEntry <-: table | optionEntry.labelName == labelName] of
			[]
				->	currentValue
			[{conversions={fromValue}, get, put} : _]
				->	put (fromValue value subOptions (get currentValue)) currentValue

PutOptions :: !(OptionsTable a) a -> [Option]
PutOptions table a
	=	putOptions 0
where
	tableSize
		=	size table
	putOptions i
		| i >= tableSize
			=	[]
		// otherwise
			=	case (PutOption a table.[i]) of
					Nothing
						->	putOptions (i+1)
					Just option
						->	[option : putOptions (i+1)]
				
PutOption :: a (OptionsTableEntry a) -> Maybe Option
PutOption value {labelName, conversions={toValue}, get}
	=	(case (toValue (get value)) of
			(Nothing, [])
				->	Nothing
			(value, subOptions)
				->	Just (Option 0 {label=labelName} value subOptions))

