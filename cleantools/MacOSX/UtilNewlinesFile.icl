implementation module UtilNewlinesFile

import StdFile, StdInt, StdChar, StdString, StdArray, StdBool, StdClass, StdTuple, StdMisc

// simple sequence operator
(:-) infixl 0
(:-) f a
	:==	a f

:: NewlineConvention
	= NewlineConventionNone
	| NewlineConventionMac
	| NewlineConventionUnix
	| NewlineConventionDos

HostNativeNewlineConvention	:==	NewlineConventionUnix

instance == NewlineConvention where
	(==) NewlineConventionNone NewlineConventionNone
		=	True
	(==) NewlineConventionMac NewlineConventionMac
		=	True
	(==) NewlineConventionUnix NewlineConventionUnix
		=	True
	(==) NewlineConventionDos NewlineConventionDos
		=	True
	(==) _ _
		=	False

instance toString NewlineConvention where
	toString NewlineConventionNone
		=	""
	toString NewlineConventionMac
		=	"\xd"
	toString NewlineConventionUnix
		=	"\xa"
	toString NewlineConventionDos
		=	"\xd\xa"

// slice that returns a unique array
(%.) infixl 9 :: !.{#Char} !(!Int,!Int) -> .{#Char}
(%.) string indices
	=	code
		{
			.inline %.
			.d 1 2 ii
				jsr sliceAC
			.o 1 0
			.end
		}

// this should be added to the Clean rts, so that the string doesn't have to be copied
downSize :: Int *{#Char} -> *{#Char}
downSize newSize string
	=	string %. (0, newSize-1)

convertLine :: !*{#Char} -> (NewlineConvention, *{#Char})
convertLine line
	#! maxIndex
			=	size line - 1
	| maxIndex >= 0
		#! lastChar
			=	line.[maxIndex]
		| lastChar == '\xa'
			| maxIndex >= 1
				#! lastButOneChar
					=	line.[maxIndex-1]
				|  lastButOneChar == '\xd'
					=	(NewlineConventionDos, {downSize maxIndex line & [maxIndex-1] = '\n'})
				// otherwise
					=	(NewlineConventionUnix, {line & [maxIndex] = '\n'})
			// otherwise
				=	(NewlineConventionUnix, {line & [maxIndex] = '\n'})
		| lastChar == '\xd'
			=	(NewlineConventionMac, {line & [maxIndex] = '\n'})
		// otherwise
			=	(NewlineConventionNone, line)
	// otherwise
		=	(NewlineConventionNone, line)

readAnyLine :: !*File -> (NewlineConvention, !.{#Char}, !*File)
readAnyLine file
	# (line, file)
		=	freadline file
	  (convention, line)
	  	=	convertLine line
	=	(convention, line, file)

readLine file
	:==	(line, file`)
	where
		(_, line, file`)
			=	readAnyLine file

writeAnyLine :: !{#Char} !{#Char} !*File -> *File
writeAnyLine line newlineString file
	# maxIndex
			=	size line - 1
	  lastChar
			=	line.[maxIndex]
	| maxIndex >= 0 && lastChar == '\n'
		= file :- fwrites (line %. (0, maxIndex-1)) :- fwrites newlineString
	// otherwise
		= file :- fwrites line

//--
readConvLines :: !*File -> (NewlineConvention,[String],*File)
readConvLines file
  # (conv,line,more,file)	= readAnyLine` file
	(eof, file)		= fend file
  | eof 
	| more // last line ends in a newline?
		= (conv,[line,""], file)
		= (conv,[line], file)
  # (_,lines,file)	= readConvLines file
  = (conv,[line:lines], file)
	
//readAnyLine` :: !*File -> (NewlineConvention, !.{#Char}, !*File)
readAnyLine` file
	# (line, file)			=	freadline file
	#! maxIndex				=	size line - 1
	| maxIndex >= 0
		# lastChar			=	line.[maxIndex]
		| lastChar == '\xa'
			| maxIndex >= 1
				# lastButOneChar					=	line.[maxIndex-1]
				| lastButOneChar == '\xd'
					= (NewlineConventionDos, downSize (dec maxIndex) line, True, file)
				= (NewlineConventionUnix, downSize maxIndex line, True, file)
			= (NewlineConventionUnix, downSize maxIndex line, True, file)
		| lastChar == '\xd'
			= (NewlineConventionMac, downSize maxIndex line, True, file)
		= (NewlineConventionNone, line, False, file)
	= (NewlineConventionNone, line, False, file)
