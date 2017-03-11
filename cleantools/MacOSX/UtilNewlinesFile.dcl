definition module UtilNewlinesFile

from StdClass import class ==, class toString

:: NewlineConvention
	=	NewlineConventionNone | NewlineConventionMac | NewlineConventionUnix | NewlineConventionDos

HostNativeNewlineConvention	:==	NewlineConventionUnix

instance == NewlineConvention
instance toString NewlineConvention

// read a line with any newline convention
// 	the file should have been opened with mode FReadData
//  the line returned ends with one '\n' (except for the last line)

readAnyLine :: !*File -> (NewlineConvention, !.{#Char}, !*File)

// same as readAnyLine, but discards newline convention (compatible with freadline)

readLine file
	:==	(line, file`)
	where
		(_, line, file`)
			=	readAnyLine file

// write a line with a specified newline convention
//	the first argument is the line
//  	(only the last character of the line is inspected for a newline character,
//		 their shouldn't be any newlines in the middle of the line)
//	the second argument is the newline string
// 	the file should have been opened with mode FWriteData

writeAnyLine :: !{#Char} !{#Char} !*File -> *File

convertLine :: !*{#Char} -> (NewlineConvention, *{#Char})

readConvLines :: !*File -> (NewlineConvention,[String],*File)
