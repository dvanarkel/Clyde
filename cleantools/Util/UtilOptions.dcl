definition module UtilOptions

import StdClass, StdString
from UtilStrictLists import :: List

:: Option
:: Conversions a
:: OptionsTableEntry a =
	E.value:
	{ labelName		:: !{#Char}
	, conversions	:: !Conversions value
	, get			:: a -> value
	, put			:: value a -> a
	}

:: OptionsTable a	:== {!OptionsTableEntry a}

WriteOptionsFile	:: !{#Char} ![Option] !*File -> *File
ReadOptionsFile		:: *File -> ([Option], *File)
ReadVersion			:: !*File -> (!{#Char}, !*File)
PutOptions			:: !(OptionsTable a) a -> [Option]
GetOptions			:: !(OptionsTable a) ![Option] !a -> a

Simple	:: Conversions a | toString, fromString a
SimpleWithStringConversion :: ({#Char} -> {#Char}) -> (Conversions a) | toString, fromString a
List	:: (OptionsTableEntry a) a -> Conversions (List a)
OptionalGroup :: (OptionsTable a) (a->Bool) -> Conversions a
Group	:: (OptionsTable a) -> Conversions a

SimpleOption l g p		:== {labelName = l, conversions = Simple, get = g, put = p}
SimpleWithStringConversionOption c l g p :== {labelName = l, conversions = SimpleWithStringConversion c, get = g, put = p}
GroupedOption  l s g p	:== {labelName = l, conversions = Group s, get = g, put = p}
OptionalGroupedOption l e s g p :== {labelName = l, conversions = OptionalGroup s e, get = g, put = p}
ListOption  l s d g p	:== {labelName = l, conversions = List s d, get = g, put = p}

ConvertToString		:: !String -> String
