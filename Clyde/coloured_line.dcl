definition module Clyde.coloured_line

:: Range :== (Int,Int)
:: Style
	= Normal
	| Comment
	| Keyword
	| StringLit
	| CharsLit
	| Typedef
	| Typedecl


attributeStrings :: ![String] -> [[(Range,Style)]]
