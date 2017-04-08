definition module Clyde.coloured_line

// syntax colouring for Clean

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
