definition module Clyde.Link

from IdeState				import :: GeneralSt
//from StdPathname			import :: Pathname
from PmTypes				import :: ApplicationOptions, :: Processor
//from UtilStrictLists	import :: List

:: Path :== String

Link ::	!String !Path !ApplicationOptions
		! Path ![!Path!] ![!Path!] ![!Path!] !Bool !Bool !Bool !Bool !Bool !String
		!Bool !String !Path !String !Processor !Bool !*GeneralSt
		 -> (!*GeneralSt,!Bool)


from PmFileInfo			import :: FileInfoCache
from PmProject			import :: Project

MakeOptionsName :: !.String !Processor -> String
CheckObjsOutOfDate :: !Bool !Path ![!String!] !*GeneralSt -> (!Bool,!*GeneralSt)
CheckExecOutOfDate :: !Bool !Path !FileInfoCache !Project !*GeneralSt -> *(!Bool,!*GeneralSt)
