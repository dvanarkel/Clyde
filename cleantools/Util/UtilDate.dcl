definition module UtilDate

import StdOverloaded

NoDate	:== {exists=False,yy=0,mm=0,dd=0,h=0,m=0,s=0}

::	DATE = {	exists	:: !Bool,
				yy		:: !Int,
				mm		:: !Int,
				dd		:: !Int,
				h		:: !Int,
				m		:: !Int,
				s		:: !Int }

instance fromString DATE
instance toString DATE

//	Checks whether the first date is older than the second one.
Older_Date	:: !DATE !DATE -> Bool

