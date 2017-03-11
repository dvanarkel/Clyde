definition module messwin

import StdString
from IdeState import :: GeneralSt,::General, writeLog

:: InfoMessage
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo	:: !.InfoMessage !*GeneralSt -> *GeneralSt
closeInfo	:: !*GeneralSt -> *GeneralSt
