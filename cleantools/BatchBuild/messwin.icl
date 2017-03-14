implementation module messwin

import StdString, StdBool, StdList, StdFunc
from IdeState import :: GeneralSt,::General,writeLog, closeLog
import StdDebug, StdMisc

:: InfoMessage
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo :: !.InfoMessage !*GeneralSt -> !*GeneralSt
showInfo info  ps
	= case info of
		(Level1 s)	-> writeLog` s ps
		(Level2 s)	-> writeLog` s ps
		(Level3 s)	-> seq (map writeLog` s) ps

writeLog` :: !String !*GeneralSt -> *GeneralSt
writeLog` s ps
	| trace_n s False = undef
	= writeLog s ps

closeInfo :: !*GeneralSt -> !*GeneralSt
closeInfo ps
	= closeLog ps
