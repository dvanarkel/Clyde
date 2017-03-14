implementation module errwin

import StdString, StdList, StdFunc
import IdeState

updateErrorWindow :: ![String] !*GeneralSt -> *GeneralSt;
updateErrorWindow s ps = seq (map writeLog` s) ps

import StdDebug, StdMisc
writeLog` :: !String !*GeneralSt -> *GeneralSt
writeLog` s ps
	| trace_n s False = undef
	= writeLog s ps

