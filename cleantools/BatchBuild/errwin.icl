implementation module errwin

import StdString, StdList, StdFunc
import IdeState

updateErrorWindow :: ![String] !*GeneralSt -> *GeneralSt;
updateErrorWindow s ps = seq (map writeLog s) ps
