implementation module typeatt

import StdEnv
import Data.List, Text
import Clyde.ClydeApplicationController
import IdeState

update_type_window :: !Bool !String ![String] !*GeneralSt -> *GeneralSt
update_type_window interact name message ps
	| isEmpty message
		= ps
	#!	ps	= app_world_instead_of_ps (appendTypeWindow info) ps
	= ps
where
	info = concat (intersperse "\n" ["\"" +++ name +++ "\"" : message])