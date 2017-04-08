definition module Clyde.projactions

from IdeState import :: GeneralSt

cleanhome :: !*env -> (!String,!*env)

build :: !Bool !String !(String Bool Bool *GeneralSt -> *GeneralSt) !*World -> (!Int,!*World)
