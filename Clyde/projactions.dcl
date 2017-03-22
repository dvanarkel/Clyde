definition module Clyde.projactions

from IdeState import :: GeneralSt

cleanhome :: !*env -> (!String,!*env)

build :: !Bool !String !(!String !Bool !Bool !*GeneralSt -> *GeneralSt) !*World -> (!Int,!*World)
//build :: !Bool !String !*World -> (!Int,!*World)
buildAndRun :: !*World -> (!Int,!*World)
run :: !*World -> (!Int,!*World)
