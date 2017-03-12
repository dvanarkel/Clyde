definition module Clyde.projactions

cleanhome :: !*env -> (!String,!*env)

build :: !Bool !String !*World -> (!Int,!*World)
buildAndRun :: !*World -> (!Int,!*World)
run :: !*World -> (!Int,!*World)
