definition module Clyde.projactions

cleanhome :: !String

build :: !Bool !String !*World -> (!Int,!*World)
buildAndRun :: !*World -> (!Int,!*World)
run :: !*World -> (!Int,!*World)
