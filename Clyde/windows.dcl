definition module Clyde.windows

from System._Pointer import :: Pointer

contentLayoutRect :: !Pointer !*World -> (!Pointer,!*World)
visibleFrame :: !Pointer !*a -> (!Pointer,!*a)
readRect :: !Pointer !*a -> (!(!Real,!Real),!(!Real,!Real),!*a)
cascade :: !Pointer !*a -> *a


/*
populateWindow :: !Pointer !*World -> *World
populateSecondWindow :: !Pointer !*World -> *World
populateThirdWindow :: !Pointer !*World -> *World
populateFourthWindow :: !Pointer !*World -> *World
*/