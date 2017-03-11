definition module Clyde.windows

from System._Pointer import :: Pointer

windBounds :: !Pointer !*World -> (!Pointer,!*World)

populateWindow :: !Pointer !*World -> *World
populateSecondWindow :: !Pointer !*World -> *World
populateThirdWindow :: !Pointer !*World -> *World
populateFourthWindow :: !Pointer !*World -> *World
