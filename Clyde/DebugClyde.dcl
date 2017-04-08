definition module Clyde.DebugClyde

installDebug :: !*World -> *World
// DEBUG_PATH is in Clyde defaults dictionary
// write app name, path and args to DEBUG_PATH/Clyde_debug.txt
// if stdin is not a tty (ie we did not launch from Terminal) then redirect
// stdout & stderr to DEBUG_PATH/Clyde_debug_out and Clyde_debug_err

ttyStandardDescriptors :: [Bool]
// are stdin,stdout,stderr connected to a tty?
