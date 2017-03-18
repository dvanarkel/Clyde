definition module Clyde.Process

import Data.Maybe, Data.Either
import System.OSError, System.FilePath
from System._Posix import exit

/*
Not yet implemented:
=> - Pass startup directory
- Passsing environment, i.e. [(!String,!String)], to created process
- Ability to redirect standard input, standard output, standard error
*/

:: ProcessHandle = { pid :: Int
				   }

/**
* runs a new process
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process
*/
runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
/**
* Check if a process is still running
* @param Process handle to the process
* @return Return code if the process has finished, Nothing if the process is still running
*/
checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)

/**
* Wait for a process to terminate, closes the handle and returns the exit code
* @param Process handle to the process
* @return Exit code of the process
*/
waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)

/**
* runs a new process and wait for it to terminate
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Exit code of the process
*/
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)



// as above but with potential redirect of stdout & stderr to passed file descriptors
runProcessWithRedirect :: !FilePath ![String] !(Maybe String) !(Maybe Int) !(Maybe Int) !*World -> (MaybeOSError ProcessHandle, *World)
waitForAnyChild :: !{#Int} !*World -> (!(!Int,!MaybeOSError Int),!*World)
