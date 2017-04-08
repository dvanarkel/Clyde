definition module Clyde.Console

createConsoleWindowControllerClass :: !*World -> *World		// create class

con_stdin_remote :: Int					// file descriptor for stdin pipe
con_stdout_remote :: Int				// file descriptor for stdout pipe
con_stderr_remote :: Int				// file descriptor for stderr pipe

openConsoleWindow :: !String !*World -> *World	// run process with console window

// exports for foreign...
from Cocoa.objc import :: ID, :: SEL, :: BOOL, :: Pointer

doHideC :: !ID !SEL !ID -> BOOL
keyDown :: !ID !SEL !ID -> BOOL
didRead :: !ID !SEL !ID -> BOOL
