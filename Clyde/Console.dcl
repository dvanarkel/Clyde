definition module Clyde.Console

createConsoleWindowControllerClass :: !*World -> *World

doHideC :: !Int !Int !Int -> Int

con_stdin_remote :: Int
con_stdout_remote :: Int
con_stderr_remote :: Int

keyDown :: !Int !Int !Int -> Int
didRead :: !Int !Int !Int -> Int

openConsoleWindow :: !String !*World -> *World
