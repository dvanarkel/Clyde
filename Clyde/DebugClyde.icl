implementation module Clyde.DebugClyde

import StdEnv
import StdDebug

import System._Posix
import System._Pointer
import System._Unsafe
import System.CommandLine
import Text

import Cocoa.Foundation
import Cocoa.UserDefaults

debugpath =: accUnsafe (stringForKey "DEBUG_PATH")

installDebug :: !*World -> *World
installDebug world
	| debugpath == ""
		= world
	#!	world			= if (not (isatty 0)) (swizzleStandardDescriptors world) world
		world			= writeDebugPaths world
	= world

ttyStandardDescriptors :: [Bool]
ttyStandardDescriptors = map isatty [0,1,2]

writeDebugPaths world
 	#!	buffer			= createArray 2048 '\0'
 		(ptr,world)		= getcwd buffer 2048 world
 		(ok,file,world)	= fopen (debugpath +++ "/Clyde_debug.txt") FWriteText world
 		cwd				= derefString ptr
 		(args,world)	= getCommandLine world
 		file			= file <<< "CWD:\t'" <<< cwd <<< "'\n"
 		file			= file <<< "app name:\t'" <<< applicationName <<< "'\n"
 		file			= file <<< "app path:\t'" <<< applicationPath <<< "'\n"
 		
 		file			= file <<< "app args:\t"
 		file			= file <<< join "," args
 		(ok,world)		= fclose file world
	= world

swizzleStandardDescriptors :: !*a -> *a
swizzleStandardDescriptors world
	#!	outp	= debugpath +++ "/Clyde_debug_out.txt\0"
		errp	= debugpath +++ "/Clyde_debug_err.txt\0"
		(outd,world)	= fopen` outp "w\0" world
		(errd,world)	= fopen` errp "w\0" world
		(outn,world)	= fileno outd world
		(outf,world)	= dup2 outn 1 world
		(errn,world)	= fileno errd world
		(errf,world)	= dup2 errn 2 world
		(outc,world)	= fclose` outd world
		(errc,world)	= fclose` errd world
	= world

fopen` :: !String !String !*env -> (!Int,!*env)
fopen` filename mode env = code {
		ccall fopen "ss:p:A"
	}

fclose` :: !Int !*env -> (!Int,!*env)
fclose` stream env = code {
		ccall fclose "p:I:A"
	}

dup2 :: !Int !Int !*env -> (!Int,!*env)
dup2 fildes fildes2 env = code {
		ccall dup2 "II:I:A"
	}

fileno :: !Int !*env -> (!Int,!*env)
fileno stream env = code {
		ccall fileno "I:I:A"
	}

isatty :: !Int -> Bool
isatty _ = code {
		ccall isatty "I:I"
	}
