implementation module Clyde.Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString
import StdMisc
import StdTuple

//Data
import Data.Maybe

//System
import System.FilePath
import System.File
import System.OSError
import System._Pointer
import System._Posix

/**/
runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world
/*	//Check if path exists 
	# (ok,world)	= fileExists path world		// why stat before fork?
	| not ok
		= (Error (1,"File " +++ path +++ " does not exist"),world)	// if we are enforcing this then why use execvp (with $PATH search...)
*/
	//Fork
	# (pid, world)			= fork world
	| pid == 0
		//Exec
		# world					= setChildDir mCurrentDirectory world
		# (argv,args_memory)	= makeArgv [path:args]
//		# (res,world)			= execvp (path +++ "\0") argv world
		# (res,world)			= execv (path +++ "\0") argv world	// search via $PATH makes no sense... (in IDE context)
		= (exit 1 world)
	| pid > 0
		= (Ok {ProcessHandle| pid = pid}, world)
	| otherwise
		= getLastOSError world

// as above but with potential redirect of stdout & stderr to passed file descriptors
runProcessWithRedirect :: !FilePath ![String] !(Maybe String) !(Maybe Int) !(Maybe Int) !*World -> (MaybeOSError ProcessHandle, *World)
runProcessWithRedirect path args mCurrentDirectory mStdOut mStdErr world
	//Check if path exists 
/*	#	(ok,world)				= fileExists path world		// why stat before fork?
	| not ok
		= (Error (1,"File " +++ path +++ " does not exist"),world)	// if we are enforcing this then why use execvp (with $PATH search...)
*/
	//Fork
	#	(pid, world)			= fork world
	| pid == 0
		//Exec
		#	world				= setChildDir mCurrentDirectory world
			world				= setChildOut mStdOut world
			world				= setChildErr mStdErr world

		#	(argv,args_memory)	= makeArgv [path:args]
		#	(res,world)			= execv (path +++ "\0") argv world
		= (exit 1 world)
	| pid > 0
		= (Ok {ProcessHandle| pid = pid}, world)
	| otherwise
		= getLastOSError world

setChildDir :: !(Maybe String) !*World -> *World
setChildDir (Nothing) world	= world
setChildDir (Just dir) world
	# (ret,world)	= chdir (packString dir) world
	| ret <> 0
		= snd (exit (-1) world)
	= world

:: FileDesc :== Int
setChildOut :: !(Maybe FileDesc) !*World -> *World
setChildOut Nothing world
	= world
setChildOut (Just stdout_fd) world
	#	(r,world)	= dup2 stdout_fd 1 world
	| r == (-1)
		= abort "dup2 failed"
	= world
setChildErr :: !(Maybe FileDesc) !*World -> *World
setChildErr Nothing world
	= world
setChildErr (Just stderr_fd) world
	# (r,world)	= dup2 stderr_fd 2 world			// redirect cgen stderr to errors file
	| r== (-1)
		= abort "dup2 failed"
	= world

makeArgv :: [String] -> (!{#Pointer},!Pointer)
makeArgv argv_list
	# args_size = argvLength argv_list 0
	  args_string = createArgsString args_size argv_list
	  args_memory = malloc args_size
	| args_memory == 0
		= abort "malloc failed"
	# args_memory = memcpy_string_to_pointer args_memory args_string args_size
	  argv = createArgv argv_list args_memory
	= (argv,args_memory)
where
		argvLength [a:as] l
			= argvLength as (l+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4)))
		argvLength [] l
			= l

    	createArgsString args_size argv_list
			# s = createArray args_size '\0'
			= copyArgs argv_list 0 s
		where
			copyArgs [a:as] i s
				# s = copyChars 0 a i s
				= copyArgs as (i+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))) s
			copyArgs [] i s
				= s
    
			copyChars :: !Int !{#Char} !Int !*{#Char} -> *{#Char}
			copyChars ai a si s
				| ai<size a
					# s = {s & [si]=a.[ai]}
					= copyChars (ai+1) a (si+1) s
				= s

		createArgv argv_list args_memory
			# n_args = length argv_list
			# argv = createArray (n_args+1) 0;
			= fillArgv 0 argv_list argv args_memory 
		where
			fillArgv :: !Int ![{#Char}] !*{#Pointer} !Int -> *{#Pointer}
			fillArgv arg_n [a:as] argv args_memory
				# argv = {argv & [arg_n]=args_memory}
				  args_memory = args_memory + ((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))
				= fillArgv (arg_n+1) as argv args_memory
			fillArgv arg_n [] argv args_memory
				= {argv & [arg_n]=0}

checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status WNOHANG world //Non-blocking wait :)
	| ret == 0
		= (Ok Nothing, world)	
	| ret == pid	
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok (Just exitCode), world)
	| otherwise
		= getLastOSError world

waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
waitForProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status 0 world //Blocking wait
	| ret == pid
		#	signal	= status.[0] bitand 0x7f
		| signal <> 0	// wtermsig
			= (Error (signal, "Process stopped due to signal "+++toString signal), world)
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok exitCode, world)
	| otherwise
		= getLastOSError world

waitForAnyChild :: !{#Int} !*World -> (!(!Int,!MaybeOSError Int),!*World)
waitForAnyChild process_ids world
	# status		= createArray 1 0
	# (pid,world)	= waitpid -1 status 0 world //Blocking wait

	| pid == -1
		#	(err,world)	= getLastOSError world
		= ((pid,err),world)

	| otherwise
		# process_n		= find_process_id_index 0 pid process_ids
		#	signal	= status.[0] bitand 0x7f
		| signal <> 0	// wtermsig
			= ((process_n ,Error (signal, "Process stopped due to signal "+++toString signal)), world)
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= ((process_n ,Ok exitCode), world)
where
	find_process_id_index i w_pid pids
		| i<size pids
			| pids.[i]==w_pid
				= i
				= find_process_id_index (i+1) w_pid pids
		= -1

callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of 
		Ok handle	= waitForProcess handle world
		Error e		= (Error e, world)

execv :: !{#Char} !{#Int} !*w -> (!Int,!*w)
execv program argv world = code {
	ccall execv "sA:I:A"
}

dup2 :: !Int !Int !*w -> (!Int, !*w)
dup2 old new world = code {
    ccall dup2 "II:I:A"
}

