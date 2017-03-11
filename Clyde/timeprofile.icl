implementation module Clyde.timeprofile

import StdEnv
import StdDebug
import System.CommandLine
import Text.CSV
//import time_profile_os_dependent

ApplicationName :==  "ShowTimeProfile"
HelpFileName :== ApplicationName +++ "Help"

////


handleCommands world
	# (cmds,world)	= getCommandLine world
	# world			= traceCommands cmds world
	# fname			= findCmd cmds
	= (fname,world)
where
	findCmd [h,f:t]
		# fsize	= size f
		# idx	= [i \\ i <- [fsize-1..0] | f.[i] == '.' && f%(i,i+3) == ".pcl"]
		| isEmpty idx
			= (f +++ " Time Profile.pcl",f +++ " Time Profile.csv")
		# idx	= hd idx
		# fn	= f%(0,idx-1)
		= (f,fn+++".csv")
	findCmd _	= abort "usage: ShowProfile <profile>\n"

traceCommands [] world	= world
traceCommands [cmd:cmds] world
	#! world	= trace_n cmd world
	= traceCommands cmds world


testStart world
	# ((fname,oname),world)				= handleCommands world

	# (lines,world)			= readProfileLines fname world
	
	# lines					= [headerline:lines]
	# (ok,file,world)		= fopen oname FWriteText world
	# file					= writeCSVFile lines file
	# (ok,world)			= fclose file world

	= (ok,world)

readProfileLines :: !String !*World -> (![[String]],!*World)
readProfileLines fname world
	# ((ok,profile),world)	= open_profile fname world
	
	# (total_strict_calls,total_lazy_calls,total_curried_calls,total_allocation,total_time)
		= sum_time_and_allocation profile
	# (fprof,sprof)
		= format_profile total_strict_calls total_lazy_calls total_curried_calls total_allocation total_time profile
	# fprof
		= [sprof:fprof]
	# lines
		= map fp2sl fprof
	= (lines,world)

headerline :: [String]
headerline =
	[ "Module"
	, "Function"
	, "Strict(n)"
	, "Lazy(n)"
	, "Curried(n)"
	, "Alloc(bytes)"
	, "Alloc(%)"
	, "Time(s)"
	, "Time(%)"
	]

//--

:: Profile =
	{ module_name		:: String
	, function_name		:: String
	, n_strict_calls	:: Int
	, n_lazy_calls		:: Int
	, n_curried_calls	:: Int
	, n_allocated_words	:: Int
	, time				:: Real
	}

:: FormattedProfile =
	{ f_module_name			:: String
	, f_function_name		:: String
	, f_n_strict_calls		:: Int
	, f_n_lazy_calls		:: Int
	, f_n_curried_calls		:: Int
	, f_n_allocated_words	:: Int
	, f_alloc_percentage	:: Real
	, f_time				:: Real
	, f_time_percentage		:: Real
	}

fp2sl fp =
	[ fp.f_module_name
	, fp.f_function_name
	, toString fp.f_n_strict_calls
	, toString fp.f_n_lazy_calls
	, toString fp.f_n_curried_calls
	, n_words_to_n_bytes_string fp.f_n_allocated_words
	, format_real 0 2 3 1000.0 fp.f_alloc_percentage
	, format_real 0 1 6 1000000.0 fp.f_time
	, format_real 0 2 3 1000.0 fp.f_time_percentage
	]

//-- File funs

sum_time_and_allocation :: ![.Profile] -> .(Int,Int,Int,Int,Real);
sum_time_and_allocation l = foldl add_time_and_allocation (0,0,0,0,0.0) l
where
	add_time_and_allocation (s,l,c,a,t) {function_name,n_strict_calls,n_lazy_calls,n_curried_calls,n_allocated_words,time}
		| n_allocated_words>=0
			= (s+n_strict_calls,l+n_lazy_calls,c+n_curried_calls,a+n_allocated_words,t+time)
			= (s+n_strict_calls,l+n_lazy_calls,c+n_curried_calls,a,t+time)

totals_per_module :: ![.Profile] -> [.Profile]
totals_per_module []
	= []
totals_per_module [f=:{module_name}:l]
	# (functions,l) = split_at_next_module l
	# functions = [f:functions]
	# (total_strict_calls,total_lazy_calls,total_curried_calls,total_allocation,total_time) = sum_time_and_allocation functions
	# new_module =
			{ module_name=module_name
			, function_name="Module "+++module_name
			, n_strict_calls=total_strict_calls
			, n_lazy_calls=total_lazy_calls
			, n_curried_calls=total_curried_calls
			, n_allocated_words=total_allocation
			, time=total_time
			}
	= [new_module:totals_per_module l]
where
	split_at_next_module []
		= ([],[])
	split_at_next_module l=:[f=:{module_name=m}:t]
		| m==module_name
			# (functions,l) = split_at_next_module t
			= ([f:functions],l)
			= ([],l)

// File i/o

open_profile :: {#.Char} !*a -> *((.Bool,[.Profile]),!*a) | FileSystem a;
open_profile file_name files
	# (open_ok,input_file,files)	= fopen file_name FReadText files
	| not open_ok
		= ((False,[]),files)
	# (profile,input_file)			= read_profile input_file
	  (_,files)						= fclose input_file files
	= ((True,profile),files)
where
	read_profile :: *File -> ([.Profile],.File);
	read_profile file
		# (compute_time_function,file) = get_compute_time_function file
		= read_function_profiles compute_time_function file
	
	read_function_profiles :: (.(Int,Int,Int) -> .Real) *File -> ([.Profile],.File);
	read_function_profiles compute_time_function file
		# (ok,function_profile,file) = read_function_profile file
		| not ok
			= ([],file)
			# (profile,file) = read_function_profiles compute_time_function file
			= ([function_profile : profile],file)
	where
		read_function_profile file
			# (ok,module_name,file) = read_function_name file
			| not ok
				= error file
			# (ok,function_name,file) = read_function_name file
			| not ok
				= error file
			# (ok,n_strict_calls,file)=freadi file
			| not ok
				= error file
			# (ok,n_lazy_calls,file)=freadi file
			| not ok
				= error file
			# (ok,n_curried_calls,file)=freadi file
			| not ok
				= error file
			# (ok,n_profiler_calls,file)=freadi file
			| not ok
				= error file
			# (ok,n_allocated_words,file)=freadi file
			| not ok
				= error file
			# (ok,time_hi,file)=freadi file
			| not ok
				= error file
			# (ok,time_lo,file)=freadi file
			| not ok
				# time_lo=time_hi
				# time_hi=0
				# time = compute_time_function (time_hi,time_lo,n_profiler_calls)
				=	(True,
					{ module_name		= module_name
					, function_name		= function_name
					, n_strict_calls	= n_strict_calls
					, n_lazy_calls		= n_lazy_calls
					, n_curried_calls	= n_curried_calls
					, n_allocated_words	= n_allocated_words
					, time				= time
					},file)
			# (ok,c,file) = freadc file
			| not ok || c<>'\n'
				= error file
				# time = compute_time_function (time_hi,time_lo,n_profiler_calls)
				=	(True,
					{ module_name		= module_name
					, function_name		= function_name
					, n_strict_calls	= n_strict_calls
					, n_lazy_calls		= n_lazy_calls
					, n_curried_calls	= n_curried_calls
					, n_allocated_words	= n_allocated_words
					, time				= time
					},file)
		where
				error file = (False,abort "error in read_function_profile",file)
		
		read_function_name :: !*File -> (!Bool,!String,!*File)
		read_function_name file
			# (ok,c,file) = freadc file
			| not ok || c==' ' || c=='\n'
				= (False,"",file)
				# (acc,file) = read_function_name [c] file
				= (True,{c \\ c <- reverse acc},file)
		where
			read_function_name acc file
				# (ok,c,file) = freadc file
				| not ok || c == ' ' || c == '\n' = (acc,file)
				= read_function_name [c:acc] file

//-- Profile Look

//format_string_r :: .Int u:(a v:Char) -> a Char | Array .a, [u <= v];
format_string_r length string
	# string_size=size string
	| string_size >= length
		= string
		= (createArray (length-string_size) ' ')+++string

format_real :: .Int .Int .Int .Real .Real -> {#Char};
format_real n_spaces n d m r
	| r<0.0
		= format_negative_real (if (n_spaces<1) 0 (dec n_spaces)) n d m (~r)
	# s=toString (toInt (m*r))
	  l=size s
	| l<=d
		= createArray n_spaces ' ' +++ createArray n '0' +++"."+++createArray (d-l) '0'+++s
	| l<=n+d
		= createArray n_spaces ' ' +++ createArray (n+d-l) '0' +++insert_dot_in_string s l d
	| l<=n_spaces+n+d
		= createArray (n_spaces+n+d-l) ' '+++ insert_dot_in_string s l d
		= insert_dot_in_string s l d

format_negative_real :: .Int .Int .Int a a -> {#Char} | * , toInt a;
format_negative_real n_spaces n d m r
	# s=toString (toInt (m*r))
	  l=size s
	| l<=d
		= createArray n_spaces ' ' +++"-"+++ createArray n '0' +++"."+++ createArray (d-l) '0' +++s
	| l<=n+d
		= createArray n_spaces ' ' +++"-"+++ createArray (n+d-l) '0' +++insert_dot_in_string s l d
	| l<=n_spaces+n+d
		= createArray (n_spaces+n+d-l) ' ' +++ "-"+++insert_dot_in_string s l d
		= "-"+++insert_dot_in_string s l d

insert_dot_in_string :: {#.Char} .Int .Int -> {#Char};
insert_dot_in_string s l d = s % (0,l-1-d) +++"."+++ s % (l-d,l-1)

format_profile :: .Int .Int .Int .Int .Real [.Profile] -> ([.FormattedProfile],.FormattedProfile);
format_profile total_strict_calls total_lazy_calls total_curried_calls total_allocation total_time profile_list
	= ([format_profile p \\ p<-profile_list],
	   { f_module_name			= "All Modules"
	   , f_function_name		= "Total"
	   , f_n_strict_calls		= total_strict_calls
	   , f_n_lazy_calls			= total_lazy_calls
	   , f_n_curried_calls		= total_curried_calls
	   , f_n_allocated_words	= total_allocation
	   , f_alloc_percentage		= 100.0
	   , f_time					= total_time
	   , f_time_percentage		= 100.0
	   })
where
	format_profile {module_name,function_name,n_strict_calls,n_lazy_calls,n_curried_calls,n_allocated_words,time} =
		{ f_module_name			= module_name
		, f_function_name		= function_name
		, f_time				= time
		, f_time_percentage		= (time*100.0)/total_time
		, f_n_allocated_words	= n_allocated_words
		, f_alloc_percentage	= (toReal (n_allocated_words)*100.0)/toReal total_allocation
		, f_n_strict_calls		= n_strict_calls
		, f_n_lazy_calls		= n_lazy_calls
		, f_n_curried_calls		= n_curried_calls
		}

n_words_to_n_bytes_string n_words
	| n_words>0
		| n_words<536870912 /* 1<<29 */
			= toString (n_words<<2);
			# n_words_d_25=n_words/25;
			# r=n_words-25*n_words_d_25;
			# r1=r/10;
			# r0=r-10*r1;
			= toString n_words_d_25+++{toChar (48+r1),toChar (48+r0)};
		| n_words>= -536870912 /* -(1<<29) */
			= toString (n_words<<2);
			# n_words_d_25=n_words/25;
			# r= ~(n_words-25*n_words_d_25);
			# r1=r/10;
			# r0=r-10*r1;
			= toString n_words_d_25+++{toChar (48+r1),toChar (48+r0)};

// from time_profile_os_dependant

clock_speed_and_profile_overhead :: (!Int,!Real,!Real)
clock_speed_and_profile_overhead
//	| define_fltused True
//		=: measure_clock_speed_and_profile_overhead
//		= measure_clock_speed_and_profile_overhead
		= (0,3100.0,0.0)

get_compute_time_function :: !*File -> (!(Int,Int,Int) -> Real,!*File)
get_compute_time_function file
	# (_,clock_speed,overhead) = clock_speed_and_profile_overhead
	= (compute_time_x86 (clock_speed*1.0E6) overhead,file)

TwoPower32Real:==4294967296.0

compute_time_x86 :: a .Real -> .((b,.Int,c) -> Real) | toReal a & toReal b & toReal c;
compute_time_x86 processor_clock profile_overhead
	= \ (time_hi,time_lo,n_profiler_calls)
		-> ( toReal time_hi*TwoPower32Real 
		     + (if (time_lo>=0) (toReal time_lo) (TwoPower32Real+toReal time_lo))
		   )/toReal processor_clock
		   - (toReal n_profiler_calls*profile_overhead/toReal processor_clock)
