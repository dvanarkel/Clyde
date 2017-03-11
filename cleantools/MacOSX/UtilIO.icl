implementation module UtilIO;

import StdEnv;
from UtilDate import ::DATE(..);

GetFullApplicationPath :: !*Files -> (!{#Char},!*Files);
GetFullApplicationPath files
	# s_p = get_appl_path_address;
	# (s,s_p) = c_string_to_clean_string s_p;
	= (s,files);

c_string_to_clean_string :: !Int -> (!.{#Char},!Int);
c_string_to_clean_string s_p
	# end_p = c_string_end s_p;
	= ({c_string_char p \\ p<-[s_p..end_p-1]},s_p);

c_string_end :: !Int -> Int;
c_string_end p
	| c_string_char p<>'\0'
		= c_string_end (p+1);
		= p;

c_string_char :: !Int -> Char;
c_string_char p = code inline {
	load_ui8 0
 }

get_appl_path_address :: Int;
get_appl_path_address = code {
	pushLc appl_path
}

GetLongPathName :: !{#Char} -> {#Char};
GetLongPathName p
	# s_p = realpath (p+++"\0") 0;
	| s_p==0
		= p;
	# (s,s_p) = c_string_to_clean_string s_p
	# r = free s_p
	| r>=0
		= s;
		= s;

FModified :: !String !Files -> (!DATE, !Files);
FModified name files
	# s = createArray (IF_INT_64_OR_32 144 88) '\0';
	# r = stat (name+++"\0") s;
	| r<>0
		= ({exists=False, yy=0, mm=0, dd=0, h=0, m=0, s=0}, files);
		# struct_tm = localtime (s % (48,55));
		| struct_tm==0
			= ({exists=False, yy=0, mm=0, dd=0, h=0, m=0, s=0}, files);
			= (struct_tm_to_DATE struct_tm , files);
	{
		struct_tm_to_DATE struct_tm
			# sec_min=load_long (struct_tm+0);
			# sec=(sec_min<<32)>>32;
			# min=sec_min>>32;
			# hour_day=load_long (struct_tm+8);
			# hour=(hour_day<<32)>>32;
			# day=hour_day>>32;
			# mon_year=load_long (struct_tm+16);
			# mon=((mon_year<<32)>>32)+1;
			# year=(mon_year>>32)+1900;
			= {exists=True, yy=year, mm=mon, dd=day, h=hour, m=min, s=sec};
	}

stat :: !{#Char} !{#Char} -> Int;
stat file_name stat_struct
	= code {
		ccall stat$INODE64 "ss:p"
	}

localtime :: !{#Char} -> Int;
localtime time_t_p
	= code {
		ccall localtime "s:p"
	}

load_long :: !Int -> Int;
load_long p = code {
	load_i 0
}

realpath :: !{#Char} !Int -> Int;
realpath path resolved_path = code {
	ccall realpath "sp:p"
}

free :: !Int -> Int;
free p = code {
	ccall free "p:I"
}

FExists	:: .a;
FExists = abort "FExists";
