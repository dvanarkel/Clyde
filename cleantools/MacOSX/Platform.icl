implementation module Platform;

import StdEnv;

application_path :: !{#Char} -> {#Char};
application_path file_name
	# s_p = get_appl_path_address;
	# end_p = c_string_end s_p;
	# s = {c_string_char p \\ p<-[s_p..end_p-1]}
	= s+++("/"+++file_name);

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
