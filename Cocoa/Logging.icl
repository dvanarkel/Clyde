implementation module Cocoa.Logging

/*
- implement logging
- implement stacktrace [NSThread callStackSymbols]

see http://www.cocoawithlove.com/blog/2016/02/28/stack-traces-in-swift.html
for inspiration :-)
*/

import StdEnv
import StdDebug

import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation

callStackSymbols :: !*a -> *a
callStackSymbols env
    #!	(syms,env)		= msgC_P "NSThread\0" "callStackSymbols\0" env
		(count,env)		= msgI_I syms "count\0" env
	| trace_n ("\nCALLSTACK ("+++toString count+++")\n\n") False = undef
	#!	env				= log 0 count syms env
	| trace_n ("\n\nCALLSTACK DONE\n\n") False = undef
	= env
where
	log :: !Int !Int !Pointer !*a -> *a
	log index count syms env
		| index >= count
			= env
		#!	(sym,env)		= msgII_P syms "objectAtIndex:\0" index env
		| trace_n (ns2cls sym) False = undef
		= log (inc index) count syms env

/* ASL LOGGING

man asl
https://developer.apple.com/library/mac/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/LoggingErrorsAndWarnings.html
http://boredzo.org/blog/archives/2008-01-20/why-asl
https://developer.apple.com/library/mac/technotes/tn2124/_index.html

by default, messages below the "notice" level will not be visible in the system log. Additionally, if we tried using the MWLogDebug as it is now, we wouldn't see anything in the debug console.
Fortunately ASL let's us send our messages to additional file descriptors. If we add the STDERR file descriptor to the logging facility, we'll be able to see our messages in the debugger:
asl_add_log_file(NULL, STDERR_FILENO);

    asl_log(NULL, NULL, (LEVEL), "%s", [message UTF8String]); \

asl_set_filter(NULL, ASL_FILTER_MASK_UPTO(ASL_LEVEL_DEBUG));

asl_set_filter(NULL, ASL_FILTER_MASK_UPTO(ASL_LEVEL_DEBUG)); doesn't work (at least on the simulator) b/c the syslog filters out debug level logging. You have either change the syslog filtering temporarily with 
sudo syslog -c syslogd -d
or more permanently in /System/Library/LaunchDaemons/com.apple.syslogd.plist

-(void) redirectNSLogToDocuments {
 NSArray *allPaths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
 NSString *documentsDirectory = [allPaths objectAtIndex:0];
 NSString *pathForLog = [documentsDirectory stringByAppendingPathComponent:@"yourFile.txt"];
 freopen([pathForLog cStringUsingEncoding:NSASCIIStringEncoding],"a+",stderr);

*/


/*
console redirection for launched tasks...
https://gist.github.com/atr000/621601
*/
