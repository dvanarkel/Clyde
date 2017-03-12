implementation module Cocoa.UserDefaults

import StdEnv
import Cocoa.msg
import Cocoa.Foundation

registerApplicationDefaults :: !*env -> *env
registerApplicationDefaults env
	#!	(dict,env)	= getDictionary "Defaults" env
	= registerDefaults dict env

standardUserDefaults :: !*env -> (!Pointer,!*env)
standardUserDefaults env
	= msgC_P "NSUserDefaults\0" "standardUserDefaults\0" env

registerDefaults :: !Pointer !*env -> *env
registerDefaults dictionary env
	#!	(defaults,env)	= msgC_P "NSUserDefaults\0" "standardUserDefaults\0" env
		env				= msgIP_V defaults "registerDefaults:\0" dictionary env
	= env

stringForKey :: !String !*env -> (!String,!*env)
stringForKey key env
	#!	(defaults,env)	= msgC_P "NSUserDefaults\0" "standardUserDefaults\0" env
		(ns,env)		= msgIP_P defaults "stringForKey:\0" (p2ns key) env
	= (ns2cls ns,env)

setObjectForKey :: !String !Pointer !*env -> *env
setObjectForKey key value env
	#!	(defaults,env)	= msgC_P "NSUserDefaults\0" "standardUserDefaults\0" env
		(_,env)			= msgIPP_P defaults "setObject:ForKey:\0" value (p2ns key) env	// actually IPP_V
	= env

setStringForKey :: !String !String !*env -> *env
setStringForKey key val env
	= setObjectForKey key (p2ns val) env

getDictionary :: !String !*env -> (!Pointer,!*env)
getDictionary resname env
	#!	(cls,env)		= msgC_P "NSBundle\0" "mainBundle\0" env
		(pth,env)		= msgIPP_P cls "pathForResource:ofType:" (p2ns resname) (p2ns "plist") env
		(dict,env)		= msgC_P "NSDictionary\0" "alloc\0" env
		(dict,env)		= msgIP_P dict "initWithContentsOfFile:\0" pth env
	= (dict,env)

