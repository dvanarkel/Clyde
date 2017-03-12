definition module Cocoa.UserDefaults

registerApplicationDefaults :: !*env -> *env	// register defaults from <app bundle>/Resources/Defaults.plist as fallback defaults

stringForKey :: !String !*env -> (!String,!*env)	// get value for key
setStringForKey :: !String !String !*env -> *env	// set key, value

