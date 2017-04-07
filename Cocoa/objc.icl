implementation module Cocoa.objc

import StdEnv
import System._Pointer
import Cocoa.dyncall

////

objc_msgSendPtr :: Pointer
objc_msgSendPtr =: dlsym -2 "objc_msgSend\0"

objc_msgSend_stretPtr :: Pointer
objc_msgSend_stretPtr =: dlsym -2 "objc_msgSend_stret\0"

objc_getClass :: !CString !*a -> (!Class,!*a)
objc_getClass name env = code {
		ccall objc_getClass "s:p:A"
	}

objc_allocateClassPair :: !Class !CString !Int !*a -> (!Class,!*a)
objc_allocateClassPair superclass name extraBytes env = code {
		ccall objc_allocateClassPair "psI:p:A"
	}

objc_registerClassPair :: !Class !*a -> *a
objc_registerClassPair cls env = snd (objc_registerClassPair cls env)
where
	objc_registerClassPair :: !Class !*a -> (!Int,!*a)
	objc_registerClassPair cls env = code {
			ccall objc_registerClassPair "p:I:A"
		}

sel_getUid :: !CString !*a -> (!SEL,!*a)
sel_getUid str env = code {
		ccall sel_getUid "s:p:A"
	}

sel_getName :: !SEL -> String
sel_getName sel = derefString (sel_getName sel)
where
	sel_getName :: !SEL -> Pointer
	sel_getName sel = code {
			ccall sel_getName "p:p"
		}

object_getClass :: !Pointer -> Class
object_getClass id = code {
		ccall object_getClass "p:p"
	}

object_getClassName :: !ID -> String
object_getClassName obj = derefString (object_getClassName obj)
where
	object_getClassName :: !ID -> Pointer
	object_getClassName obj = code {
			ccall object_getClassName "p:p"
		}

//Ivar object_setInstanceVariable(id obj, const char *name, void *value)
object_setInstanceVariable :: !ID !CString !Int !*a -> (!Ivar,!*a)
object_setInstanceVariable obj name value env = code {
		ccall object_setInstanceVariable "psp:p:A"
	}

//Ivar object_getInstanceVariable(id obj, const char *name, void **outValue)
object_getInstanceVariable :: !ID !CString !*a -> (!Int,!*a)
object_getInstanceVariable obj name env
	#!	(ivar,outvalue,env)	= object_getInstanceVariable obj name env
	= (outvalue,env)
where
	object_getInstanceVariable :: !ID !CString !*a -> (!Int,!Int,!*a)
	object_getInstanceVariable obj name env = code {
			ccall object_getInstanceVariable "ps:pp:A"
		}

class_getName :: !Class !*a -> (!String,!*a)
class_getName cls env = (derefString (class_getName cls), env)
where
	class_getName :: !Class -> Pointer
	class_getName cls = code {
			ccall class_getName "p:p"
		}

class_addMethod :: !Class !SEL !IMP !CString !*a -> (!Int,!*a)
class_addMethod cls sel imp types env = code {
		ccall class_addMethod "ppps:I:A"
	}

class_replaceMethod :: !Class !SEL !IMP !CString !*a -> (!Int,!*a)
class_replaceMethod cls sel imp types env = code {
		ccall class_replaceMethod "ppps:I:A"
	}

// BOOL class_addIvar(Class cls, const char *name, size_t size, uint8_t alignment, const char *types)
class_addIvar :: !Class !CString !Int !Int !CString !*a -> (!BOOL,!*a)
class_addIvar cls name size alignment types env = code {
		ccall class_addIvar "psIIs:I:A"
	}

class_getInstanceMethod :: !Class !SEL -> Method
class_getInstanceMethod aClass aSelector = code {
		ccall class_getInstanceMethod "pp:p"
	}

class_getClassMethod :: !Class !SEL -> Method
class_getClassMethod aClass aSelector = code {
		ccall class_getClassMethod "pp:p"
	}

// Method * class_copyMethodList(Class cls, unsigned int *outCount)
// To get the class methods of a class, use class_copyMethodList(object_getClass(cls), &count).
class_copyMethodList :: !Class -> (!Method,!Int)
class_copyMethodList aClass = code {
		ccall class_copyMethodList "p:pI"
	}

class_createInstance :: !Class !Int !*a -> (!ID,!*a)
class_createInstance cls xtra env = code {
		ccall class_createInstance "pI:p:A"
	}

// const char * method_getTypeEncoding( Method method)
method_getTypeEncoding :: !Method -> String
method_getTypeEncoding aMethod = derefString (method_getTypeEncoding aMethod)
where
	method_getTypeEncoding :: !Method -> Pointer
	method_getTypeEncoding aMethod = code {
			ccall method_getTypeEncoding "p:p"
		}

method_copyReturnType :: !Method -> String
method_copyReturnType aMethod = derefString (method_copyReturnType aMethod)
where
	method_copyReturnType :: !Method -> Pointer
	method_copyReturnType aMethod = code {
			ccall method_copyReturnType "p:p"
		}

method_getNumberOfArguments :: !Method -> Int
method_getNumberOfArguments _ = code {
		ccall method_getNumberOfArguments "p:I"
	}

method_copyArgumentType :: !Method !Int -> String
method_copyArgumentType aMethod anIndex = derefString (method_copyArgumentType aMethod anIndex)
where
	method_copyArgumentType :: !Method !Int -> Pointer
	method_copyArgumentType aMethod anIndex = code {
			ccall method_copyArgumentType "pI:p"
		}

NSSelectorFromString :: !NSString !*a -> (!SEL,!*a)
NSSelectorFromString selectorName env = code {
		ccall NSSelectorFromString "p:p:A"
	}

/* An Application's startup function */
//APPKIT_EXTERN int NSApplicationMain(int argc, const char *argv[]);
NSApplicationMain :: !Int !Int !*a -> (!Int,!*a)
NSApplicationMain argc argv env = code {
		ccall NSApplicationMain "Gpp:I:A"
	}

/* NSApplicationLoad should be called when loading a Cocoa bundle in a Carbon app in order to initialize NSApplication and other Cocoa objects.  Redundant calls are ignored. */  
//APPKIT_EXTERN BOOL NSApplicationLoad(void);
NSApplicationLoad :: !*a -> (!Int,!*a)
NSApplicationLoad env = code {
		ccall NSApplicationLoad ":I:A"
	}

logEncoding :: !CString !CString !*World -> (!*String,!*World)
logEncoding  clsS selS world
	#!	(cls,world)		= objc_getClass clsS world
		(sel,world)		= sel_getUid selS world
		methodC			= class_getClassMethod cls sel
		methodI			= class_getInstanceMethod cls sel

		method			= methodC + methodI
	| method == 0
		= abort "\nmethod not found\n\n"
	#!	method_type		= if (method == methodC) "+" "-"
		encoding		= method_getTypeEncoding method
		str				= method_type +++. clsS +++. "." +++. selS +++. "\t" +++. encoding
//		io				= io <<< str <<< '\n'
//		io				= io <<< "returns: " <<< method_copyReturnType method <<< '\n'
//		cnt				= method_getNumberOfArguments method
//		io				= logArgs 0 cnt method io
//		io = io <<< "exiting logEncoding\n"
	= (str,world)
where
	logArgs :: !Int !Int !Int !*File -> *File
	logArgs n cnt method io
		| n >= cnt = io
		#!	io	= io <<< "arg " <<< n <<< ": " <<< method_copyArgumentType method n <<< '\n'
		= logArgs (inc n) cnt method io

