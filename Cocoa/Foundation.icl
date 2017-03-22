implementation module Cocoa.Foundation

import StdEnv
import System._Pointer
import System._Posix
import System.CommandLine 
import Text		
import Cocoa.objc
import Cocoa.msg
import Cocoa.dyncall

import StdDebug

traceMsg :: !String !Int !Int !Int -> Bool
traceMsg msg self cmd notification
	#!	sSelf	= object_getClassName self
		sCmd	= sel_getName cmd
		sNot	= object_getClassName notification
	= trace_n (join "\t" [msg,toString self,toString cmd,toString notification,sSelf,sCmd,sNot]) False

allocObject :: !String !*a -> (!Int,!*a)
allocObject classname env
	= msgC_P (packString classname) "alloc\0" env

objectAtIndex :: !Int !Int !*a -> (!Int,!*a)
objectAtIndex object index env
	= msgII_P object "objectAtIndex:\0" index env

subviews :: !Int !*a -> (!Int,!*a)
subviews view env
	= msgI_P view "subviews\0" env

count :: !Int !*a -> (!Int,!*a)
count object env
	= msgI_I object "count\0" env

initObject :: !Int !*a -> (!Int,!*a)
initObject object env
	= msgI_P object "init\0" env

retain :: !Int !*a -> *a
retain object env
	= msgI_V object "retain\0" env

initWithFrame :: !Int !Int !*a -> (!Int,!*a)
initWithFrame object rect env
	= msgIS_P object "initWithFrame:\0" NSRectType rect env

setButtonType :: !NSControl !Int !*a -> *a
setButtonType but typ env
	= msgII_V but "setButtonType:\0" typ env

setBezelStyle :: !NSControl !Int !*a -> *a
setBezelStyle but sty env
	= msgII_V but "setBezelStyle:\0" sty env

setTitle :: !NSObject !String !*a -> *a
setTitle object title env
	= msgIP_V object "setTitle:\0" (p2ns title) env

sizeToFit :: !NSObject !*a -> *a
sizeToFit object env
	= msgI_V object "sizeToFit\0" env

setAction :: !NSObject !String !*a -> *a
setAction object action env
	#!	(action`,env)	= sel_getUid action env
	= msgIP_V object "setAction:\0" action` env

setFrameOrigin :: !NSObject !Real !Real !*a -> *a
setFrameOrigin object x y env
	= msgIRR_V object "setFrameOrigin:\0" x y env

createClass` :: !String !String ![(String,Int,String)] !*a -> *a
createClass` superclass classname methods world
	#!	(cls,world)		= objc_getClass (packString superclass) world
		(adc,world)		= objc_allocateClassPair cls (packString classname) 0 world
		world			= foldl (createMethod adc) world methods
		world			= objc_registerClassPair adc world
	= world

createClass :: !String !String ![(String,Int,String)] ![(String,Int,Int,String)] !*a -> *a
createClass superclass classname methods ivars world
	#!	(cls,world)		= objc_getClass (packString superclass) world
		(adc,world)		= objc_allocateClassPair cls (packString classname) 0 world
		world			= foldl (createMethod adc) world methods
		world			= foldl (createIvar adc) world ivars
		world			= objc_registerClassPair adc world
	= world

createMethod :: !Int !*a !(!String,!Int,!String) -> *a
createMethod adc world (name,imp,typ)
		#!	(sel,world)		= sel_getUid (packString name) world
			(ok,world)		= class_addMethod adc sel imp typ world
		| ok == 42 = abort ("\nerror adding method for class: "+++name+++"\n\n")
		= world
createIvar :: !Int !*a !(!String,!Int,!Int,!String) -> *a
createIvar adc world (name,size,alignment,typ)
		#!	(ok,world)		= class_addIvar adc (packString name) size alignment typ world
		| ok ==42 = abort ("\nerror adding ivar for class: "+++name+++"\n\n")
		= world


swizzleMethod :: !String !(!String,!Int,!String) !*a -> (!Int,!*a)
swizzleMethod classname (selector,imp,typ) world
	#!	(cls, world)	= objc_getClass (packString classname) world
		(sel, world)	= sel_getUid (packString selector) world
		(old,world)		= class_replaceMethod cls sel imp typ world
	= (old,world)

applicationDelegate :: !*a -> (!NSObject,!*a)
applicationDelegate env
	#!	(application,env)	= msgC_P "NSApplication\0" "sharedApplication\0" env
	= msgI_P application "delegate\0" env

sharedApplication :: !*a -> (!NSApplication,!*a)
sharedApplication world
	= msgC_P "NSApplication\0" "sharedApplication\0" world

runApplication :: !NSApplication !*a -> *a
runApplication app world
	= msgI_V app "run\0" world

// 	[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
NSApplicationActivationPolicyRegular	:== 0

makeUnbundledLaunchable :: !NSApplication !*a -> *a
makeUnbundledLaunchable app env
	= msgII_V app "setActivationPolicy:\0" NSApplicationActivationPolicyRegular env

openDocument :: !String !*a -> *a
openDocument path world
	#!	(sdc,world)		= msgC_P "NSDocumentController\0" "sharedDocumentController\0" world
		(url,world)		= msgCP_P "NSURL\0" "fileURLWithPath:\0" (p2ns path) world
		(doc,world)		= msgIP_P sdc "documentForURL:\0" url world
	| trace_n ("sdc: '"+++toString sdc+++"'") False = undef
	| trace_n ("url: '"+++toString url+++"'") False = undef
	| trace_n ("doc: '"+++toString doc+++"'") False = undef

	#!	errorHdl		= malloc 8	// do we also need to alloc the Ptr??
		errorHdl		= writeInt errorHdl 0 0
//	#!	errorObj	= readInt errorHdl 0
//	| trace_n ("<error:\t"+++toString errorHdl+++"\t"+++toString errorObj) False = undef
	#!	(str,world)		= msgIPP_P sdc "typeForContentsOfURL:error:\0" url errorHdl world
	#!	(error,world)	= getError errorHdl world
		string			= ns2cls str
	| trace_n ("Error: '"+++error+++"'") False = undef
	| trace_n ("Type: '"+++string+++"'") False = undef
	#!	(cls,world)		= msgIP_P sdc "documentClassForType:\0" str world
	| trace_n ("cls: '"+++toString cls+++"'") False = undef
	#!	(nme,world)		= class_getName cls world
	| trace_n ("nme: '"+++toString nme+++"'") False = undef
	= world

getError :: !Pointer !*a -> (!String,!*a)
getError errorHdl env
	#!	errorObj	= readInt errorHdl 0
	| errorObj == 0
		= ("",env)
	#!	(err,env)	= msgI_P errorObj "localizedDescription\0" env
	= (ns2cls err,env)

// next two need to free their results sometime... or only alloc once
getBounds :: !Pointer !*a -> (!Pointer,!*a)
getBounds self env
	#!	(sel,env)		= sel_getUid "bounds\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
//		env = trace_n ("getBounds\t"+++toString self+++"\t"+++toString sel+++"\t"+++toString ptr+++"\t"+++toString ptr`) env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

getFrame :: !Pointer !*a -> (!Pointer,!*a)
getFrame self env
	#!	(sel,env)		= sel_getUid "frame\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
//		env = trace_n ("getFrame\t"+++toString self+++"\t"+++toString sel+++"\t"+++toString ptr+++"\t"+++toString ptr`) env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

ns2cls :: !NSString -> String
ns2cls cfstr
	| cfstr == 0
		= ""
	#!	bsize	= 1024*1024
		buff	= malloc bsize
		ok		= CFStringGetCString cfstr buff bsize 0x08000100
	| ok <> 0
		#	str		= derefString buff
		= force (free buff) str
	= abort ("\nerror in ns2cls ("+++toString cfstr+++")\n\n")
	
//Boolean CFStringGetCString(CFStringRef theString, char *buffer, CFIndex bufferSize, CFStringEncoding encoding
CFStringGetCString :: !Pointer !Pointer !Int !Int -> Int
CFStringGetCString _ _ _ _ = code {
		ccall CFStringGetCString "ppII:I"
	}

c2ns :: !CString -> NSString
c2ns cStr = CFStringCreateWithCString 0 cStr 0

CFStringCreateWithCString :: !Int !ZString !Int -> NSString
CFStringCreateWithCString alloc cStr encoding = code {
		ccall CFStringCreateWithCString "IsI:p"
	}

p2ns :: !String -> NSString
p2ns pStr
//	= CFStringCreateWithPascalString 0 pStr 0
	= CFStringCreateWithBytes 0 pStr (size pStr) 0 0

CFStringCreateWithPascalString :: !Int !String !Int -> NSString
CFStringCreateWithPascalString alloc pStr encoding = code {
		ccall CFStringCreateWithPascalString "ISI:p"
	}
CFStringCreateWithBytes :: !Int !String !Int !Int !Int -> NSString
CFStringCreateWithBytes alloc bytes numbytes encoding isexternal = code {
		ccall CFStringCreateWithBytes "IsIII:p"
	}
//CFStringRef CFStringCreateWithBytes(CFAllocatorRef alloc, const UInt8 *bytes, CFIndex numBytes, CFStringEncoding encoding, Boolean isExternalRepresentation);

CFRelease :: !Int !*a -> *a
CFRelease ptr env = code {
		ccall CFRelease "p:V:A"
	}

cgRect :: !Real !Real !Real !Real -> Pointer
cgRect x y w h
	#!	ptr	= malloc 32
		ptr	= writeReal8 ptr 0 x 
		ptr	= writeReal8 ptr 8 y
		ptr	= writeReal8 ptr 16 w 
		ptr	= writeReal8 ptr 24 h 
	= ptr

rect2string :: !Pointer -> String
rect2string bounds
	#!	origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
	= "{x="+++toString origin_x+++",y="+++toString origin_y+++",w="+++toString size_w+++",h="+++toString size_h+++"}"

application :: NSApplication
application
	#!	(app,world) 	= msgC_P "NSApplication\0" "sharedApplication\0" newWorld
	= app

applicationName :: String
applicationName
	#!	(cls,env)		= msgC_P "NSBundle\0" "mainBundle\0" newWorld
	| cls == 0
		= commandlineName
	#!	(cfstr,env)		= msgIP_P cls "objectForInfoDictionaryKey:\0" (c2ns "CFBundleName\0") env
	| cfstr == 0
		= commandlineName
	= ns2cls cfstr		

commandlineName
	#!	(args,env)	= getCommandLine newWorld
		arg0		= args!!0  
		idx			= findSep 0 arg0
	= arg0%(inc idx,size arg0 - 1)
where
	findSep i s
		| i >= size s	= -1
		| s.[i] == '/'	= max i (findSep (inc i) s)
		= findSep (inc i) s

applicationPath :: String
applicationPath
	#!	(cls,env)		= msgC_P "NSBundle\0" "mainBundle\0" newWorld
	| cls == 0
		= commandlinePath
	#!	(cfstr,env)		= msgI_P cls "bundlePath\0" env
	| cfstr == 0
		= commandlinePath
	= ns2cls cfstr		

commandlinePath
	#!	buffer			= createArray 2048 '\0'
		(ptr,env)		= getcwd buffer 2048 newWorld	// also not correct if launched from Terminal/script
		cwd				= derefString ptr
	= cwd		// not entirely correct.. only if app is ./app ~> more fixup required!

///// dynlib dependant


NSRectType :: DCStruct
NSRectType =: makeNSRectType
where
	makeNSRectType :: Int
	makeNSRectType
		#!	(rectT,env)		= dcNewStruct 4 32 newWorld
			type			= toInt 'd'
			alignment		= 8	// default alignment
			env				= dcStructField rectT type alignment 1 env
			env				= dcStructField rectT type alignment 1 env
			env				= dcStructField rectT type alignment 1 env
			env				= dcStructField rectT type alignment 1 env
			env				= dcCloseStruct rectT env
//			env = trace_n ("NSRectType: "+++logDCStruct rectT) env
			fields	= readInt rectT 0
			fields	= writeInt fields 0 0 + 48
			fields	= writeInt fields 0 8 + 48
			fields	= writeInt fields 0 16 + 48
			fields	= writeInt fields 0 24 + 48
		| fields == 0 = undef
//		#!	env = trace_n ("NSRectType: "+++logDCStruct rectT) env
		= force env rectT

NSSizeType :: DCStruct
NSSizeType =: makeNSRectType
where
	makeNSRectType :: Int
	makeNSRectType
		#!	(rectT,env)		= dcNewStruct 2 32 newWorld
			type			= toInt 'd'
			alignment		= 8	// default alignment
			env				= dcStructField rectT type alignment 1 env
			env				= dcStructField rectT type alignment 1 env
			env				= dcCloseStruct rectT env
//			env = trace_n ("NSSizeType: "+++logDCStruct rectT) env
			fields	= readInt rectT 0
			fields	= writeInt fields 0 0 + 48
			fields	= writeInt fields 0 8 + 48
		| fields == 0 = undef
//		#!	env = trace_n ("NSSizeType: "+++logDCStruct rectT) env
		= force env rectT

NSMakeSize :: !Real !Real -> Pointer
NSMakeSize w h
	#!	ptr	= malloc 16
		ptr	= writeReal8 ptr 0 w
		ptr	= writeReal8 ptr 8 h
	= ptr

sizeHeight :: !Pointer -> Real
sizeHeight ptr = readReal8 ptr 8


///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

