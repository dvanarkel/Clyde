definition module Cocoa.Foundation

from System._Pointer import ::Pointer
from Cocoa.objc import :: NSString, :: CString
from StdEnv import <<

:: NSApplication :== Int
:: NSControl	:== Pointer
:: NSObject		:== Pointer

traceMsg :: !String !Int !Int !Int -> Bool

allocObject :: !String !*a -> (!Int,!*a)			// [Object alloc]
objectAtIndex :: !Int !Int !*a -> (!Int,!*a)		// objectAtIndex:
subviews :: !Int !*a -> (!Int,!*a)
count :: !Int !*a -> (!Int,!*a)
initObject :: !Int !*a -> (!Int,!*a)
retain :: !Int !*a -> *a
initWithFrame :: !Int !Int !*a -> (!Int,!*a)
setButtonType :: !NSControl !Int !*a -> *a
setBezelStyle :: !NSControl !Int !*a -> *a
setTitle :: !NSObject !String !*a -> *a
sizeToFit :: !NSObject !*a -> *a
setAction :: !NSObject !String !*a -> *a
setFrameOrigin :: !NSObject !Real !Real !*a -> *a


createClass :: !String !String ![(String,Int,String)] !*a -> *a
swizzleMethod :: !String !(!String,!Int,!String) !*a -> (!Int,!*a)

applicationDelegate :: !*a -> (!NSObject,!*a)
sharedApplication :: !*a -> (! NSApplication,!*a)
makeUnbundledLaunchable :: ! NSApplication !*a -> *a
runApplication :: !NSApplication !*a -> *a

openDocument :: !String !*a -> *a

getError :: !Pointer !*a -> (!String,!*a)

getBounds :: !Pointer !*a -> (!Pointer,!*a)
getFrame :: !Pointer !*a -> (!Pointer,!*a)

c2ns :: !CString -> NSString
p2ns :: !String -> NSString
ns2cls :: !NSString -> String

cgRect :: !Real !Real !Real !Real -> Pointer
rect2string :: !Pointer -> String

application :: NSApplication
applicationName :: String
applicationPath :: String

from Cocoa.dyncall import :: DCStruct, :: DCpointer

NSRectType :: DCStruct
NSSizeType :: DCStruct

NSMakeSize :: !Real !Real -> Pointer
sizeHeight :: !Pointer -> Real

::NSStringEncoding	:== Int
NSASCIIStringEncoding				:== 1
NSNEXTSTEPStringEncoding			:== 2
NSJapaneseEUCStringEncoding			:== 3
NSUTF8StringEncoding				:== 4
NSISOLatin1StringEncoding			:== 5
NSSymbolStringEncoding				:== 6
NSNonLossyASCIIStringEncoding		:== 7
NSShiftJISStringEncoding			:== 8
NSISOLatin2StringEncoding			:== 9
NSUnicodeStringEncoding				:== 10
NSWindowsCP1251StringEncoding		:== 11
NSWindowsCP1252StringEncoding		:== 12
NSWindowsCP1253StringEncoding		:== 13
NSWindowsCP1254StringEncoding		:== 14
NSWindowsCP1250StringEncoding		:== 15
NSISO2022JPStringEncoding			:== 21
NSMacOSRomanStringEncoding			:== 30
NSUTF16StringEncoding				:== NSUnicodeStringEncoding
NSUTF16BigEndianStringEncoding		:== 0x90000100
NSUTF16LittleEndianStringEncoding	:== 0x94000100
NSUTF32StringEncoding				:== 0x8c000100
NSUTF32BigEndianStringEncoding		:== 0x98000100
NSUTF32LittleEndianStringEncoding	:== 0x9c000100
NSProprietaryStringEncoding			:== 65536

NSBorderlessWindowMask		:== 0
NSTitledWindowMask			:== 1 << 0
NSClosableWindowMask		:== 1 << 1
NSMiniaturizableWindowMask	:== 1 << 2
NSResizableWindowMask		:== 1 << 3

NSBackingStoreRetained	 	:== 0
NSBackingStoreNonretained	:== 1
NSBackingStoreBuffered	 	:== 2

