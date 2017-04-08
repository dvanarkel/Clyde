definition module Cocoa.objc

from System._Pointer import :: Pointer 

:: CString :== String	// zero-terminated

:: Class	:== Pointer
:: SEL		:== Pointer
:: IMP		:== Pointer
:: ID		:== Pointer
:: Method	:== Pointer
:: NSString	:== Pointer
:: Ivar		:== Pointer
:: BOOL		:== Int		// Int or can we convert straight to Bool???
:: VOID		:== Int		// type for void returns...

YES						:== 1
NO						:== 0

objc_msgSendPtr :: Pointer
objc_msgSend_stretPtr :: Pointer

objc_getClass :: !CString !*a -> (!Class,!*a)
objc_allocateClassPair :: !Class !CString !Int !*a -> (!Class,!*a)
objc_registerClassPair :: !Class !*a -> *a

sel_getUid :: !CString !*a -> (!SEL,!*a)
sel_getName :: !SEL -> String

object_getClass :: !Pointer -> Class
object_getClassName :: !ID -> String
object_setInstanceVariable :: !ID !CString !Int !*a -> (!Ivar,!*a)
object_getInstanceVariable :: !ID !CString !*a -> (!Int,!*a)

class_getName :: !Class !*a -> (!String,!*a)
class_addMethod :: !Class !SEL !IMP !CString !*a -> (!Int,!*a)
class_replaceMethod :: !Class !SEL !IMP !CString !*a -> (!Int,!*a)
class_addIvar :: !Class !CString !Int !Int !CString !*a -> (!BOOL,!*a)
class_getInstanceMethod :: !Class !SEL -> Method
class_getClassMethod :: !Class !SEL -> Method
class_copyMethodList :: !Class -> (!Method,!Int)
class_createInstance :: !Class !Int !*a -> (!ID,!*a)

method_copyReturnType :: !Method -> String
method_getTypeEncoding :: !Method -> String
method_getNumberOfArguments :: !Method -> Int
method_copyArgumentType :: !Method !Int -> String

NSSelectorFromString :: !NSString !*a -> (!SEL,!*a)
NSApplicationMain :: !Int !Int !*a -> (!Int,!*a)
NSApplicationLoad :: !*a -> (!Int,!*a)

logEncoding :: !CString !CString !*World -> (!*String,!*World)

/*
Table 6-1  Objective-C type encodings

Code			Meaning

c				A char
i				An int
s				A short
l				A long				l is treated as a 32-bit quantity on 64-bit programs.
q				A long long
C				An unsigned char
I				An unsigned int
S				An unsigned short
L				An unsigned long
Q				An unsigned long long
f				A float
d				A double
B				A C++ bool or a C99 _Bool
v				A void
*				A character string (char *)
@				An object (whether statically typed or typed id)
#				A class object (Class)
:				A method selector (SEL)
[array type]	An array
{name=type...}	A structure
(name=type...)	A union
bnum			A bit field of num bits
^type			A pointer to type
?				An unknown type (among other things, this code is used for function pointers)

Table 6-2  Objective-C method encodings
Code			Meaning
r				const
n				in
N				inout
o				out
O				bycopy
R				byref
V				oneway
*/
