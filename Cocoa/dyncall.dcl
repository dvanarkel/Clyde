definition module Cocoa.dyncall

from System._Pointer import :: Pointer

// dyncall types

:: DCsize		:== Int
:: DCCallVM		:== DCpointer
:: DCMode		:== DCint
:: DCbool		:== Bool				// 
:: DCint		:== Int			// C int
:: DCdouble		:== Real		// C double
:: DCpointer	:== Int			// C void*
:: DCStruct		:== DCpointer		// pointer to DCstruct

// dynload types

:: DLLib	:== DCpointer		// pointer

// other types

:: ZString	:== String			// zero terminated string

// DCMode constants

DC_CALL_C_DEFAULT               :==	0
DC_CALL_C_ELLIPSIS            	:== 100

// functions

dcNewCallVM :: !DCsize !*a -> (!DCCallVM,!*a)				// TODO: do something with environment typing...
dcFree :: !DCCallVM !*a -> *a
dcReset :: !DCCallVM !*a -> *a
dcMode :: !DCCallVM !DCMode !*a -> *a

dcArgBool :: !DCCallVM !DCbool !*a -> *a
dcArgInt :: !DCCallVM !DCint !*a -> *a
dcArgDouble :: !DCCallVM !DCdouble !*a -> *a
dcArgPointer :: !DCCallVM !DCpointer !*a -> *a
dcArgArrayPointer :: !DCCallVM !{#elem} !*a -> *a
dcArgStruct :: !DCCallVM !DCStruct !DCpointer !*a -> *a

dcCallVoid :: !DCCallVM !DCpointer !*a -> *a
dcCallInt :: !DCCallVM !DCpointer !*a -> (!DCint,!*a)
dcCallDouble :: !DCCallVM !DCpointer !*a -> (!DCdouble,!*a)
dcCallPointer :: !DCCallVM !DCpointer !*a -> (!DCpointer,!*a)
dcCallStruct :: !DCCallVM !DCpointer !DCStruct !*a -> (!DCpointer,!*a)

dcGetError :: !Pointer !*a -> (!Int,!*a)

dcNewStruct :: !DCsize !DCint !*a -> (!DCStruct,!*a)
dcStructField :: !DCStruct !DCint !DCint !DCsize !*a -> *a
dcCloseStruct :: !DCStruct !*a -> *a
dcFreeStruct :: !DCStruct !*a -> *a
dcDefineStruct :: !ZString !*a -> (!DCStruct,!*a)

dlLoadLibrary :: !ZString !*a -> (!DLLib,!*a)
dlFreeLibrary :: !DLLib !*a -> *a
dlFindSymbol :: !DLLib !ZString !*a -> (!DCpointer,!*a)

dcbArgInt :: !Pointer !*a -> (!Int,!*a)
dcbArgPointer :: !Pointer !*a -> (!Pointer,!*a)
dcbNewCallback :: !ZString !Pointer !Int -> Int

dlsym :: !Int !ZString -> Int
