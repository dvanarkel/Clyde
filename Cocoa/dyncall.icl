implementation module Cocoa.dyncall

import StdTuple, StdMisc
import System._Pointer
import System._Posix

/* Supported Calling Convention Modes */

DC_CALL_C_DEFAULT               :==	0
DC_CALL_C_ELLIPSIS            	:== 100
DC_CALL_C_ELLIPSIS_VARARGS    	:== 101
DC_CALL_C_X86_CDECL             :== 1
DC_CALL_C_X86_WIN32_STD         :== 2
DC_CALL_C_X86_WIN32_FAST_MS     :== 3
DC_CALL_C_X86_WIN32_FAST_GNU    :== 4
DC_CALL_C_X86_WIN32_THIS_MS     :== 5
DC_CALL_C_X86_WIN32_THIS_GNU    :== 6
DC_CALL_C_X64_WIN64             :== 7
DC_CALL_C_X64_SYSV              :== 8
DC_CALL_C_PPC32_DARWIN          :== 9
DC_CALL_C_PPC32_OSX            	:== DC_CALL_C_PPC32_DARWIN /* alias */
DC_CALL_C_ARM_ARM_EABI         	:== 10
DC_CALL_C_ARM_THUMB_EABI       	:== 11
DC_CALL_C_ARM_ARMHF            	:== 30
DC_CALL_C_MIPS32_EABI          	:== 12
DC_CALL_C_MIPS32_PSPSDK        	:== DC_CALL_C_MIPS32_EABI /* alias - deprecated. */
DC_CALL_C_PPC32_SYSV           	:== 13
DC_CALL_C_PPC32_LINUX          	:== DC_CALL_C_PPC32_SYSV /* alias */
DC_CALL_C_ARM_ARM              	:== 14
DC_CALL_C_ARM_THUMB            	:== 15
DC_CALL_C_MIPS32_O32           	:== 16
DC_CALL_C_MIPS64_N32           	:== 17
DC_CALL_C_MIPS64_N64           	:== 18
DC_CALL_C_X86_PLAN9            	:== 19
DC_CALL_C_SPARC32              	:== 20
DC_CALL_C_SPARC64              	:== 21
DC_CALL_C_ARM64                	:== 22
DC_CALL_C_PPC64                	:== 23
DC_CALL_C_PPC64_LINUX          	:== DC_CALL_C_PPC64 /* alias */
DC_CALL_SYS_DEFAULT           	:== 200
DC_CALL_SYS_X86_INT80H_LINUX  	:== 201
DC_CALL_SYS_X86_INT80H_BSD    	:== 202
DC_CALL_SYS_PPC32             	:== 210
DC_CALL_SYS_PPC64             	:== 211

/* Error codes. */

DC_ERROR_NONE                	:== 0
DC_ERROR_UNSUPPORTED_MODE   	:== -1

dcNewCallVM :: !DCsize !*a -> (!DCCallVM,!*a)				// TODO: do something with environment typing...
dcNewCallVM dc_size env = (dcNewCallVM dc_size,env)			// TODO: consider adding finalizer to auto dcFree
where
	dcNewCallVM :: !DCsize -> DCCallVM
	dcNewCallVM _ = code {
		ccall dcNewCallVM "I:p"
	}

dcFree :: !DCCallVM !*a -> *a
dcFree vm env = snd (dcFree vm env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcFree :: !DCCallVM !*a -> (!Int,!*a)
	dcFree _ _ = code {
			ccall dcFree "p:I:A"
		}

dcReset :: !DCCallVM !*a -> *a
dcReset vm env = snd (dcReset vm env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcReset :: !DCCallVM !*a -> (!Int,!*a)
	dcReset _ _ = code {
			ccall dcReset "p:I:A"
		}

dcMode :: !DCCallVM !DCMode !*a -> *a
dcMode vm mode env = snd (dcMode vm mode env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcMode :: !DCCallVM !DCMode !*a -> (!Int,!*a)
	dcMode _ _ _ = code {
			ccall dcMode "pI:I:A"
		}

// ...

dcArgBool :: !DCCallVM !DCbool !*a -> *a
dcArgBool vm value env = snd (dcArgBool vm (if value '\1' '\0') env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcArgBool :: !DCCallVM !Char !*a -> (!Int,!*a)
	dcArgBool _ _ _ = code {
			ccall dcArgBool "pI:I:A"
		}

dcArgInt :: !DCCallVM !DCint !*a -> *a
dcArgInt vm value env = snd (dcArgInt vm value env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcArgInt :: !DCCallVM !DCint !*a -> (!Int,!*a)
	dcArgInt _ _ _ = code {
			ccall dcArgInt "pI:I:A"
		}

dcArgDouble :: !DCCallVM !DCdouble !*a -> *a
dcArgDouble vm value env = snd (dcArgDouble vm value env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcArgDouble :: !DCCallVM !DCdouble !*a -> (!Int,!*a)
	dcArgDouble _ _ _ = code {
			ccall dcArgDouble "pR:I:A"
		}

dcArgPointer :: !DCCallVM !DCpointer !*a -> *a
dcArgPointer vm value env = snd (dcArgPointer vm value env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcArgPointer :: !DCCallVM !DCpointer !*a -> (!Int,!*a)
	dcArgPointer _ _ _ = code {
			ccall dcArgPointer "pp:I:A"
		}

dcArgArrayPointer :: !DCCallVM !{#elem} !*a -> *a
dcArgArrayPointer vm value env = snd (dcArgPointer vm value env)
where	// TODO: think we have to do this to avoid void return issues on current x64 cg
	dcArgPointer :: !DCCallVM !{#elem} !*a -> (!Int,!*a)
	dcArgPointer _ _ _ = code {
			ccall dcArgPointer "ps:I:A"
		}

dcArgStruct :: !DCCallVM !DCStruct !DCpointer !*a -> *a
dcArgStruct vm s value env = snd (dcArgStruct vm s value env)
where
	dcArgStruct :: !DCCallVM !DCStruct !DCpointer !*a -> (!Int,!*a)
	dcArgStruct _ _ _ _ = code {
			ccall dcArgStruct "ppp:I:A"
		}
		
// ...

dcCallVoid :: !DCCallVM !DCpointer !*a -> *a
dcCallVoid vm funcptr env = snd (dcCallVoid vm funcptr env)
where
	dcCallVoid :: !DCCallVM !DCpointer !*a -> (!Int,!*a)
	dcCallVoid vm funcptr env = code {
			ccall dcCallVoid "Gpp:I:A"
		}

dcCallInt :: !DCCallVM !DCpointer !*a -> (!DCint,!*a)
dcCallInt vm funcptr env = code {
		ccall dcCallInt "Gpp:I:A"
	}

dcCallDouble :: !DCCallVM !DCpointer !*a -> (!DCdouble,!*a)
dcCallDouble vm funcptr env = code {
		ccall dcCallDouble "Gpp:R:A"
	}

dcCallPointer :: !DCCallVM !DCpointer !*a -> (!DCpointer,!*a)
dcCallPointer vm funcptr env = code {
		ccall dcCallPointer "Gpp:p:A"
	}

dcCallStruct :: !DCCallVM !DCpointer !DCStruct !*a -> (!DCpointer,!*a)
dcCallStruct vm funcptr s env
	| True
		= abort "\ndcCallStruct not supported by the library implementation!\n\n"
	#!	ptr			= malloc 32
		(_,ret,env)	= dcCallStruct vm funcptr s ptr env
	= (ret,env)
//	= dcCallStruct vm funcptr s ptr env
//	= (returnValue,env)
where
	dcCallStruct :: !DCCallVM !DCpointer !DCStruct !Pointer !*a -> (!Int,!DCpointer,!*a)
	dcCallStruct vm funcptr s ptr env = code {
			 ccall dcCallStruct "Gppp:Vp:A"
			| ccall dcCallStruct "Gppp:Ip:A"
		}

dcGetError :: !Pointer !*a -> (!Int,!*a)
dcGetError _ _ = code {
		ccall dcGetError "p:I:A"
	}

dcNewStruct :: !DCsize !DCint !*a -> (!DCStruct,!*a)
dcNewStruct fieldCount alignment env = code {
		ccall dcNewStruct "II:p:A"
	}

dcStructField :: !DCStruct !DCint !DCint !DCsize !*a -> *a
dcStructField s type alignment arrayLength env = snd (dcStructField s type alignment arrayLength env)
where
	dcStructField :: !DCStruct !DCint !DCint !DCsize !*a -> (!Int,!*a)
	dcStructField _ _ _ _ _ = code {
			ccall dcStructField "pIII:I:A"
		}

dcCloseStruct :: !DCStruct !*a -> *a
dcCloseStruct s env = snd (dcCloseStruct s env)
where
	dcCloseStruct :: !DCStruct !*a -> (!Int,!*a)
	dcCloseStruct _ _ = code {
			ccall dcCloseStruct "p:I:A"
		}

dcFreeStruct :: !DCStruct !*a -> *a
dcFreeStruct s env = snd (dcFreeStruct s env)
where
	dcFreeStruct :: !DCStruct !*a -> (!Int,!*a)
	dcFreeStruct _ _ = code {
			ccall dcFreeStruct "p:I:A"
		}

dcDefineStruct :: !ZString !*a -> (!DCStruct,!*a)
dcDefineStruct signature env = code {
		ccall dcDefineStruct "s:p:A"
	}

/**********
 DC_API void       dcArgBool       (DCCallVM* vm, DCbool     value);
DC_API void       dcArgChar       (DCCallVM* vm, DCchar     value);
DC_API void       dcArgShort      (DCCallVM* vm, DCshort    value);
 DC_API void       dcArgInt        (DCCallVM* vm, DCint      value);
DC_API void       dcArgLong       (DCCallVM* vm, DClong     value);
DC_API void       dcArgLongLong   (DCCallVM* vm, DClonglong value);
DC_API void       dcArgFloat      (DCCallVM* vm, DCfloat    value);
 DC_API void       dcArgDouble     (DCCallVM* vm, DCdouble   value);
 DC_API void       dcArgPointer    (DCCallVM* vm, DCpointer  value);
 DC_API void       dcArgStruct     (DCCallVM* vm, DCstruct* s, DCpointer value);

 DC_API void       dcCallVoid      (DCCallVM* vm, DCpointer funcptr);
DC_API DCbool     dcCallBool      (DCCallVM* vm, DCpointer funcptr);
DC_API DCchar     dcCallChar      (DCCallVM* vm, DCpointer funcptr);
DC_API DCshort    dcCallShort     (DCCallVM* vm, DCpointer funcptr);
 DC_API DCint      dcCallInt       (DCCallVM* vm, DCpointer funcptr);
DC_API DClong     dcCallLong      (DCCallVM* vm, DCpointer funcptr);
DC_API DClonglong dcCallLongLong  (DCCallVM* vm, DCpointer funcptr);
DC_API DCfloat    dcCallFloat     (DCCallVM* vm, DCpointer funcptr);
 DC_API DCdouble   dcCallDouble    (DCCallVM* vm, DCpointer funcptr);
 DC_API DCpointer  dcCallPointer   (DCCallVM* vm, DCpointer funcptr);
 DC_API void       dcCallStruct    (DCCallVM* vm, DCpointer funcptr, DCstruct* s, DCpointer returnValue);

DC_API DCint      dcGetError      (DCCallVM* vm);

#define DEFAULT_ALIGNMENT 0
 DC_API DCstruct*  dcNewStruct      (DCsize fieldCount, DCint alignment);
 DC_API void       dcStructField    (DCstruct* s, DCint type, DCint alignment, DCsize arrayLength);
DC_API void       dcSubStruct      (DCstruct* s, DCsize fieldCount, DCint alignment, DCsize arrayLength);  	
/* Each dcNewStruct or dcSubStruct call must be paired with a dcCloseStruct. */
 DC_API void       dcCloseStruct    (DCstruct* s);  	
DC_API DCsize     dcStructSize     (DCstruct* s);  	
DC_API DCsize     dcStructAlignment(DCstruct* s);  	
 DC_API void       dcFreeStruct     (DCstruct* s);

 DC_API DCstruct*  dcDefineStruct  (const char* signature);
*/

// library dynload

/* shared library loading and explicit symbol resolving */

dlLoadLibrary :: !ZString !*a -> (!DLLib,!*a)
dlLoadLibrary libpath env = code {
		ccall dlLoadLibrary "s:p:A"
	}

dlFreeLibrary :: !DLLib !*a -> *a
dlFreeLibrary lib env = snd (dlFreeLibrary lib env)
where
	dlFreeLibrary :: !DLLib !*a -> (!Int,!*a)
	dlFreeLibrary _ _ = code {
			ccall dlFreeLibrary "p:I:A"
		}

dlFindSymbol :: !DLLib !ZString !*a -> (!DCpointer,!*a)
dlFindSymbol lib symbol env = code {
		ccall dlFindSymbol "ps:p:A"
	}


dcbArgInt :: !Pointer !*a -> (!Int,!*a)
dcbArgInt _ _ = code {
		ccall dcbArgInt "p:I:A"
	}
dcbArgPointer :: !Pointer !*a -> (!Pointer,!*a)
dcbArgPointer _ _ = code {
		ccall dcbArgPointer "p:p:A"
	}
dcbNewCallback :: !ZString !Pointer !Int -> Int
dcbNewCallback _ _ _ = code {
		ccall dcbNewCallback "spI:p"
	}
	

/* symbol table enumeration - only for symbol lookup, not resolve */
/*
typedef struct DLSyms_ DLSyms;

DL_API DLSyms*     dlSymsInit   (const char* libPath);
DL_API void        dlSymsCleanup(DLSyms* pSyms);

DL_API int         dlSymsCount        (DLSyms* pSyms);
DL_API const char* dlSymsName         (DLSyms* pSyms, int index);
DL_API const char* dlSymsNameFromValue(DLSyms* pSyms, void* value); /* symbol must be loaded */
*/

// extern void * dlsym(void * __handle, const char * __symbol);
dlsym :: !Int !ZString -> Int
dlsym handle symbol = code {
		ccall dlsym "Is:p"
	}

