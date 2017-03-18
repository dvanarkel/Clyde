definition module Cocoa.msg

from System._Pointer	import ::Pointer
from Cocoa.dyncall		import ::ZString

//:: ZString :== String	// zero-terminated string

msgS_P :: !Pointer !Pointer !*a -> (!Pointer,!*a)
msgSP_I :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)

msgC_P :: !ZString !ZString !*a -> (!Pointer,!*a)
msgCP_P :: !ZString !ZString !Pointer !*a -> (!Pointer,!*a)
msgCV_P :: !ZString !ZString !{#Int} !*a -> (!Pointer,!*a)	// send Int array as NSArray of NSInt
msgCII_P :: !ZString !ZString !Int !Int !*a -> (!Pointer,!*a)
msgCPI_P :: !ZString !ZString !Pointer !Int !*a -> (!Pointer,!*a)
msgCPR_P :: !ZString !ZString !Pointer !Real !*a -> (!Pointer,!*a)
msgCRRR_P :: !ZString !ZString !Real !Real !Real !*a -> (!Pointer,!*a)
msgCRRRR_P :: !ZString !ZString !Real !Real !Real !Real !*a -> (!Pointer,!*a)
msgCPPP_P :: !ZString !ZString !Pointer !Pointer !Pointer !*a -> (!Pointer,!*a)

msgI_V :: !Pointer !ZString !*a -> *a
msgI_P :: !Pointer !ZString !*a -> (!Pointer,!*a)
msgI_I :: !Pointer !ZString !*a -> (!Int,!*a)
msgIP_P :: !Pointer !ZString !Pointer !*a -> (!Pointer,!*a)
msgIP_I :: !Pointer !ZString !Pointer !*a -> (!Int,!*a)
msgIPA_V :: !Pointer !ZString !Pointer !{#Int} !*a -> *a
msgIPA_P :: !Pointer !ZString !Pointer !{#Int} !*a -> (!Pointer,!*a)
msgIII_P :: !Pointer !ZString !Int !Int !*a -> (!Pointer,!*a)
msgIPP_P :: !Pointer !ZString !Pointer !Pointer !*a -> (!Pointer,!*a)
msgIPPPP_I :: !Pointer !ZString !Pointer !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
msgIPI_V :: !Pointer !ZString !Pointer !Int !*a -> *a
msgIPP_V :: !Pointer !ZString !Pointer !Pointer !*a -> *a
msgIPII_V :: !Pointer !ZString !Pointer !Int !Int !*a -> *a
msgIPPII_V :: !Pointer !ZString !Pointer !Pointer !Int !Int !*a -> *a
msgIPPP_P :: !Pointer !ZString !Pointer !Pointer !Pointer !*a -> (!Pointer,!*a)
msgII_P :: !Pointer !ZString !Int !*a -> (!Pointer,!*a)
msgII_V :: !Pointer !ZString !Int !*a -> *a
msgIP_V :: !Pointer !ZString !Pointer !*a -> *a
msgIR_V :: !Pointer !ZString !Real !*a -> *a
msgIRR_V :: !Pointer !ZString !Real !Real !*a -> *a
msgIPIIB_P :: !Pointer !ZString !Pointer !Int !Int !Bool !*a -> (!Pointer,!*a)

from Cocoa.dyncall import :: DCStruct, :: DCpointer
msgCS_V :: !ZString !ZString !DCStruct !Pointer !*a -> *a
// struct returns not supported by dyncall library
//msgIS_S :: !Pointer !ZString !DCStruct !Pointer !DCStruct !*a -> (!Pointer,*a)
//msgI_S :: !Pointer !ZString !DCStruct !*a -> (!Pointer,!*a)
msgIB_V :: !Pointer !ZString !Bool !*a -> *a
//msgII_S :: !Pointer !ZString !Int !DCStruct !*a -> (!Pointer,!*a)
msgIPS_P :: !Pointer !ZString !Pointer !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgIPPS_P :: !Pointer !ZString !Pointer !Pointer !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgIS_V :: !Pointer !ZString !DCStruct !Pointer !*a -> *a
msgIS_P :: !Pointer !ZString !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgISIIB_P :: !Pointer !ZString !DCStruct !DCpointer !Int !Int !Bool !*a -> (!Pointer,!*a)
