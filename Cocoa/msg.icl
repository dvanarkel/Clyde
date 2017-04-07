implementation module Cocoa.msg

import StdEnv
import StdDebug
import System._Pointer
import Cocoa.objc
import Cocoa.dyncall

msgS_V :: !Pointer !Pointer !*a -> *a
msgS_V cls sel env
	#!	(vm,env)		= dcNewCallVM 4096 env
//		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcCallVoid vm objc_msgSendSuper2Ptr env
		env				= dcFree vm env
	= env

msgS_P :: !Pointer !Pointer !*a -> (!Pointer,!*a)
msgS_P cls sel env
	#!	(vm,env)		= dcNewCallVM 4096 env
//		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		(ret,env)		= dcCallPointer vm objc_msgSendSuper2Ptr env
		env				= dcFree vm env
	= (ret,env)

msgSP_I :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
msgSP_I cls sel arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
//		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg env
		(ret,env)		= dcCallInt vm objc_msgSendSuperPtr env
		env				= dcFree vm env
	= (ret,env)

objc_msgSendSuperPtr :: Pointer
objc_msgSendSuperPtr =: dlsym -2 "objc_msgSendSuper\0"

objc_msgSendSuper2Ptr :: Pointer
objc_msgSendSuper2Ptr =: dlsym -2 "objc_msgSendSuper2\0"

objc_msgSendSuper :: !Int !Int -> Int
objc_msgSendSuper _ _ = code {
		ccall objc_msgSendSuper "pp:p"
	}

msgC_P :: !ZString !ZString !*a -> (!Pointer,!*a)
msgC_P clsS selS env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCV_P :: !ZString !ZString !{#Int} !*a -> (!Pointer,!*a)
msgCV_P clsS selS arr env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
//		env				= seq (map (dcArgPointer vm) arr) env
		env				= seqA (dcArgPointer vm) arr env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

seqA :: !(!Int !*a -> *a) !{#Int} !*a -> *a
seqA fun arr env
	#!	limit	= size arr
	= seqA` 0 limit fun arr env
where
	seqA` :: !Int !Int !(!Int !*a -> *a) !{#Int} !*a -> *a
	seqA` idx lim fun arr env
		| idx >= lim
			= env
		#!	elm	= arr.[idx]
//			(num,env)	= msgCI_P "NSNumber\0" "numberWithInt:\0" elm env
			(num,env)	= msgCP_P "NSNumber\0" "numberWithInt:\0" elm env
			env	= fun num env
		= seqA` (inc idx) lim fun arr env

msgCP_P :: !ZString !ZString !Pointer !*a -> (!Pointer,!*a)
msgCP_P clsS selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCII_P :: !ZString !ZString !Int !Int !*a -> (!Pointer,!*a)
msgCII_P clsS selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg1 env
		env				= dcArgInt vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCPI_P :: !ZString !ZString !Pointer !Int !*a -> (!Pointer,!*a)
msgCPI_P clsS selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgInt vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCPR_P :: !ZString !ZString !Pointer !Real !*a -> (!Pointer,!*a)
msgCPR_P clsS selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgDouble vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCRRR_P :: !ZString !ZString !Real !Real !Real !*a -> (!Pointer,!*a)
msgCRRR_P clsS selS arg1 arg2 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg1 env
		env				= dcArgDouble vm arg2 env
		env				= dcArgDouble vm arg3 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCRRRR_P :: !ZString !ZString !Real !Real !Real !Real !*a -> (!Pointer,!*a)
msgCRRRR_P clsS selS arg1 arg2 arg3 arg4 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg1 env
		env				= dcArgDouble vm arg2 env
		env				= dcArgDouble vm arg3 env
		env				= dcArgDouble vm arg4 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCPPP_P :: !ZString !ZString !Pointer !Pointer !Pointer !*a -> (!Pointer,!*a)
msgCPPP_P clsS selS arg1 arg2 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcArgPointer vm arg3 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgCS_V :: !ZString !ZString !DCStruct !Pointer !*a -> *a
msgCS_V clsS selS argt argv env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(cls,env)		= objc_getClass clsS env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env

//		env				= dcArgStruct vm argt argv env
		env				= dcArgDouble vm (readReal8 argv 0) env
		env				= dcArgDouble vm (readReal8 argv 8) env
		env				= dcArgDouble vm (readReal8 argv 16) env
		env				= dcArgDouble vm (readReal8 argv 24) env

		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env


msgI_V :: !Pointer !ZString !*a -> *a
msgI_V cls selS env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIS_S :: !Pointer !ZString !DCStruct !Pointer !DCStruct !*a -> (!Pointer,*a)
msgIS_S cls selS argt argv rett env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgStruct vm argt argv env

		(ret,env)		= dcCallStruct vm objc_msgSend_stretPtr rett env
		env				= dcFree vm env
	= (ret,env)

msgI_S :: !Pointer !ZString !DCStruct !*a -> (!Pointer,!*a)
msgI_S cls selS s env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		(ret,env)		= dcCallStruct vm objc_msgSend_stretPtr s env
		env				= dcFree vm env
	= (ret,env)

msgIB_V :: !Pointer !ZString !Bool !*a -> *a
msgIB_V cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgBool vm arg env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgII_S :: !Pointer !ZString !Int !DCStruct !*a -> (!Pointer,!*a)
msgII_S cls selS arg s env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg env
		(ret,env)		= dcCallStruct vm objc_msgSend_stretPtr s env
		env				= dcFree vm env
	= (ret,env)

msgI_P :: !Pointer !ZString !*a -> (!Pointer,!*a)
msgI_P cls selS env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgI_I :: !Pointer !ZString !*a -> (!Int,!*a)
msgI_I cls selS env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		(ret,env)		= dcCallInt vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIP_V :: !Pointer !ZString !Pointer !*a -> *a
msgIP_V cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIP_P :: !Pointer !ZString !Pointer !*a -> (!Pointer,!*a)
msgIP_P cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIR_P :: !Pointer !ZString !Real !*a -> (!Pointer,!*a)
msgIR_P cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIP_I :: !Pointer !ZString !Pointer !*a -> (!Int,!*a)
msgIP_I cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg env
		(ret,env)		= dcCallInt vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIII_V :: !Pointer !ZString !Int !Int !*a -> *a
msgIII_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg1 env
		env				= dcArgInt vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIII_P :: !Pointer !ZString !Int !Int !*a -> (!Pointer,!*a)
msgIII_P cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg1 env
		env				= dcArgInt vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPP_P :: !Pointer !ZString !Pointer !Pointer !*a -> (!Pointer,!*a)
msgIPP_P cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPS_P :: !Pointer !ZString !Pointer !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgIPS_P cls selS arg1 argt2 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgStruct vm argt2 arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPA_V :: !Pointer !ZString !Pointer !{#Int} !*a -> *a
msgIPA_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgArrayPointer vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPA_P :: !Pointer !ZString !Pointer !{#Int} !*a -> (!Pointer,!*a)
msgIPA_P cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgArrayPointer vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPPS_P :: !Pointer !ZString !Pointer !Pointer !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgIPPS_P cls selS arg1 arg2 argt3 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcArgStruct vm argt3 arg3 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPPPP_I :: !Pointer !ZString !Pointer !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
msgIPPPP_I cls selS arg1 arg2 arg3 arg4 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcArgPointer vm arg3 env
		env				= dcArgPointer vm arg4 env
		(ret,env)		= dcCallInt vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPI_V :: !Pointer !ZString !Pointer !Int !*a -> *a
msgIPI_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgInt vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPP_V :: !Pointer !ZString !Pointer !Pointer !*a -> *a
msgIPP_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPR_V :: !Pointer !ZString !Pointer !Real !*a -> *a
msgIPR_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgDouble vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPR_P :: !Pointer !ZString !Pointer !Real !*a -> (!Pointer,!*a)
msgIPR_P cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgDouble vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPRR_P :: !Pointer !ZString !Pointer !Real !Real !*a -> (!Pointer,!*a)
msgIPRR_P cls selS arg1 arg2 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgDouble vm arg2 env
		env				= dcArgDouble vm arg3 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPII_V :: !Pointer !ZString !Pointer !Int !Int !*a -> *a
msgIPII_V cls selS arg1 arg2 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgInt vm arg2 env
		env				= dcArgInt vm arg3 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPPII_V :: !Pointer !ZString !Pointer !Pointer !Int !Int !*a -> *a
msgIPPII_V cls selS arg1 arg2 arg3 arg4 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcArgInt vm arg3 env
		env				= dcArgInt vm arg4 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIPPP_P :: !Pointer !ZString !Pointer !Pointer !Pointer !*a -> (!Pointer,!*a)
msgIPPP_P cls selS arg1 arg2 arg3 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgPointer vm arg2 env
		env				= dcArgPointer vm arg3 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIS_V :: !Pointer !ZString !DCStruct !Pointer !*a -> *a
msgIS_V cls selS argt argv env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgStruct vm argt argv env

		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIS_P :: !Pointer !ZString !DCStruct !Pointer !*a -> (!Pointer,!*a)
msgIS_P cls selS argt argv env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env

		env				= dcArgStruct vm argt argv env
//		env				= dcArgDouble vm (readReal8 argv 0) env
//		env				= dcArgDouble vm (readReal8 argv 8) env
//		env				= dcArgDouble vm (readReal8 argv 16) env
//		env				= dcArgDouble vm (readReal8 argv 24) env

		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgII_P :: !Pointer !ZString !Int !*a -> (!Pointer,!*a)
msgII_P cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgII_V :: !Pointer !ZString !Int !*a -> *a
msgII_V cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgInt vm arg env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIR_V :: !Pointer !ZString !Real !*a -> *a
msgIR_V cls selS arg env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIRR_V :: !Pointer !ZString !Real !Real !*a -> *a
msgIRR_V cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg1 env
		env				= dcArgDouble vm arg2 env
		env				= dcCallVoid vm objc_msgSendPtr env
		env				= dcFree vm env
	= env

msgIRR_P :: !Pointer !ZString !Real !Real !*a -> (!Pointer,!*a)
msgIRR_P cls selS arg1 arg2 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgDouble vm arg1 env
		env				= dcArgDouble vm arg2 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgIPIIB_P :: !Pointer !ZString !Pointer !Int !Int !Bool !*a -> (!Pointer,!*a)
msgIPIIB_P cls selS arg1 arg2 arg3 arg4 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgPointer vm arg1 env
		env				= dcArgInt vm arg2 env
		env				= dcArgInt vm arg3 env
		env				= dcArgBool vm arg4 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

msgISIIB_P :: !Pointer !ZString !DCStruct !DCpointer !Int !Int !Bool !*a -> (!Pointer,!*a)
msgISIIB_P cls selS arg1t arg1v arg2 arg3 arg4 env
	#!	(vm,env)		= dcNewCallVM 4096 env
		(sel,env)		= sel_getUid selS env
		env				= dcReset vm env
		env				= dcArgPointer vm cls env
		env				= dcArgPointer vm sel env
		env				= dcArgStruct vm arg1t arg1v env
		env				= dcArgInt vm arg2 env
		env				= dcArgInt vm arg3 env
		env				= dcArgBool vm arg4 env
		(ret,env)		= dcCallPointer vm objc_msgSendPtr env
		env				= dcFree vm env
	= (ret,env)

