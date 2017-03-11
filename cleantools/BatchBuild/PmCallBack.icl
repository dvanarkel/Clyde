implementation module PmCallBack

import StdBool

from IdeState import ::GeneralSt{..},::General

start :: !.a !(.Bool -> .(.a -> .(*GeneralSt -> *(.a,*GeneralSt)))) !*GeneralSt -> *GeneralSt
start ini_step step ps
	# (ls,ps) = step False ini_step ps
	| not ps.gst_continue_or_stop
		= start ls step ps
		= {ps & gst_continue_or_stop=False}

cont :: !*(!.a,!*GeneralSt) -> *(.a,!*GeneralSt);
cont (ls,ps)
	= (ls,ps)

stop :: !*(.a,!*GeneralSt) -> *(.a,!*GeneralSt);
stop (ls,ps)
	= (ls,{ps & gst_continue_or_stop=True})
