implementation module Clyde.windows

from Cocoa.objc import YES, NO
import Cocoa.msg

import code from "NSWindow+DvA.o"
//	gcc -c -ObjC -fobjc-arc -o Clean\ System\ Files/NSWindow+DvA.o NSWindow+DvA.m 

cascade :: !Pointer !*a -> *a
cascade wind env
	= code {
		ccall doCascade "p:V:A"
	}

////// Line Numbers
import code from "NoodleLineNumberMarker.o"
import code from "NoodleLineNumberView.o"
import code from "NSTextView+JSDExtensions.o"
//	gcc -c -ObjC -fobjc-arc -o Clean\ System\ Files/NoodleLineNumberMarker.o NoodleLineNumberMarker.m 
//	gcc -c -ObjC -fobjc-arc -o Clean\ System\ Files/NoodleLineNumberView.o NoodleLineNumberView.m 
//	gcc -c -ObjC -fobjc-arc -o Clean\ System\ Files/NSTextView+JSDExtensions.o NSTextView+JSDExtensions.m 

setShowsLineNumbers :: !Pointer !Bool !* a -> *a
setShowsLineNumbers textv show env
	= msgII_V textv "setShowsLineNumbers:" (if show YES NO) env			// from Noodle & NSTextView+JSDExtensions...

/*
import StdEnv
import StdDebug
import System._Pointer
import System._Posix
import Cocoa.msg
import Cocoa.objc
import Cocoa.Foundation
import Clyde.controls
import Clyde.tableviewcontroller
//import Clyde.outlineviewcontroller
//import Clyde.textviewcontroller
import Clyde.menus
import Clyde.textdocument

NSWindowDocumentVersionsButton = 6

contentLayoutRect :: !Pointer !*World -> (!Pointer,!*World)
contentLayoutRect self env
	#!	(sel,env)		= sel_getUid "contentLayoutRect\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

readRect :: !Pointer !*a -> (!(!Real,!Real),!(!Real,!Real),!*a)
readRect bounds env
	#!	origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
	= ((origin_x,origin_y),(size_w,size_h),env)

visibleFrame :: !Pointer !*a -> (!Pointer,!*a)
visibleFrame self env
	#!	(sel,env)		= sel_getUid "visibleFrame\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
//		env = trace_n ("getFrame\t"+++toString self+++"\t"+++toString sel+++"\t"+++toString ptr+++"\t"+++toString ptr`) env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

/*
cascadeTL	=: NSMakeSize 0.0 0.0
cascadeTL`	=: NSMakeSize 0.0 0.0
cascade wind env
	| trace_n ("<<<cascade "+++toString cascadeTL+++"\t"+++toString (readReal8 cascadeTL 0)+++"\t"+++toString (readReal8 cascadeTL 8)) False = undef
//	#!	(tl,env)		= msgIS_S wind "cascadeTopLeftFromPoint:\0" NSSizeType cascadeTL NSSizeType  env
	#!	(tl,env)		= call wind "cascadeTopLeftFromPoint:\0" NSSizeType cascadeTL NSSizeType  env
	| trace_n (">>>cascade "+++toString cascadeTL+++"\t"+++toString (readReal8 cascadeTL 0)+++"\t"+++toString (readReal8 cascadeTL 8)) False = undef
	| trace_n (">> c\t" +++ toString tl) False = undef
	| trace_n ("cascade\t"
				+++toString cascadeTL
				+++"\t"+++toString tl
				+++"\t"+++toString (readReal8 cascadeTL 0) 
				+++"\t"+++toString (readReal8 cascadeTL 8)
				+++"\t"+++toString (readReal8 tl 0)
				+++"\t"+++toString (readReal8 tl 8) 
			  ) False = undef
	= env
where
	call self seln _ ptr _ env
		#!	(sel,env)		= sel_getUid seln env
			ptr` = malloc 16
//			env	= call` ptr` self sel ptr env
			(ptr`,env)	= call` ptr` self sel ptr env
//		| ptr` <> cascadeTL` = abort "????"
//		#!	ptr	= writeReal8 ptr 0 (readReal8 ptr` 0)
//			ptr	= writeReal8 ptr 8 (readReal8 ptr` 8)
//			env = force ptr env
		= (ptr`, env)
//	call` :: !Int !Int !Int !Int !*a -> *a//(!Int,!*a)
	call` :: !Int !Int !Int !Int !*a -> (!Int,!*a)
	call` _ _ _ _ _ = code {
			ccall objc_msgSend_stret "Gpppp:I:A"
		}
*/		

cascadeTL	=: NSMakeSize 0.0 0.0

cascadeTopLeftFromPoint wind env
	#!	(sel,env)		= sel_getUid "cascadeTopLeftFromPoint:\0" env
		x				= readReal8 cascadeTL 0
		y				= readReal8 cascadeTL 8
		(x,y,env)		= maybeInit x y wind env
		(l,t,env)		= cascade` wind sel x y env
	| trace_n ("ctlfp\t"+++toString l+++"\t"+++toString t) False = undef
	#!	ptr				= cascadeTL
		ptr				= writeReal8 ptr 0 l
		ptr				= writeReal8 ptr 8 t
	= force ptr env

maybeInit 0.0 0.0 wind env
	#!	(screen,env)	= msgI_P wind "screen\0" env
		(vis,env)		= visibleFrame screen env
		(frm,env)		= getFrame wind env
		shgt			= readReal8 vis 24
		fhgt			= (readReal8 frm 24) - (readReal8 frm 8)
		top				= shgt - fhgt
	= (0.0,top,env)
maybeInit x y wind env
	= (x,y,env)
cascade` :: !Pointer !Pointer !Real !Real !*a -> (!Real,!Real,!*a)	// second 'R' not picked up...
cascade` _ _ _ _ _ = code {
		ccall objc_msgSend "GppRR:RR:A"
	} 


//////

/*
populateWindow :: !Pointer !*World -> *World
populateWindow self env
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained
		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env
		env				= msgIP_V wind "setTitle:\0" (c2ns "Project Window\0") env
		(view,env)		= msgC_P "NSView\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 400.0 400.0
//		(cont_,env)		= msgI_P wind "contentView:\0" env
//		(rect,env)		= getBounds cont_ env
		(view,env)		= msgIS_P view "initWithFrame:\0" rectT rect env
//		(flip,env)		= msgI_I view "isFlipped\0" env
//		env = trace_n ("root is flipped: "+++toString flip) env

		env				= msgIP_V wind "setContentView:\0" view env
		(vw,env)		= msgI_P wind "contentView\0" env
		env				= createPOView self view env		
		(w,env)			= msgI_P wind "becomeFirstResponder\0" env	
		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env
	= env


populateSecondWindow :: !Pointer !*World -> *World
populateSecondWindow self env
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained

		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env

		env				= msgIP_V wind "setTitle:\0" (c2ns "My Second Window\0") env

		(view,env)		= msgC_P "View\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 320.0 480.0
		(view,env)		= msgIS_P view "initWithFrame:\0" rectT rect env

		(but,env)		= createButton "My Button" "orderFrontStandardAboutPanel:\0" env
		env				= msgIP_V view "addSubview:\0" but env
		
		(but2,env)		= createButton "Default button" "orderFrontStandardAboutPanel:\0" env
		env				= msgIRR_V but2 "setFrameOrigin:" 100.0 10.0 env
		env				= msgIP_V but2 "setKeyEquivalent:\0" (p2ns "\r") env
		env				= msgIP_V view "addSubview:\0" but2 env
		
		(but3,env)		= createCheckbox "My checkbox" "orderFrontStandardAboutPanel:\0" env
		env				= msgIRR_V but3 "setFrameOrigin:" 400.0 10.0 env
		env				= msgIP_V view "addSubview:\0" but3 env
		
		(rbut1,env)		= createRadioButton "One" "orderFrontStandardAboutPanel:\0" env
		(rbut2,env)		= createRadioButton "Two" "orderFrontStandardAboutPanel:\0" env
		(rbut3,env)		= createRadioButton "Three" "orderFrontStandardAboutPanel:\0" env
		env				= msgIRR_V rbut1 "setFrameOrigin:" 10.0 150.0 env
		env				= msgIRR_V rbut2 "setFrameOrigin:" 10.0 130.0 env
		env				= msgIRR_V rbut3 "setFrameOrigin:" 10.0 110.0 env
		env				= msgIP_V view "addSubview:\0" rbut1 env
		env				= msgIP_V view "addSubview:\0" rbut2 env
		env				= msgIP_V view "addSubview:\0" rbut3 env

		env				= msgIP_V wind "setContentView:\0" view env
		(w,env)			= msgI_P wind "becomeFirstResponder\0" env
///...
/*
		(url,env)		= msgCP_P "NSURL\0" "fileURLWithPath:\0" (p2ns "/Users/dvanarkelmaccom/Documents/CleanLab/icfp2015/hex.icl") env
		env				= msgIP_V wind "setRepresentedURL:\0" url env

		(vbut,env)		= msgCII_P "NSWindow\0" "standardWindowButton:forStyleMask:\0" NSWindowDocumentVersionsButton 0 env
		(titb,env)		= msgI_P view "superview\0" env
		(tcont,env)		= findView "NSTitlebarContainerView" titb env
		(ttitb,env)		= findView "NSTitlebarView" tcont env
		(ttext,env)		= findView "NSTextField" ttitb env
		(bounds,env)	= getBounds ttext env
		origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
//		env				= msgIRR_V vbut "setFrameOrigin:" (origin_x + size_w) origin_y env
		env				= msgIRR_V vbut "setFrameOrigin:" (459.0 + 124.0) 3.0 env
// fiddling with frame...
//		env				= msgIP_V titb "_addKnownSubview:\0" vbut env
		env				= msgIP_V ttitb "addSubview:\0" vbut env

		(act,env)		= msgI_P vbut "action\0" env
		env = trace_n ("action\t"+++toString act +++"\t"+++ sel_getName act) env
*/
//		env				= msgII_V wind "_setShowAutosaveButton:\0" 1 env
//...	
//		env				= force (writeInt self 8 wind) env
		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env
	= env
*/
findView name view env
	#!	(subs,env)		= msgI_P view "subviews\0" env
		(count,env)		= msgI_I subs "count\0" env
	= logsubs 0 count subs env
where
	logsubs :: !Int !Int !Pointer !*a -> (!Pointer,!*a)
	logsubs index count subs env
		| index >= count
			= (0,env)
		#!	(view,env)		= msgII_P subs "objectAtIndex:\0" index env
		| object_getClassName view == name
			= (view,env)
//		#!	env				= logviews (inc nest) view env
		= logsubs (inc index) count subs env

/*

NSView *themeFrame = [[self contentView] superview];
NSRect c = [themeFrame frame];  // c for "container"
NSRect aV = [closeButton frame];    // aV for "accessory view"
NSRect newFrame = NSMakeRect(                                                c.size.width - aV.size.width - 5,  // x position                                                c.size.height - aV.size.height - 5,    // y position                                           aV.size.width,  // width                                                aV.size.height); // height

[closeButton setFrame:newFrame];    
[themeFrame addSubview:closeButton];
[closeButton setAutoresizingMask:NSViewMaxXMargin | NSViewMinYMargin];  
[closeButton setEnabled:YES];
[closeButton setTarget:self];
[closeButton setAction:NSSelectorFromString(@"testClick:") ];

//+ (nullable NSButton *)standardWindowButton:(NSWindowButton)b forStyleMask:(NSUInteger)styleMask;
*/

/*
populateThirdWindow :: !Pointer !*World -> *World
populateThirdWindow self env
	= populateSecondWindow self env

populateFourthWindow :: !Pointer !*World -> *World
populateFourthWindow self env
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained
		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env

		(bounds1,env)	= contentLayoutRect wind env

		env				= msgIP_V wind "setTitle:\0" (c2ns "Profile Window\0") env

		(view,env)		= msgC_P "NSView\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 400.0 400.0
//		(cont_,env)		= msgI_P wind "contentView:\0" env
//		(rect,env)		= getBounds cont_ env
		(view,env)		= msgIS_P view "initWithFrame:\0" rectT rect env

		env				= msgIP_V wind "setContentView:\0" view env
		(vw,env)		= msgI_P wind "contentView\0" env
		(bounds1,env)	= getBounds vw env
		env = trace_n ("root "+++toString wind+++"\t"+++toString bounds1) env

		env				= createPLView self view env
		
		(w,env)			= msgI_P wind "becomeFirstResponder\0" env
	
		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env
	= env
*/
///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

*/