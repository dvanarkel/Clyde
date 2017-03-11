implementation module Clyde.windows

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

windBounds :: !Pointer !*World -> (!Pointer,!*World)
windBounds self env
	#!	(sel,env)		= sel_getUid "contentLayoutRect\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
	= (ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

populateWindow :: !Pointer !*World -> *World
populateWindow self env
	= env
/*
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
*/

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
		# 1	140556612321792
			NSTitlebarContainerView
			{x=0,y=460,w=1024,h=22}
			# subviews	1
			# 0	140556612329664
				NSTitlebarView
				{x=0,y=0,w=1024,h=22}
				# subviews	5
				# 0	140556612351568
					_NSThemeCloseWidget
					{x=7,y=3,w=14,h=16}
					# subviews	0
				# 1	140556610168960
					_NSThemeZoomWidget
					{x=47,y=3,w=14,h=16}
					# subviews	0
				# 2	140556610173056
					_NSThemeWidget
					{x=27,y=3,w=14,h=16}
					# subviews	0
				# 3	140556609187264
					NSTextField
					{x=459,y=3,w=124,h=17}
					# subviews	0
				# 4	140556610271200
					NSThemeDocumentButton
					{x=441,y=3,w=16,h=16}
					# subviews	0

NSView *themeFrame = [[self contentView] superview];
NSRect c = [themeFrame frame];  // c for "container"
NSRect aV = [closeButton frame];    // aV for "accessory view"
NSRect newFrame = NSMakeRect(                                                c.size.width - aV.size.width - 5,  // x position                                                c.size.height - aV.size.height - 5,    // y position                                           aV.size.width,  // width                                                aV.size.height); // height

[closeButton setFrame:newFrame];    
[themeFrame addSubview:closeButton];
[closeButton setAutoresizingMask:NSViewMaxXMargin | NSViewMinYMargin];  
[closeButton setEnabled:YES];
[closeButton setTarget:self];
[closeButton setAction:NSSelectorFromString(@"testClick:") ];*/
//+ (nullable NSButton *)standardWindowButton:(NSWindowButton)b forStyleMask:(NSUInteger)styleMask;
NSWindowDocumentVersionsButton = 6

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

		(bounds1,env)	= windBounds wind env
		origin_x		= readReal8 bounds1 0
		origin_y		= readReal8 bounds1 8
		size_w			= readReal8 bounds1 16
		size_h			= readReal8 bounds1 24

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

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

