implementation module Clyde.textviewcontroller

import StdEnv
import StdDebug
import System._Posix
import System._Pointer
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Clyde.controls

FLT_MAX :: Real
FLT_MAX
	= 8888888888888.0 * 8888888888888.0 * 8888888888888.0 * 8888888888888.0
//	= 340282346638528859811704183484516925440.000000
/*
FLT_MAX = code {
		ccall FLT_MAX ":R"
	}
*/	
populateThirdWindow :: !Pointer !*World -> *World
populateThirdWindow self env
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreBuffered	// NSBackingStoreRetained

		rectT			= NSRectType
		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" rectT rect style backing False env

		env				= msgIP_V wind "setTitle:\0" (c2ns "My Third Window\0") env

		env				= createTextView self wind env

		env				= msgI_V wind "retain\0" env
		
//		(w,env)			= msgI_P wind "becomeFirstResponder\0" env
//		env				= msgIP_V wind "makeKeyAndOrderFront:\0" self env

//		env				= force (writeInt self 8 wind) env		// hmmm... we keep overwriting the app window pointer???
	= env


createTextView :: !Pointer !Pointer !*a -> *a
createTextView delegate window env
	// create scroller
	#!	//(bounds,env)	= getBounds container env
//		(cview,env)		= msgI_P window "contentView\0" env
//		(bounds,env)	= msgI_P cview "frame\0" env
		bounds			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		
		(scroll,env)	= msgC_P "NSScrollView\0" "alloc\0" env
		(scroll,env)	= msgIS_P scroll "initWithFrame:\0" NSRectType bounds env

		env				= msgII_V scroll "setBorderType:\0" NSNoBorder env
		env				= msgII_V scroll "setHasVerticalScroller:\0" YES env
		env				= msgII_V scroll "setHasHorizontalScroller:\0" YES env
//		env				= msgII_V scroll "setAutohidesScrollers:\0" NO env
//		env				= msgII_V scroll "setAutohidesScrollers:\0" YES env
		env				= msgII_V scroll "setAutoresizingMask:\0" (NSViewWidthSizable bitor NSViewHeightSizable) env

	// create textview

		(content,env)	= getBounds scroll env

		origin_x		= readReal8 content 0
		origin_y		= readReal8 content 8
		size_w			= readReal8 content 16
		size_h			= readReal8 content 24
//		env = trace_n ("contentO\t"+++toString origin_x+++"\t"+++toString origin_y) env
//		env = trace_n ("contentS\t"+++toString size_w+++"\t"+++toString size_h) env

		(textv,env)		= msgC_P "NSTextView\0" "alloc\0" env
		(textv,env)		= msgIS_P textv "initWithFrame:\0" NSRectType content env
		env				= msgIS_V textv "setMinSize:\0" NSSizeType (NSMakeSize 0.0 (sizeHeight content)) env
		env				= msgIS_V textv "setMaxSize:\0" NSSizeType (NSMakeSize FLT_MAX FLT_MAX) env
		env				= msgII_V textv "setVerticallyResizable:\0" YES env
		env				= msgII_V textv "setHorizontallyResizable:\0" YES env
		env				= msgII_V textv "setAutoresizingMask:\0" (NSViewWidthSizable bitor NSViewHeightSizable) env
		env				= msgIS_V textv "setTextContainerInset:\0" NSSizeType (NSMakeSize 5.0 5.0) env
		
//		env				= msgII_V textv "setUsesFindPanel:\0" YES env
		env				= msgII_V textv "setUsesFindBar:\0" YES env

		(tcont,env)		= msgI_P textv "textContainer\0" env
		env				= msgIS_V tcont "setContainerSize:\0" NSSizeType (NSMakeSize FLT_MAX FLT_MAX) env
//		env				= msgIS_V tcont "setContainerSize:\0" NSSizeType (NSMakeSize 2048.0 2048.0) env
		env				= msgII_V tcont "setWidthTracksTextView:\0" NO env

	// load some content...
		encodingPtr		= malloc 8
		errorHdl		= malloc 8	// do we also need to alloc the Ptr??
		path			= "/Users/dvanarkelmaccom/Documents/CleanLab/Clyde.icl\0"
		(string,env)	= msgCPPP_P "NSString\0" "stringWithContentsOfFile:usedEncoding:error:\0" (c2ns path) encodingPtr errorHdl env
		env				= msgIP_V textv "setString:\0" string env

// add app delegate to text storage for syntax colouring...
		(stor,env)		= msgI_P textv "textStorage\0" env
		env				= msgIP_V stor "setDelegate:\0" delegate env
/*		range			= malloc 16
		range			= writeInt range 0 0	//0
		range			= writeInt range 8 6	//(dec n)
		value			= commentColour
		env = trace_n ("NSForegroundColorAttributeName\t"+++toString NSForegroundColorAttributeName) env
//		(_,env)			= msgIPPS_P stor "addAttribute:value:range:\0" NSForegroundColorAttributeName value NSRangeType range env
		(_,env)			= msgIPPP_P stor "addAttribute:value:range:\0" NSForegroundColorAttributeName value range env
		range			= writeInt range 0 16	//0
		range			= writeInt range 8 6	//(dec n)
		(_,env)			= msgIPPP_P stor "addAttribute:value:range:\0" NSForegroundColorAttributeName value range env
*/
	// assemble the pieces
		env				= msgIP_V scroll "setDocumentView:\0" textv env

		env				= msgIP_V window "setContentView:\0" scroll env
//		env				= msgIP_V window "makeFirstResponder:\0" textv env
		(ret,env)		= msgIP_I window "makeFirstResponder:\0" textv env
//		env = trace_n ("makeFirstResponder returns: "+++toString ret) env

		env				= msgIP_V window "makeKeyAndOrderFront:\0" delegate env

		(cview,env)		= msgI_P window "contentView\0" env
//		env				= logView cview env
	= env

import Cocoa.dyncall
NSForegroundColorAttributeName =: dlsym (-2) "NSForegroundColorAttributeName\0"
commentColour	=: fst (msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" 0.0 0.0 (195.0/255.0) 1.0 newWorld)
NSRangeType :: DCStruct
NSRangeType =: makeNSRangeType
where
	makeNSRangeType :: Int
	makeNSRangeType
		#!	(rectT,env)		= dcNewStruct 2 16 newWorld
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
//		#!	env = trace_n ("NSRangeType: "+++logDCStruct rectT) env
		= force env rectT


// setString:
//+ (nullable instancetype)stringWithContentsOfFile:(NSString *)path usedEncoding:(nullable NSStringEncoding *)enc error:(NSError **)error;

///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

