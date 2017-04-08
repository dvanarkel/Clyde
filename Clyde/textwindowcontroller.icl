implementation module Clyde.textwindowcontroller

import StdEnv
import StdDebug
import Cocoa.msg
import Cocoa.objc
import Cocoa.Foundation
import Clyde.controls
import Clyde.menus
import Clyde.windows
from Clyde.textdocument import setMyParagraphStyle

populateTextWindow :: !Pointer !String !*World -> (!Pointer,!*World)
populateTextWindow self type env
	| trace_n ("populateTextWindow of type '"+++type+++"'") False = undef
	#!	(wind,env)		= msgC_P "NSWindow\0" "alloc\0" env
		rect			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		style			= NSTitledWindowMask + NSClosableWindowMask + NSResizableWindowMask + NSMiniaturizableWindowMask
		backing			= NSBackingStoreRetained

		(wind,env)		= msgISIIB_P wind "initWithContentRect:styleMask:backing:defer:\0" NSRectType rect style backing True env
		env				= cascade wind env

		env				= msgIP_V wind "setTitle:\0" (c2ns "My Third Window\0") env

	// create scroller
	#!	(r,g,b,a)		= colour
		//(bounds,env)	= getBounds container env
		bounds			= cgRect 0.0 0.0 1024.0 460.0			// TODO: need to free...
		
		(scroll,env)	= msgC_P "NSScrollView\0" "alloc\0" env
		(scroll,env)	= msgIS_P scroll "initWithFrame:\0" NSRectType bounds env

		env				= msgII_V scroll "setBorderType:\0" NSNoBorder env
		env				= msgII_V scroll "setHasVerticalScroller:\0" YES env
		env				= msgII_V scroll "setHasHorizontalScroller:\0" YES env
		env				= msgII_V scroll "setAutoresizingMask:\0" (NSViewWidthSizable bitor NSViewHeightSizable) env

	// create textview

		(content,env)	= getBounds scroll env

		(textv,env)		= msgC_P "NSTextView\0" "alloc\0" env
		(textv,env)		= msgIS_P textv "initWithFrame:\0" NSRectType content env
		env				= msgIS_V textv "setMinSize:\0" NSSizeType (NSMakeSize 0.0 (sizeHeight content)) env
		env				= msgIS_V textv "setMaxSize:\0" NSSizeType (NSMakeSize FLT_MAX FLT_MAX) env
		env				= msgII_V textv "setVerticallyResizable:\0" YES env
		env				= msgII_V textv "setHorizontallyResizable:\0" YES env
		env				= msgII_V textv "setAutoresizingMask:\0" (NSViewWidthSizable bitor NSViewHeightSizable) env
		env				= msgIS_V textv "setTextContainerInset:\0" NSSizeType (NSMakeSize 5.0 5.0) env
		env				= msgII_V textv "setUsesFindBar:\0" YES env
		env				= msgII_V textv "setAllowsUndo:\0" YES env
		env				= msgII_V textv "setIncrementalSearchingEnabled:\0" YES env
		env				= msgII_V textv "setAutomaticDashSubstitutionEnabled:\0" NO env
		env				= msgII_V textv "setAutomaticQuoteSubstitutionEnabled:\0" NO env
		env				= msgII_V textv "setAutomaticTextReplacementEnabled:\0" NO env
		env				= msgII_V textv "setAutomaticSpellingCorrectionEnabled:\0" NO env
		env				= msgII_V textv "setContinuousSpellCheckingEnabled:\0" NO env
		env				= msgII_V textv "setEnabledTextCheckingTypes:\0" 0 env
		
		// NOTE: this is a shared menu between all text windows so take care to only patch once! 
		(menu,env)		= msgI_P textv "menu\0" env
		(nitem,env)		= msgI_I menu "numberOfItems\0" env
		env = trace_n ("context menu items #: "+++toString nitem) env
		(mitem,env)		= itemAtIndex menu (dec nitem) env
		env = trace_n ("myItem menu item: "+++toString mitem) env
		(title,env)		= msgI_P mitem "title\0" env
		env = trace_n ("myItem menu title: "+++(if (title==0) "<null>" (ns2cls title))) env
//		(action,env)	= msgI_P mitem "action\0" env
//		env = trace_n ("myItem menu item: '"+++ns2cls title+++"'\t'"+++ns2cls action+++"'") env
		
/*
		env				= addSeparator menu env
		(mitem,env)		= itemWithTag menu (p2ns "myTag:") env
		env = trace_n ("myItem menu item: "+++toString mitem) env
		title			= "Jump to..."
		(mitem,env)		= itemWithTitle menu "myItem" env
		env = trace_n ("myItem menu item: "+++toString mitem) env
		(mitem,env)		= case mitem of
							0	-> case (ns2cls title) of
								"myItem"	-> (mitem,env)
								_			-> addItemWith_title_action_keyEquivalent menu "myItem" "myAction:" "" env
							_	-> (mitem,env)
		// insertItemWith_title
		// implement validateMenuItem: to add context sensitive enable/disable
*/

		env			= removeItemWithTitle menu "Font" env
		env			= removeItemWithTitle menu "Substitutions" env
		env			= removeItemWithTitle menu "Spelling and Grammar" env
		env			= removeItemWithTitle menu "Layout Orientation" env
/*
		todisable	= "Layout Orientation"
		env = trace_n ("look at disabling "+++todisable+++" in menu "+++toString menu) env
		(mitem,env)		= itemWithTitle menu todisable env
		env = trace_n ("disable "+++toString mitem) env
		env				= case mitem of
							0	-> env
							_	-> removeItem menu mitem env
		env	= trace_n ("after disable") env
*/
		
// [myNSTextView setFont:[NSFont fontWithName:@"Menlo" size:11]]
		(font,env)		= msgCPR_P "NSFont\0" "fontWithName:size:\0" (p2ns "Andale Mono") 14.0 env
		env				= msgIP_V textv "setFont:\0" font env
		env				= msgII_V textv "turnOffLigatures:\0" YES env
		env				= msgII_V textv "setDrawsBackground:\0" YES env
//		(col,env)		= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" 1.0 1.0 (215.0/255.0) 1.0 env
		(col,env)		= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" r g b a env
		env				= msgI_V col "retain\0" env
		env				= msgIP_V textv "setBackgroundColor:\0" col env

		env				= setMyParagraphStyle textv font env
		
		(tcont,env)		= msgI_P textv "textContainer\0" env
		env				= msgIS_V tcont "setContainerSize:\0" NSSizeType (NSMakeSize FLT_MAX FLT_MAX) env
		env				= msgII_V tcont "setWidthTracksTextView:\0" NO env
// add app delegate to text storage for syntax colouring...
		(stor,env)		= msgI_P textv "textStorage\0" env
		env				= msgIP_V stor "setDelegate:\0" self env
	// assemble the pieces
		env				= msgIP_V scroll "setDocumentView:\0" textv env
		env				= msgIP_V wind "setContentView:\0" scroll env
		(cview,env)		= msgI_P wind "contentView\0" env
		env				= setShowsLineNumbers textv True env
	| trace_n ("created textview '"+++toString textv+++"'") False = undef
//		env				= msgI_V wind "retain\0" env
	= (wind,env)
where
	colour	= case type of
				"Clean implementation module"	-> (1.0, 1.0, v, 1.0)
				"nl.ru.clean.implementation"	-> (1.0, 1.0, v, 1.0)
				"Clean definition module"		-> (1.0, v, v, 1.0)
				"nl.ru.clean.definition"		-> (1.0, v, v, 1.0)
				_								-> (1.0, 1.0, 1.0, 1.0)
	v		= 215.0/255.0
	
FLT_MAX			= 8888888888888.0 * 8888888888888.0 * 8888888888888.0 * 8888888888888.0
