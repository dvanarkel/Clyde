implementation module Clyde.controls

import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation

zeroRect	= cgRect 0.0 0.0 0.0 0.0

//---

createButton :: !String !ZString !*a -> (!NSControl,!*a)
createButton title action env
	#!	(but,env)		= allocObject "NSButton" env
		(but,env)		= initWithFrame but zeroRect env
		env				= setButtonType but NSMomentaryPushInButton env
		env				= setBezelStyle but NSRoundedBezelStyle env
		env				= setTitle but title env
		env				= sizeToFit but env
		env				= setAction but action env
		env				= setFrameOrigin but 10.0 10.0 env
	= (but,env)

createCheckbox :: !String !ZString !*a -> (!NSControl,!*a)
createCheckbox title action env
	#!	(but,env)		= allocObject "NSButton" env
		(but,env)		= initWithFrame but zeroRect env
		env				= setButtonType but NSSwitchButton env
//		env				= setBezelStyle but NSRegularSquareBezelStyle env	// ignored...
		env				= setTitle but title env
		env				= sizeToFit but env
		env				= setAction but action env
		env				= setFrameOrigin but 10.0 10.0 env
	= (but,env)

createRadioButton :: !String !ZString !*a -> (!NSControl,!*a)
createRadioButton title action env
	#!	(but,env)		= allocObject "NSButton" env
		(but,env)		= initWithFrame but zeroRect env
		env				= setButtonType but NSRadioButton env
		env				= setBezelStyle but NSRoundedBezelStyle env
		env				= setTitle but title env
		env				= sizeToFit but env
		env				= setAction but action env
		env				= setFrameOrigin but 10.0 10.0 env
	= (but,env)

:: NSButtonType	:== Int
NSMomentaryLightButton			= 0	// was NSMomentaryPushButton
NSPushOnPushOffButton			= 1
NSToggleButton					= 2
NSSwitchButton					= 3
NSRadioButton					= 4
NSMomentaryChangeButton			= 5
NSOnOffButton					= 6
NSMomentaryPushInButton			= 7	// was NSMomentaryLight
NSAcceleratorButton				= 8
NSMultiLevelAcceleratorButton	= 9

:: NSBezelStyle :== Int
NSRoundedBezelStyle          = 1
NSRegularSquareBezelStyle    = 2
NSThickSquareBezelStyle      = 3
NSThickerSquareBezelStyle    = 4
NSDisclosureBezelStyle       = 5
NSShadowlessSquareBezelStyle = 6
NSCircularBezelStyle         = 7
NSTexturedSquareBezelStyle   = 8
NSHelpButtonBezelStyle       = 9
NSSmallSquareBezelStyle       = 10
NSTexturedRoundedBezelStyle   = 11
NSRoundRectBezelStyle         = 12
NSRecessedBezelStyle          = 13
NSRoundedDisclosureBezelStyle = 14
/* The inline bezel style contains a solid round-rect border background. It can be used to create
   an "unread" indicator in an outline view, or another inline button in a tableview, such as a
   stop progress button in a download panel. Use text for an unread indicator, and a template image
   for other buttons.*/
NSInlineBezelStyle = 15		// NS_ENUM_AVAILABLE_MAC(10_7) 
