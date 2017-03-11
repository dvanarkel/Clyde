definition module Clyde.controls

from Cocoa.msg import :: Pointer, :: ZString
from Cocoa.Foundation import :: NSControl

:: NSBorderType	:== Int
NSNoBorder				:== 0
NSLineBorder			:== 1
NSBezelBorder			:== 2
NSGrooveBorder			:== 3

:: NSAutoresizingMaskOptions :== Int
NSViewNotSizable			:==  0
NSViewMinXMargin			:==  1
NSViewWidthSizable			:==  2
NSViewMaxXMargin			:==  4
NSViewMinYMargin			:==  8
NSViewHeightSizable			:== 16
NSViewMaxYMargin			:== 32

createButton :: !String !ZString !*a -> (!NSControl,!*a)
createCheckbox :: !String !ZString !*a -> (!NSControl,!*a)
createRadioButton :: !String !ZString !*a -> (!NSControl,!*a)
