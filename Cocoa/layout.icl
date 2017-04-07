implementation module Cocoa.layout

import Cocoa.msg, Cocoa.objc
import System._Unsafe

instance NSLayoutAnchor NSLayoutDimension
where
	a2p (NSLayoutDimension p) = p

instance NSLayoutAnchor NSLayoutXAxisAnchor
where
	a2p (NSLayoutXAxisAnchor p) = p

instance NSLayoutAnchor NSLayoutYAxisAnchor
where
	a2p (NSLayoutYAxisAnchor p) = p

leadingAnchor		:: NSView -> NSLayoutXAxisAnchor
leadingAnchor (NSView v) = NSLayoutXAxisAnchor (accUnsafe (msgI_P v "layoutAnchor\0"))
trailingAnchor		:: NSView -> NSLayoutXAxisAnchor
trailingAnchor (NSView v) = NSLayoutXAxisAnchor (accUnsafe (msgI_P v "trailingAnchor\0"))
leftAnchor			:: NSView -> NSLayoutXAxisAnchor
leftAnchor (NSView v) = NSLayoutXAxisAnchor (accUnsafe (msgI_P v "leftAnchor\0"))
rightAnchor			:: NSView -> NSLayoutXAxisAnchor
rightAnchor (NSView v) = NSLayoutXAxisAnchor (accUnsafe (msgI_P v "rightAnchor\0"))
topAnchor			:: NSView -> NSLayoutYAxisAnchor
topAnchor (NSView v) = NSLayoutYAxisAnchor (accUnsafe (msgI_P v "topAnchor\0"))
bottomAnchor		:: NSView -> NSLayoutYAxisAnchor
bottomAnchor (NSView v) = NSLayoutYAxisAnchor (accUnsafe (msgI_P v "bottomAnchor\0"))
widthAnchor			:: NSView -> NSLayoutDimension
widthAnchor (NSView v) = NSLayoutDimension (accUnsafe (msgI_P v "widthAnchor\0"))
heightAnchor		:: NSView -> NSLayoutDimension
heightAnchor (NSView v) = NSLayoutDimension (accUnsafe (msgI_P v "heightAnchor\0"))
centerXAnchor		:: NSView -> NSLayoutXAxisAnchor
centerXAnchor (NSView v) = NSLayoutXAxisAnchor (accUnsafe (msgI_P v "centerXAnchor\0"))
centerYAnchor		:: NSView -> NSLayoutYAxisAnchor
centerYAnchor (NSView v) = NSLayoutYAxisAnchor (accUnsafe (msgI_P v "centerYAnchor\0"))
firstBaselineAnchor	:: NSView -> NSLayoutYAxisAnchor
firstBaselineAnchor (NSView v) = NSLayoutYAxisAnchor (accUnsafe (msgI_P v "firstBaselineAnchor\0"))
lastBaselineAnchor	:: NSView -> NSLayoutYAxisAnchor
lastBaselineAnchor (NSView v) = NSLayoutYAxisAnchor (accUnsafe (msgI_P v "lastBaselineAnchor\0"))

// These methods return an inactive constraint of the form thisAnchor = otherAnchor.
constraintEqualToAnchor					:: !a !a !*env -> (!NSLayoutConstraint,!*env) | NSLayoutAnchor a
constraintEqualToAnchor f t env
	#!	(ret,env)	= msgIP_P (a2p f) "constraintEqualToAnchor:\0" (a2p t) env
	= (NSLayoutConstraint ret,env)
constraintGreaterThanOrEqualToAnchor	:: a a -> NSLayoutConstraint | NSLayoutAnchor a
constraintGreaterThanOrEqualToAnchor f t = NSLayoutConstraint (accUnsafe (msgIP_P (a2p f) "constraintGreaterThanOrEqualToAnchor:\0" (a2p t)))
constraintLessThanOrEqualToAnchor		:: a a -> NSLayoutConstraint | NSLayoutAnchor a
constraintLessThanOrEqualToAnchor f t = NSLayoutConstraint (accUnsafe (msgIP_P (a2p f) "constraintLessThanOrEqualToAnchor:\0" (a2p t)))

// These methods return an inactive constraint of the form thisAnchor = otherAnchor + constant.
constraintEqualToAnchorConstant					:: a a Real *env -> (!NSLayoutConstraint,!*env) | NSLayoutAnchor a
constraintEqualToAnchorConstant f t c env
	#!	(ret,env)	= msgIPR_P (a2p f) "constraintEqualToAnchor:constant:\0" (a2p t) c env
	= (NSLayoutConstraint ret,env)
constraintGreaterThanOrEqualToAnchorConstant	:: a a Real -> NSLayoutConstraint | NSLayoutAnchor a
constraintGreaterThanOrEqualToAnchorConstant f t c = NSLayoutConstraint (accUnsafe (msgIPR_P (a2p f) "constraintGreaterThanOrEqualToAnchor:constant:\0" (a2p t) c))
constraintLessThanOrEqualToAnchorConstant		:: a a Real -> NSLayoutConstraint | NSLayoutAnchor a
constraintLessThanOrEqualToAnchorConstant f t c = NSLayoutConstraint (accUnsafe (msgIPR_P (a2p f) "constraintLessThanOrEqualToAnchor:constant:\0" (a2p t) c))

// These methods return an inactive constraint of the form thisVariable = constant.
constraintEqualToConstant				:: NSLayoutDimension Real *env -> (!NSLayoutConstraint,!*env)
constraintEqualToConstant (NSLayoutDimension d) c env
	#!	(ret,env)	= msgIR_P d "constraintEqualToConstant:\0" c env
	= (NSLayoutConstraint ret,env)
constraintGreaterThanOrEqualToConstant	:: NSLayoutDimension Real -> NSLayoutConstraint
constraintGreaterThanOrEqualToConstant (NSLayoutDimension d) c = NSLayoutConstraint (accUnsafe (msgIR_P d "constraintGreaterThanOrEqualToConstant:\0" c))
constraintLessThanOrEqualToConstant		:: NSLayoutDimension Real -> NSLayoutConstraint
constraintLessThanOrEqualToConstant (NSLayoutDimension d) c = NSLayoutConstraint (accUnsafe (msgIR_P d "constraintLessThanOrEqualToConstant:\0" c))

// These methods return an inactive constraint of the form thisAnchor = otherAnchor * multiplier.
constraintEqualToAnchorMultiplier				:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
constraintEqualToAnchorMultiplier f t c = NSLayoutConstraint (accUnsafe (msgIPR_P (a2p f) "constraintEqualToAnchor:multiplier:\0" (a2p t) c))
constraintGreaterThanOrEqualToAnchorMultiplier	:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
constraintGreaterThanOrEqualToAnchorMultiplier f t c = NSLayoutConstraint (accUnsafe (msgIPR_P (a2p f) "constraintGreaterThanOrEqualToAnchor:multiplier:\0" (a2p t) c))
constraintLessThanOrEqualToAnchorMultiplier		:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
constraintLessThanOrEqualToAnchorMultiplier f t c = NSLayoutConstraint (accUnsafe (msgIPR_P (a2p f) "constraintLessThanOrEqualToAnchor:multiplier:\0" (a2p t) c))

// These methods return an inactive constraint of the form thisAnchor = otherAnchor * multiplier + constant.
constraintEqualToAnchorMultiplierConstant				:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
constraintEqualToAnchorMultiplierConstant f t m c = NSLayoutConstraint (accUnsafe (msgIPRR_P (a2p f) "constraintEqualToAnchor:multiplier:constant:\0" (a2p t) m c))
constraintGreaterThanOrEqualToAnchorMultiplierConstant	:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
constraintGreaterThanOrEqualToAnchorMultiplierConstant f t m c = NSLayoutConstraint (accUnsafe (msgIPRR_P (a2p f) "constraintGreaterThanOrEqualToAnchor:multiplier:constant:\0" (a2p t) m c))
constraintLessThanOrEqualToAnchorMultiplierConstant		:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
constraintLessThanOrEqualToAnchorMultiplierConstant f t m c = NSLayoutConstraint (accUnsafe (msgIPRR_P (a2p f) "constraintLessThanOrEqualToAnchor:multiplier:constant:\0" (a2p t) m c))

activate :: !NSLayoutConstraint !*env -> *env
activate (NSLayoutConstraint lc) env
//	= msgII_V lc "setActivate:\0" YES env
//	#	(_,env)	= object_setInstanceVariable lc "active\0" YES env
	#	env	= msgII_V lc "setActive:\0" YES env
	= env
deactivate :: !NSLayoutConstraint !*env -> *env
deactivate (NSLayoutConstraint lc) env
	= msgII_V lc "setActive:\0" NO env

priority :: !NSLayoutConstraint !Real !*env -> *env
priority (NSLayoutConstraint lc) p env = msgIR_V lc "setPriority:\0" p env

translatesAutoresizingMaskIntoContraints :: !NSView !NSBool !*env -> *env	// set to NO to use manual autolayout
translatesAutoresizingMaskIntoContraints (NSView v) b env = msgII_V v "setTranslatesAutoresizingMaskIntoConstraints:\0" b env
