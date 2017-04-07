definition module Cocoa.layout

import StdMisc
import Cocoa.Foundation

class NSLayoutAnchor a 
where a2p :: a -> Int

instance NSLayoutAnchor NSLayoutDimension
instance NSLayoutAnchor NSLayoutXAxisAnchor
instance NSLayoutAnchor NSLayoutYAxisAnchor

:: NSLayoutDimension	=: NSLayoutDimension Int	//		width,height
:: NSLayoutXAxisAnchor	=: NSLayoutXAxisAnchor Int	//		horizontal layout
:: NSLayoutYAxisAnchor	=: NSLayoutYAxisAnchor Int	//		vertical layout

// attributes on NSView:
leadingAnchor		:: NSView -> NSLayoutXAxisAnchor
trailingAnchor		:: NSView -> NSLayoutXAxisAnchor
leftAnchor			:: NSView -> NSLayoutXAxisAnchor
rightAnchor			:: NSView -> NSLayoutXAxisAnchor
topAnchor			:: NSView -> NSLayoutYAxisAnchor
bottomAnchor		:: NSView -> NSLayoutYAxisAnchor
widthAnchor			:: NSView -> NSLayoutDimension
heightAnchor		:: NSView -> NSLayoutDimension
centerXAnchor		:: NSView -> NSLayoutXAxisAnchor
centerYAnchor		:: NSView -> NSLayoutYAxisAnchor
firstBaselineAnchor	:: NSView -> NSLayoutYAxisAnchor
lastBaselineAnchor	:: NSView -> NSLayoutYAxisAnchor

// These methods return an inactive constraint of the form thisAnchor = otherAnchor.
constraintEqualToAnchor					:: !a !a !*env -> (!NSLayoutConstraint,!*env) | NSLayoutAnchor a
constraintGreaterThanOrEqualToAnchor	:: a a -> NSLayoutConstraint | NSLayoutAnchor a
constraintLessThanOrEqualToAnchor		:: a a -> NSLayoutConstraint | NSLayoutAnchor a
//- (NSLayoutConstraint*)constraintEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor;
//- (NSLayoutConstraint*)constraintGreaterThanOrEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor;
//- (NSLayoutConstraint*)constraintLessThanOrEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor;

// These methods return an inactive constraint of the form thisAnchor = otherAnchor + constant.
constraintEqualToAnchorConstant					:: a a Real *env -> (!NSLayoutConstraint,!*env) | NSLayoutAnchor a
constraintGreaterThanOrEqualToAnchorConstant	:: a a Real -> NSLayoutConstraint | NSLayoutAnchor a
constraintLessThanOrEqualToAnchorConstant		:: a a Real -> NSLayoutConstraint | NSLayoutAnchor a
//- (NSLayoutConstraint*)constraintEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor constant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintGreaterThanOrEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor constant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintLessThanOrEqualToAnchor:(NSLayoutAnchor<AnchorType>*)anchor constant:(CGFloat)c;

// These methods return an inactive constraint of the form thisVariable = constant.
constraintEqualToConstant				:: NSLayoutDimension Real *env -> (!NSLayoutConstraint,!*env)
constraintGreaterThanOrEqualToConstant	:: NSLayoutDimension Real -> NSLayoutConstraint
constraintLessThanOrEqualToConstant		:: NSLayoutDimension Real -> NSLayoutConstraint
//- (NSLayoutConstraint*)constraintEqualToConstant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintGreaterThanOrEqualToConstant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintLessThanOrEqualToConstant:(CGFloat)c;

// These methods return an inactive constraint of the form thisAnchor = otherAnchor * multiplier.
constraintEqualToAnchorMultiplier				:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
constraintGreaterThanOrEqualToAnchorMultiplier	:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
constraintLessThanOrEqualToAnchorMultiplier		:: NSLayoutDimension NSLayoutDimension Real -> NSLayoutConstraint
//- (NSLayoutConstraint*)constraintEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m;
//- (NSLayoutConstraint*)constraintGreaterThanOrEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m;
//- (NSLayoutConstraint*)constraintLessThanOrEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m;

// These methods return an inactive constraint of the form thisAnchor = otherAnchor * multiplier + constant.
constraintEqualToAnchorMultiplierConstant				:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
constraintGreaterThanOrEqualToAnchorMultiplierConstant	:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
constraintLessThanOrEqualToAnchorMultiplierConstant		:: NSLayoutDimension NSLayoutDimension Real Real -> NSLayoutConstraint
//- (NSLayoutConstraint*)constraintEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m constant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintGreaterThanOrEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m constant:(CGFloat)c;
//- (NSLayoutConstraint*)constraintLessThanOrEqualToAnchor:(NSLayoutDimension*)anchor multiplier:(CGFloat)m constant:(CGFloat)c;

//////////

:: NSLayoutConstraint	=: NSLayoutConstraint Int

//constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant
//	:: NSView LayoutAttribute LayoutRelation NSView LayoutAttribute Real Real -> NSLayoutConstraint

activate	:: !NSLayoutConstraint !*env -> *env
deactivate	:: !NSLayoutConstraint !*env -> *env
priority	:: !NSLayoutConstraint !Real !*env -> *env

:: NSView	=: NSView Int
:: NSBool	:== Int

translatesAutoresizingMaskIntoContraints :: !NSView !NSBool !*env -> *env	// set to NO to use manual autolayout
