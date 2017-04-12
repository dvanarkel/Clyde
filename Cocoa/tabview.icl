implementation module Cocoa.tabview

import System._Posix
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Cocoa.viewcontroller

// view controller & delegate...???


// general Foundation

:: NSView =: NSView Int

initWithIdentifier :: !Pointer !String !*env -> (!Pointer,!*env)
initWithIdentifier obj id env
	= msgIP_P obj "initWithIdentifier:\0" (p2ns id) env

setLabel :: !Pointer !String !*env -> *env
setLabel obj label env
	= msgIP_V obj "setLabel:\0" (p2ns label) env

setView :: !Pointer !NSView !*env -> *env
setView obj (NSView view) env
	= msgIP_V obj "setView:\0" view env

// 

:: NSTabView				=: NSTabView Int
:: NSTabViewItem			=: NSTabViewItem Int
:: NSTabViewController		=: NSTabViewController Int

createTabv :: !*env -> (!NSTabView,!*env)
createTabv env
	#	(tabv,env)	= allocObject "NSTabView" env
		(tabv,env)	= initObject tabv env
		env			= msgII_V tabv "setTranslatesAutoresizingMaskIntoConstraints:\0" NO env
	= (NSTabView tabv,env)

addTab :: !NSView !NSTabView !*env -> *env	// or add viewcontroller?
addTab view tabv env
	#	(tabi,env)	= allocObject "NSTabViewItem" env
		(tabi,env)	= initWithIdentifier tabi "tab1" env
		env			= setLabel tabi "TabL" env
		env			= addTabViewItem tabv (NSTabViewItem tabi) env
		env			= setView tabi view env
	= env

class addTabViewItem a where addTabViewItem :: !a !NSTabViewItem !*env -> *env
class tabViewItems a where tabViewItems :: !a !*env -> (!Int,!*env)
class removeTabViewItem a where removeTabViewItem :: !a !NSTabViewItem !*env -> *env
class insertTabViewItemAtIndex a where insertTabViewItemAtIndex :: !a !NSTabViewItem !Int !*env -> *env
class tabView a where tabView :: !a !*env -> (!NSTabView,!*env)

// `raw` NSTabView

instance addTabViewItem NSTabView where
	addTabViewItem (NSTabView tabv) (NSTabViewItem tabi) env	// adds at end
		= msgIP_V tabv "addTabViewItem:\0" tabi env

instance insertTabViewItemAtIndex NSTabView where
	insertTabViewItemAtIndex (NSTabView tabv) (NSTabViewItem tabi) idx env
		= msgIPI_V tabv "addTabViewItem:atIndex:\0" tabi idx env

instance removeTabViewItem NSTabView where
	removeTabViewItem :: !NSTabView !NSTabViewItem !*env -> *env
	removeTabViewItem (NSTabView tabv) (NSTabViewItem tabi) env
		= msgIP_V tabv "removeTabViewItem:\0" tabi env

indexOfTabViewItem tabv tabi env
	= msgIP_I tabv "indexOfTabViewItem:\0" tabi env

indexOfTabViewItemWithIdentifier tabv id env
	= msgIP_I tabv "indexOfTabViewItemWithIdentifier:\0" (p2ns id) env

numberOfTabViewItems tabv env
	= msgI_I tabv "numberOfTabViewItems\0" env

tabViewItemAtIndex tabv idx env
	= msgII_P tabv "tabViewItemAtIndex:\0" idx env

instance tabViewItems NSTabView where
	tabViewItems (NSTabView tabv) env	// returns NSArray (copy) of tab view items
		= msgI_P tabv "tabViewItems\0" env

selectFirstTabViewItem tabv sender env		// This action method selects the first tab view item.
	= msgIP_V tabv "selectFirstTabViewItem​​​​:\0" sender env

selectLastTabViewItem tabv sender env		// This action method selects the last tab view item.
	= msgIP_V tabv "selectLastTabViewItem:\0" sender env

selectNextTabViewItem tabv sender env		// This action method selects the next tab view item.
	= msgIP_V tabv "selectNextTabViewItem:\0" sender env

selectPreviousTabViewItem tabv sender env		// This action method selects the prev tab view item.
	= msgIP_V tabv "selectPreviousTabViewItem:\0" sender env

selectTabViewItem tabv tabi env
	= msgIP_V tabv "selectTabViewItem:\0" tabi env

selectTabViewItemAtIndex tabv idx env
	= msgII_V tabv "selectTabViewItemAtIndex:\0" idx env

selectTabViewItemWithIdentifier tabv id env
	= msgIP_V tabv "selectTabViewItemWithIdentifier:\0" (p2ns id) env

selectedTabViewItem tabv env
	= msgI_P tabv "selectedTabViewItem\0" env

takeSelectedTabViewItemFromSender tabv sender env	// sends 'indexOfSelectedItem:' to sender and selects tabview item with the returned index
	= msgIP_V tabv "takeSelectedTabViewItemFromSender:\0" sender env

font tabv env
	= msgI_P tabv "font\0" env

setFont tabv font env
	= msgIP_V tabv "setFont:\0" font env

tabViewType tabv env
	#	(t,env)	= msgI_I tabv "tabViewType\0" env
	= (NSTabViewType t, env)

setTabViewType tabv (NSTabViewType type) env
	= msgII_V tabv "setTabViewType:\0" type env

:: NSTabViewType =: NSTabViewType Int
NSTopTabsBezelBorder	= NSTabViewType 0
NSLeftTabsBezelBorder	= NSTabViewType 1
NSBottomTabsBezelBorder	= NSTabViewType 2
NSRightTabsBezelBorder	= NSTabViewType 3
NSNoTabsBezelBorder		= NSTabViewType 4
NSNoTabsLineBorder		= NSTabViewType 5
NSNoTabsNoBorder		= NSTabViewType 6

controlTint tabv env
	#	(t,env)	= msgI_I tabv "controlTint\0" env
	= (NSControlTint t,env)

setControlTint tabv (NSControlTint tint) env
	= msgII_V tabv "setControlTint:\0" tint env

:: NSControlTint =: NSControlTint Int
NSDefaultControlTint	= NSControlTint 0
NSBlueControlTint		= NSControlTint 1
NSGraphiteControlTint	= NSControlTint 6
NSClearControlTint		= NSControlTint 7

drawsBackground tabv env
	= msgI_I tabv "drawsBackground\0" env

setDrawsBackground tabv bool env
	= msgII_V tabv "setDrawsBackground:\0" bool env

//minimumSize tabv env
//	= msgI_? tabv "minimumSize\0" env	// returns NSSize

//contentRect tabv env
//	= msgI_? tabv "contentRect\0" env	// returns NSRect

:: NSRect =: NSRect Int
contentRect :: !Pointer !*a -> (!NSRect,!*a)
contentRect self env
	#!	(sel,env)		= sel_getUid "contentRect\0" env
		ptr				= malloc 32
		(ptr`,env)		= theCall ptr self sel env
	= (NSRect ptr,env)
where
	theCall :: !Pointer !Pointer !Pointer !*a -> (!Int,!*a)
	theCall _ _ _ _ = code {
			ccall objc_msgSend_stret "Gppp:I:A"
		}

controlSize tabv env
	# (s,env)	= msgI_I tabv "controlSize\0" env
	= (NSControlSize s,env)

setControlSize tabv (NSControlSize csize) env
	= msgII_V tabv "setControlSize:\0" csize env

:: NSControlSize =: NSControlSize Int
NSRegularControlSize	= NSControlSize 0
NSSmallControlSize		= NSControlSize 1
NSMiniControlSize		= NSControlSize 2

allowsTruncatedLabels tabv env
	= msgI_I tabv "allowsTruncatedLabels\0" env

setAllowsTruncatedLabels tabv bool env
	= msgII_V tabv "setAllowsTruncatedLabels:\0" bool env

delegate tabv env
	= msgI_P tabv "delegate\0" env

setDelegate tabv delegate env
	= msgIP_V tabv "setDelegate:\0" delegate env

tabViewItemAtPoint tabv (x,y) env	// NSPoint -> NSTabViewItem
	# (item,env)	= msgIRR_P tabv "tabViewItemAtPoint:\0" x y env
	= (NSTabViewItem item,env)

tabPosition tabv env
	= msgI_I tabv "tabPosition\0" env

setTabPosition tabv tpos env
	= msgII_V tabv "setTabPosition:\0" tpos env

// `raw` NSTabViewItem

// initWithIdentifier	generic
// drawLabel:inRect:	override for customized label drawing
// label				generic
// sizeOfLabel:			override for customized label drawing
tabState tabi env
	# (s,env)	= msgI_I tabi "tabState\0" env	// returns NSTabState
	= (NSTabState s,env)

:: NSTabState =: NSTabState Int
NSSelectedTab	= NSTabState 0
NSBackgroundTab	= NSTabState 1
NSPressedTab	= NSTabState 2

identifier tabi env
	= msgI_P tabi "identifier\0" env	// identifier is actually a generic pointer, above we always point to NSString

setIdentifier (NSTabViewItem tab) identifier env
	= msgIP_V tab "setIdentifier:\0" (p2ns identifier) env

// could add setIdentifier
// color				deprecated, tabs are now themed
// view					generic
// initialFirstResponder	generic

instance tabView NSTabViewItem where
	tabView (NSTabViewItem tabi) env
		#	(v,env)	= msgI_P tabi "tabView\0" env
		= (NSTabView v,env)

toolTip tabi env
	#	(s,env)	= msgI_P tabi "toolTip\0" env
	= (ns2cls s,env)

setTooltip tabi tooltip env
	= msgIP_V tabi "setTooltip:\0" (p2ns tooltip) env

tabViewItemWithViewController :: !Int !*env -> (!NSTabViewItem,!*env)
tabViewItemWithViewController viewController env
	# (tabi,env)	= msgCP_P "NSTabViewItem\0" "tabViewItemWithViewController:\0" viewController env
	= (NSTabViewItem  tabi,env)
// image			NSImage				get/set
// viewController	NSViewController	get/set


// `raw` NSTabViewDelegate

// delegate can respond to:

// - (void)tabViewDidChangeNumberOfTabViewItems:(NSTabView *)tabView;
// - (BOOL)tabView:(NSTabView *)tabView shouldSelectTabViewItem:(NSTabViewItem *)tabViewItem;
// - (void)tabView:(NSTabView *)tabView willSelectTabViewItem:(NSTabViewItem *)tabViewItem;
// - (void)tabView:(NSTabView *)tabView didSelectTabViewItem:(NSTabViewItem *)tabViewItem;


// `raw` NSTabViewController (+/+ inherited NSViewController)

tabStyle :: !NSTabViewController !*env -> (!NSTabViewControllerTabStyle,!*env)
tabStyle (NSTabViewController tvc) env
	# (s,env)	= msgI_I tvc "tabStyle\0" env
	= (NSTabViewControllerTabStyle s,env)

setTabStyle :: !NSTabViewController ! NSTabViewControllerTabStyle !*env -> *env
setTabStyle (NSTabViewController tvc) (NSTabViewControllerTabStyle tstyl) env
	= msgII_V tvc "setTabStyle:\0" tstyl env

:: NSTabViewControllerTabStyle =: NSTabViewControllerTabStyle Int
NSTabViewControllerTabStyleSegmentedControlOnTop :: NSTabViewControllerTabStyle
NSTabViewControllerTabStyleSegmentedControlOnTop	= NSTabViewControllerTabStyle 0
NSTabViewControllerTabStyleSegmentedControlOnBottom :: NSTabViewControllerTabStyle
NSTabViewControllerTabStyleSegmentedControlOnBottom	= NSTabViewControllerTabStyle 1
NSTabViewControllerTabStyleToolbar :: NSTabViewControllerTabStyle
NSTabViewControllerTabStyleToolbar					= NSTabViewControllerTabStyle 2
NSTabViewControllerTabStyleUnspecified :: NSTabViewControllerTabStyle
NSTabViewControllerTabStyleUnspecified				= NSTabViewControllerTabStyle -1

// Accessing this property creates the tab view object if it does not already exist.

instance tabView NSTabViewController where
	tabView (NSTabViewController tvc) env
		#	(r,env)	= msgI_P tvc "tabView\0" env
		= (NSTabView r,env)

// You may provide your own tab view by assigning it to this property. If you do so,
// you must assign your custom object before the tab view controller creates one of
// its own. In other words, you must assign your tab view object to this property while
// the view​Loaded property is still NO.
setTabView (NSTabViewController tvc) (NSTabView tview) env
	= msgIP_V tvc "setTabView:\0" tview env

transitionOptions (NSTabViewController tvc) env		// default: NSViewControllerTransitionCrossfade + NSViewControllerTransitionAllowUserInteraction
	#	(to,env)	= msgI_I tvc "transitionOptions\0" env
	= (NSViewControllerTransitionOptions to,env)

setTransitionOptions (NSTabViewController tvc) (NSViewControllerTransitionOptions to) env
	= msgII_V tvc "setTransitionOptions:\0" to env

// default: YES
// when YES and tab view controller title = nil, get title from selected child view controller
// when NO the tab view controller always provides the title, which may be nil
canPropagateSelectedChildViewControllerTitle (NSTabViewController tvc) env
	= msgI_I tvc "canPropagateSelectedChildViewControllerTitle\0" env

setCanPropagateSelectedChildViewControllerTitle (NSTabViewController tvc) bool env
	= msgII_V tvc "setCanPropagateSelectedChildViewControllerTitle:\0" bool env

instance tabViewItems NSTabViewController where
	tabViewItems (NSTabViewController tvc) env
		= msgI_P tvc "tabViewItems\0" env	// returns NSArray of NSTabViewItem

setTabViewItems (NSTabViewController tvc) items env
	= msgIP_V tvc "setTabViewItems:\0" items env

tabViewItemForViewController (NSTabViewController tvc) vc env	// NSViewController -> NSTabViewItem
	= msgIP_P tvc "tabViewItemForViewController:\0" vc env

instance addTabViewItem NSTabViewController where
	addTabViewItem (NSTabViewController tvc) (NSTabViewItem tvi) env
		= msgIP_V tvc "addTabViewItem:\0" tvi env

instance insertTabViewItemAtIndex NSTabViewController where
	insertTabViewItemAtIndex (NSTabViewController tvc) (NSTabViewItem tvi) idx env
		= msgIPI_V tvc "insertTabViewItem:atIndex:\0" tvi idx env

instance removeTabViewItem NSTabViewController
where
	removeTabViewItem :: !NSTabViewController ! NSTabViewItem !*env -> *env
	removeTabViewItem (NSTabViewController tvc) (NSTabViewItem tvi) env
		= msgIP_V tvc "removeTabViewItem:\0" tvi env

selectedTabViewItemIndex (NSTabViewController tvc) env
	= msgI_I tvc "selectedTabViewItemIndex\0" env

setSelectedTabViewItemIndex (NSTabViewController tvc) idx env
	= msgII_V tvc "setSelectedTabViewItemIndex:\0" idx env

// viewDidLoad		can override in subclass to perform custom initialisation
// NSTabViewController implements NSTabViewDelegate methods		override in subclass for customisation

// override to specialise toolbar support:
// - (NSToolbarItem *)toolbar:(NSToolbar *)toolbar itemForItemIdentifier:(NSString *)itemIdentifier  willBeInsertedIntoToolbar:(BOOL)flag;
// - (NSArray<NSString *> *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar;
// - (NSArray<NSString *> *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar;
// - (NSArray<NSString *> *)toolbarSelectableItemIdentifiers:(NSToolbar *)toolbar;

