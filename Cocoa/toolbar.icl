implementation module Cocoa.toolbar

import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation

makeTool :: !Int !*World -> *World
makeTool wind env
	#!	(tool,env)		= allocObject "NSToolbar" env
//		tool`			= NSToolbar tool
		env				= initWithIdentifier (NSToolbar tool) "mytoolbar" env
		env				= msgIP_V wind "setToolbar:\0" tool env
		env				= setVisible (NSToolbar tool) YES env
		env				= setAllowsUserCustomization (NSToolbar tool) YES env
		env				= setDelegate (NSToolbar tool) (NSToolbarDelegate wind) env

		env				= insertItemWithItemIdentifierAtIndex (NSToolbar tool) "NSToolbarShowColorsItem" 0 env
		env				= insertItemWithItemIdentifierAtIndex (NSToolbar tool) "NSToolbarShowFontsItem" 1 env
		env				= insertItemWithItemIdentifierAtIndex (NSToolbar tool) "NSToolbarSpaceItem" 2 env
		env				= insertItemWithItemIdentifierAtIndex (NSToolbar tool) "NSToolbarPrintItem" 3 env
	= env

// `Raw` for NSToolbar

//- (instancetype)initWithIdentifier:(NSString *)identifier;

initWithIdentifier :: !NSToolbar !String !*env -> *env
initWithIdentifier (NSToolbar bar) identifier env
	= msgIP_V bar "initWithIdentifier:\0" (p2ns identifier) env

// @property NSToolbarDisplayMode displayMode;

displayMode (NSToolbar tbar) env
	#	(m,env)	= msgI_I tbar "displayMode\0" env
	= (NSToolbarDisplayMode m,env)

setDisplayMode (NSToolbar tbar) (NSToolbarDisplayMode mode) env
	= msgII_V tbar "setDisplayMode:\0" mode env

:: NSToolbarDisplayMode		=: NSToolbarDisplayMode Int

NSToolbarDisplayModeDefault			= NSToolbarDisplayMode 0
NSToolbarDisplayModeIconAndLabel	= NSToolbarDisplayMode 1
NSToolbarDisplayModeIconOnly		= NSToolbarDisplayMode 2
NSToolbarDisplayModeLabelOnly		= NSToolbarDisplayMode 3

// @property BOOL showsBaselineSeparator;

showsBaselineSeparator (NSToolbar tbar) env
	= msgI_I tbar "showsBaselineSeparator\0" env

setShowsBaselineSeparator (NSToolbar tbar) bool env
	= msgII_V tbar "setShowsBaselineSeparator:\0" bool env

// @property BOOL allowsUserCustomization;

allowsUserCustomization (NSToolbar tbar) env
	= msgI_I tbar "allowsUserCustomization\0" env


setAllowsUserCustomization (NSToolbar tbar) bool env
	= msgII_V tbar "setAllowsUserCustomization:\0" bool env

// @property BOOL allowsExtensionItems;

allowsExtensionItems (NSToolbar tbar) env
	= msgI_I tbar "allowsExtensionItems\0" env


setAllowsExtensionItems (NSToolbar tbar) bool env
	= msgII_V tbar "setAllowsExtensionItems:\0" bool env

// @property(readonly, copy) NSString *identifier;

identifier (NSToolbar tbar) env
	# (ns,env)	= msgI_P tbar "identifier\0" env
	= (ns2cls ns,env)

// @property(readonly, copy) NSArray<__kindof NSToolbarItem *> *items;

items (NSToolbar tbar) env
	= msgI_P tbar "items\0" env

// @property(readonly, copy) NSArray<__kindof NSToolbarItem *> *visibleItems;

visibleItems (NSToolbar tbar) env
	= msgI_P tbar "visibleItems\0" env

// @property NSToolbarSizeMode sizeMode;

sizeMode (NSToolbar tbar) env
	# (m,env)	= msgI_I tbar "sizeMode\0" env
	= (NSToolbarSizeMode m,env)

setSizeMode (NSToolbar tbar) (NSToolbarSizeMode mode) env
	= msgII_V tbar "setSizeMode:\0" mode env

:: NSToolbarSizeMode	=: NSToolbarSizeMode Int
NSToolbarSizeModeDefault	= NSToolbarSizeMode 0
NSToolbarSizeModeRegular	= NSToolbarSizeMode 1
NSToolbarSizeModeSmall		= NSToolbarSizeMode 2

// @property(assign) id<NSToolbarDelegate> delegate;

delegate (NSToolbar tbar) env
	# (d,env)	= msgI_P tbar "delegate\0" env
	= (NSToolbarDelegate d,env)

setDelegate (NSToolbar tbar) (NSToolbarDelegate tdel) env
	= msgIP_V tbar "setDelegate:\0" tdel env

// - (void)insertItemWithItemIdentifier:(NSString *)itemIdentifier atIndex:(NSInteger)index;

insertItemWithItemIdentifierAtIndex :: !NSToolbar !String !Int !*env -> *env
insertItemWithItemIdentifierAtIndex (NSToolbar tbar) itemIdentifier index env
	= msgIPI_V tbar "insertItemWithItemIdentifier:atIndex:\0" (p2ns itemIdentifier) index env

// - (void)removeItemAtIndex:(NSInteger)index;

removeItemAtIndex (NSToolbar tbar) index env
	= msgII_V tbar "removeItemAtIndex:\0" index env

// @property(copy) NSString *selectedItemIdentifier;

selectedItemIdentifier (NSToolbar tbar) env
	# (i,env)	= msgI_P tbar "selectedItemIdentifier\0" env
	= (ns2cls i,env)

setSelectedItemIdentifier (NSToolbar tbar) itemIdentifier env
	= msgIP_V tbar "setSelectedItemIdentifier:\0" (p2ns itemIdentifier) env

// @property(getter=isVisible) BOOL visible;

visible :: !NSToolbar !*env -> (!BOOL,!*env)
visible (NSToolbar tbar) env
	= msgI_I tbar "visible\0" env

setVisible :: !NSToolbar !BOOL !*env -> *env
setVisible (NSToolbar tbar) bool env
	= msgII_V tbar "setVisible:\0" bool env

// - (void)runCustomizationPalette:(id)sender;

runCustomizationPalette (NSToolbar tbar) sender env
	= msgIP_V tbar "runCustomizationPalette\0" sender env

// @property(readonly) BOOL customizationPaletteIsRunning;

customizationPaletteIsRunning (NSToolbar tbar) env
	= msgI_I tbar "customizationPaletteIsRunning\0" env

// @property BOOL autosavesConfiguration;

autosavesConfiguration (NSToolbar tbar) env
	= msgI_I tbar "autosavesConfiguration\0" env

setAutosavesConfiguration (NSToolbar tbar) bool env
	= msgII_V tbar "setAutosavesConfiguration:\0" bool env

// @property(readonly, copy) NSDictionary<NSString *,id> *configurationDictionary;

configurationDictionary (NSToolbar tbar) env
	= msgI_P tbar "configurationDictionary\0" env

// - (void)validateVisibleItems;

validateVisibleItems (NSToolbar tbar) env
	= msgI_V tbar "validateVisibleItems\0" env

// Notifications...
// NSNotificationName NSToolbarDidRemoveItemNotification;
// NSNotificationName NSToolbarWillAddItemNotification;

// - (void)setConfigurationFromDictionary:(NSDictionary<NSString *,id> *)configDict;

setConfigurationFromDictionary (NSToolbar tbar) dict env
	= msgIP_V tbar "setConfigurationFromDictionary:\0" dict env


// `Raw` for NSToolbarItem

// - (instancetype)initWithItemIdentifier:(NSString *)itemIdentifier;

initWithItemIdentifier :: !NSToolbarItem !String !*env -> *env
initWithItemIdentifier (NSToolbarItem item) itemIdentifier env
	= msgIP_V item "initWithItemIdentifier:\0" (p2ns itemIdentifier) env

// @property(readonly, copy) NSString *itemIdentifier;

itemIdentifier (NSToolbarItem item) env
	# (ns,env)	= msgI_P item "itemIdentifier\0" env
	= (ns2cls ns,env)

// @property(readonly, assign) NSToolbar *toolbar;

toolbar (NSToolbarItem item) env
	# (tb,env)	= msgI_P item "toolbar\0" env
	= (NSToolbar tb,env)

// @property(copy) NSString *label;

label (NSToolbarItem item) env
	# (ns,env)	= msgI_P item "label\0" env
	= (ns2cls ns,env)

setLabel (NSToolbarItem item) label env
	= msgIP_V item "setLabel:\0" (p2ns label) env

// @property(copy) NSString *paletteLabel;

paletteLabel (NSToolbarItem item) env
	# (ns,env)	= msgI_P item "paletteLabel\0" env
	= (ns2cls ns,env)

setPaletteLabel (NSToolbarItem item) label env
	= msgIP_V item "setPaletteLabel:\0" (p2ns label) env

// @property(copy) NSString *toolTip;

toolTip (NSToolbarItem item) env
	# (ns,env)	= msgI_P item "toolTip\0" env
	= (ns2cls ns,env)

setToolTip (NSToolbarItem item) tooltip env
	= msgIP_V item "setToolTip:\0" (p2ns tooltip) env

// @property(strong) NSMenuItem *menuFormRepresentation;

menuFormRepresentation (NSToolbarItem item) env
	# (ns,env)	= msgI_P item "menuFormRepresentation\0" env
//	= (NSMenuItem ns,env)
	= (ns,env)

setMenuFormRepresentation (NSToolbarItem item) menuitem env
	= msgIP_V item "setMenuFormRepresentation:\0" menuitem env

// @property NSInteger tag;

tag (NSToolbarItem item) env
	= msgI_I item "tag\0" env

setTag (NSToolbarItem item) tag env
	= msgII_V item "setTag:\0" tag env

// @property(weak) id target;

target (NSToolbarItem item) env
	= msgI_P item "target\0" env

setTarget (NSToolbarItem item) target env
	= msgIP_V item "setTarget:\0" target env

// @property SEL action;

action (NSToolbarItem item) env
	= msgI_P item "action\0" env

setAction (NSToolbarItem item) action env
	= msgIP_V item "setAction:\0" action env

// @property(getter=isEnabled) BOOL enabled;

isEnabled (NSToolbarItem item) env
	= msgI_I item "isEnabled\0" env

setEnabled (NSToolbarItem item) enabled env
	= msgII_V item "setEnabled:\0" enabled env

// @property(strong) NSImage *image;

image (NSToolbarItem item) env
	= msgI_P item "image\0" env

setImage (NSToolbarItem item) image env
	= msgIP_V item "setImage:\0" image env

// @property(strong) NSView *view;

view (NSToolbarItem item) env
	= msgI_P item "view\0" env

setView (NSToolbarItem item) view env
	= msgIP_V item "setView:\0" view env

// @property NSSize minSize;
// @property NSSize maxSize;
// @property NSInteger visibilityPriority;

visibilityPriority (NSToolbarItem item) env
	= msgI_I item "visibilityPriority\0" env

setVisibilityPriority (NSToolbarItem item) priority env
	= msgII_V item "setVisibilityPriority:\0" priority env

NSToolbarItemVisibilityPriorityStandard = 0
NSToolbarItemVisibilityPriorityLow = -1000
NSToolbarItemVisibilityPriorityHigh = 1000
NSToolbarItemVisibilityPriorityUser = 2000

// - (void)validate;

validate (NSToolbarItem item) env
	= msgI_V item "validate\0" env

// @property BOOL autovalidates;

autovalidates (NSToolbarItem item) env
	= msgI_I item "autovalidates\0" env

setAutovalidates (NSToolbarItem item) bool env
	= msgII_V item "setAutovalidates:\0" bool env

// @property(readonly) BOOL allowsDuplicatesInToolbar;

allowsDuplicatesInToolbar (NSToolbarItem item) env
	= msgI_I item "allowsDuplicatesInToolbar\0" env

// Standard NSToolbarItem

//NSString *NSToolbarSpaceItemIdentifier;
//NSString *NSToolbarFlexibleSpaceItemIdentifier;
//NSString *NSToolbarShowColorsItemIdentifier;
//NSString *NSToolbarShowFontsItemIdentifier;
//NSString *NSToolbarPrintItemIdentifier;				// sends printDocument:
//NSString *NSToolbarToggleSidebarItemIdentifier;		// sends toggleSidebar:


// `Raw` for NSToolbarItemGroup

// @property(copy) NSArray<__kindof NSToolbarItem *> *subitems;

subitems (NSToolbarItemGroup group) env
	= msgI_P group "subitems\0" env

setSubitems (NSToolbarItemGroup group) items env
	= msgIP_V group "setSubitems:\0" items env


// `Raw` for NSToolbarDelegate

// - (NSToolbarItem *)toolbar:(NSToolbar *)toolbar itemForItemIdentifier:(NSString *)itemIdentifier willBeInsertedIntoToolbar:(BOOL)flag;
// - (NSArray<NSString *> *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar;
// - (NSArray<NSString *> *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar;
// - (NSArray<NSString *> *)toolbarSelectableItemIdentifiers:(NSToolbar *)toolbar;

// - (void)toolbarWillAddItem:(NSNotification *)notification;
// - (void)toolbarDidRemoveItem:(NSNotification *)notification;

