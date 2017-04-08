implementation module Clyde.menus

import StdEnv
import System._Pointer
import Cocoa.Foundation
import Cocoa.msg
import Cocoa.objc

// 'cocoa' functions

NSFindPanelActionShowFindPanel = 1
NSFindPanelActionNext = 2
NSFindPanelActionPrevious = 3
NSFindPanelActionReplaceAll = 4
NSFindPanelActionReplace = 5
NSFindPanelActionReplaceAndFind = 6
NSFindPanelActionSetFindString = 7
NSFindPanelActionReplaceAllInSelection = 8
NSFindPanelActionSelectAll = 9
NSFindPanelActionSelectAllInSelection = 10

removeItemWithTitle :: !Menu !Title !*a -> *a
removeItemWithTitle menu title env
	#!	(mitem,env)		= itemWithTitle menu title env
		env				= case mitem of
							0	-> env
							_	-> removeItem menu mitem env
	= env

performSelectorWithObject :: !NSObject !String !Pointer !*a -> *a
performSelectorWithObject object selname arg env
	#!	(sel,env)		= sel_getUid (packString selname) env
		(_,env)			= msgIPP_P object "performSelector:withObject:\0" sel arg env
	= env

setAppleMenu :: !Menu !*a -> *a
setAppleMenu menu env
	= performSelectorWithObject application "setAppleMenu:" menu env

setWindowsMenu :: !Menu !*a -> *a
setWindowsMenu menu env
	= msgIP_V application "setWindowsMenu:\0" menu env

addMainmenu :: !*a -> (!Menu,!*a)
addMainmenu env
	#!	(main,env) 	= allocObject "NSMenu" env
		(main,env) 	= msgIP_P main "initWithTitle:\0" (c2ns "MainMenu\0") env
		(main,env) 	= msgI_P main "autorelease\0" env
	= (main,env)

setMainMenu :: !Menu !*a -> (!Int,!*a)
setMainMenu menu env
	= msgIP_P application "setMainMenu:\0" menu env

addSubmenu :: !Menu !String !*a -> (!Menu,!*a)
addSubmenu menu title env
	#!	(item,env)	= addItemWith_title menu title env
		(smenu,env)	= allocObject "NSMenu" env
		(smenu,env)	= msgIP_P smenu "initWithTitle:\0" (p2ns title) env
		(smenu,env)	= msgI_P smenu "autorelease\0" env
		(_,env)		= msgIPP_P menu "setSubmenu:forItem:\0" smenu item env
	= (smenu,env)

addSeparator :: !Menu !*a -> *a
addSeparator menu env
	#!	(sepI,env)	= msgC_P "NSMenuItem\0" "separatorItem\0" env
		(_,env)		= msgIP_P menu "addItem:\0" sepI env
	= env

addItemWith_title :: !Menu !Title !*a -> (!MenuItem,!*a)
addItemWith_title menu title env
	#!	title`	= p2ns title
		action	= 0				// nil action
		key		= ""			// no key equivalent
		key`	= p2ns key
	= msgIPPP_P menu "addItemWithTitle:action:keyEquivalent:\0" title` action key` env

addItemWith_title_keyEquivalent :: !Menu !Title !KeyEquivalent !*a -> (!MenuItem,!*a)
addItemWith_title_keyEquivalent menu title key env
	#!	title`	= p2ns title
		key`	= p2ns key
		action	= 0				// nil action
	= msgIPPP_P menu "addItemWithTitle:action:keyEquivalent:\0" title` action key` env

addItemWith_title_action_keyEquivalent :: !Menu !Title !Action !KeyEquivalent !*a -> (!MenuItem,!*a)
addItemWith_title_action_keyEquivalent menu title action key env
	#!	title`			= p2ns title
		key`			= p2ns key
		(action`,env)	= sel_getUid (packString action) env
	= msgIPPP_P menu "addItemWithTitle:action:keyEquivalent:\0" title` action` key` env
/*
addItemWithTitle_action_keyEquivalent :: !Menu !Title !Action` !KeyEquivalent !*a -> (!MenuItem,!*a)
addItemWithTitle_action_keyEquivalent menu title action key env
	#!	title`	= p2ns title
		key`	= p2ns key
	= msgIPPP_P menu "addItemWithTitle:action:keyEquivalent:\0" title` action key` env

insertItemWith_title_action_keyEquivalent_atIndex :: !Menu !Title !Action !KeyEquivalent !Int !*a -> (!MenuItem,!*a)
insertItemWith_title_action_keyEquivalent_atIndex menu title action key idx env
	#!	title`			= p2ns title
		key`			= p2ns key
		(action`,env)	= sel_getUid action env
//	= msgIPPPP_P menu "addItemWithTitle:action:keyEquivalent:\0" title` action` key` idx env
	= msgIPPPP_I menu "addItemWithTitle:action:keyEquivalent:\0" title` action` key` idx env
*/
removeItem :: !Menu !MenuItem !*a -> *a
removeItem menu item env
	= msgIP_V menu "removeItem:\0" item env

removeItemAtIndex :: !Menu !Int !*a -> *a
removeItemAtIndex menu idx env
	= msgII_V menu "removeItemAtIndex:\0" idx env

itemWithTag :: !Menu !Int !*a -> (!MenuItem,!*a)
itemWithTag menu tag env
	= msgII_P menu "itemWithTag:\0" tag env

itemWithTitle :: !Menu !Title !*a -> (!MenuItem,!*a)
itemWithTitle menu title env
	#!	title`			= p2ns title
	= msgIP_P menu "itemWithTitle:\0" title` env

itemAtIndex :: !Menu !Int !*a -> (!MenuItem,!*a)
itemAtIndex menu idx env
	= msgII_P menu "itemAtIndex:\0" idx env

setTag :: !MenuItem !Int !*a -> *a
setTag item tag env
	= msgIP_V item "setTag:\0" tag env

setTarget :: !MenuItem !Object !*a -> *a
setTarget item app env
	#!	env			= msgIP_V item "setTarget:\0" app env
	= env

setKeyEquivalentModifierMask :: !MenuItem !Int !*a -> *a
setKeyEquivalentModifierMask item equi env
	= msgIP_V item "setKeyEquivalentModifierMask:\0" equi env

setRepresentedObject :: !MenuItem !String !*a -> *a
setRepresentedObject item name env
	= msgIP_V item "setRepresentedObject:\0" (p2ns name) env

populateMainMenu :: !*World -> *World
populateMainMenu world
	// The titles of the menu items are for identification purposes only and shouldn't be localized.
	// The strings in the menu bar come from the submenu titles,
	// except for the application menu, whose title is ignored at runtime.

	#!	(main,world)	= addMainmenu world
		
		(menu,world)	= addSubmenu main "Apple" world
		world			= setAppleMenu menu world
		world			= populateApplicationMenu menu world
		
		(menu,world)	= addSubmenu main "File" world
		world			= populateFileMenu menu world
		
		(menu,world)	= addSubmenu main "Edit" world
		world			= populateEditMenu menu world
		
		(menu,world)	= addSubmenu main "Project" world
		world			= populateProjectMenu menu world
		
		(menu,world)	= addSubmenu main "View" world
		world			= populateViewMenu menu world
		
		(menu,world)	= addSubmenu main "Window" world
		world			= populateWindowMenu menu world
		world			= setWindowsMenu menu world
		
		(menu,world)	= addSubmenu main "Help" world
		world			= populateHelpMenu menu world
		
		(menu,world)	= addSubmenu main "Debug" world
		world			= populateDebugMenu menu world

		(_,world)		= setMainMenu main world
	= world

populateApplicationMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu ("About " +++. applicationName) "orderFrontStandardAboutPanel:" "" world
		world			= setTarget item application world

		world			= addSeparator menu world
		
		(item,world)	= addItemWith_title_keyEquivalent menu "Preferences..." "," world
		
		world			= addSeparator menu world

		(smenu,world)	= addSubmenu menu "Services" world
		
		world			= addSeparator menu world

		(item,world)	= addItemWith_title_action_keyEquivalent menu ("Hide "+++applicationName) "hide:" "h" world
		world			= setTarget item application world

		(item,world)	= addItemWith_title_action_keyEquivalent menu "Hide Others" "hideOtherApplications:" "h" world
		world			= setKeyEquivalentModifierMask item (NSCommandKeyMask bitor NSAlternateKeyMask) world
		world			= setTarget item application world

		(item,world)	= addItemWith_title_action_keyEquivalent menu "Show All" "unhideAllApplications:" "" world
		world			= setTarget item application world

		world			= addSeparator menu world

		(item,world)	= addItemWith_title_action_keyEquivalent menu ("Quit "+++applicationName) "terminate:" "q" world
		world			= setTarget item application world
	= world

populateFileMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "New" "newDocument:" "n" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Open..." "openDocument:" "o" world

		(smenu,world)	= addSubmenu menu "Open Recent" world
		world			= performSelectorWithObject smenu "_setMenuName:" (p2ns "NSRecentDocumentsMenu") world

		(item,world)	= addItemWith_title_action_keyEquivalent smenu "Clear Menu" "clearRecentDocuments:" "" world

		world			= addSeparator menu world
		
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Close" "performClose:" "w" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Save" "saveDocument:" "s" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Save As..." "saveDocumentAs:" "S" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Revert" "revertDocumentToSaved:" "" world

		world			= addSeparator menu world
		
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Page Setup..." "runPageLayout:" "P" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Print..." "print:" "p" world
	= world

populateEditMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Undo" "undo:" "z" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Redo" "redo:" "Z" world

		world			= addSeparator menu world
		
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Cut" "cut:" "x" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Copy" "copy:" "c" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Paste" "pasteAsPlainText:" "v" world
/*
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Paste" "paste:" "v" world

		(item,world)	= addItemWith_title_action_keyEquivalent menu "Paste and Match Style" "pasteAsPlainText:" "V" world
		world			= setKeyEquivalentModifierMask item (NSCommandKeyMask bitor NSAlternateKeyMask) world
*/
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Delete" "delete:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Select All" "selectAll:" "a" world

		world			= addSeparator menu world
		
		(smenu,world)	= addSubmenu menu "Find" world
		world			= populateFindMenu smenu world

		(smenu,world)	= addSubmenu menu "Spelling" world
		world			= populateSpellingMenu smenu world
	= world

populateFindMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Find..." "performFindPanelAction:" "f" world
		world			= setTag item NSFindPanelActionShowFindPanel world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Find Next" "performFindPanelAction:" "g" world
		world			= setTag item NSFindPanelActionNext world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Find Previous" "performFindPanelAction:" "G" world
		world			= setTag item NSFindPanelActionPrevious world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Use Selection for Find" "performFindPanelAction:" "e" world
		world			= setTag item NSFindPanelActionSetFindString world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Jump to Selection" "centerSelectionInVisibleArea:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Go to line..." "didTapOpenButton:" "j" world
	= world

populateSpellingMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Spelling..." "showGuessPanel:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Check Spelling" "checkSpelling:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Check Spelling as You Type" "toggleContinuousSpellChecking:" "" world
	= world

populateViewMenu menu world
	// TODO
	= world

populateWindowMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Minimize" "performMiniaturize:" "m" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Zoom" "performZoom:" "" world
		world			= addSeparator menu world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Bring All to Front" "arrangeInFront:" "" world
	= world

populateHelpMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu (applicationName +++. " Help") "showHelp:" "?" world
		world			= setTarget item application world
	= world

populateDebugMenu menu world
	// TODO
	#!	(delegate,world)	= applicationDelegate world
		(item,world)		= addItemWith_title_action_keyEquivalent menu ("Log windows") "logwindows:" "" world
		world				= setTarget item delegate world

		(item,world)		= addItemWith_title_action_keyEquivalent menu ("Test") "test:" "" world
		world				= setTarget item delegate world
	= world

populateProjectMenu menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Build" "build:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Build & Run" "buildAndRun:" "" world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Run" "run:" "" world
		world			= addSeparator menu world
		(delegate,world)= applicationDelegate world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Open implementation module" "openIcl:" "\n" world
		world			= setKeyEquivalentModifierMask item (0) world
//		(item,world)	= addItemWith_title_action_keyEquivalent menu "Open implementation module" "doubleClick:" "m" world
		world			= setTarget item delegate world
		(item,world)	= addItemWith_title_action_keyEquivalent menu "Open definition module" "openDcl:" "\n" world
		world			= setKeyEquivalentModifierMask item (NSShiftKeyMask) world
//		(item,world)	= addItemWith_title_action_keyEquivalent menu "Open definition module" "doubleClick:" "M" world
		world			= setTarget item delegate world
/*
		world			= addSeparator menu world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Clyde.prj" "project:" "" world
		world			= setRepresentedObject item "Clyde.prj" world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "Play.prj" "project:" "" world
		world			= setRepresentedObject item "Play.prj" world
	#!	(item,world)	= addItemWith_title_action_keyEquivalent menu "hex.prj" "project:" "" world
		world			= setRepresentedObject item "hex.prj" world
*/
	= world
