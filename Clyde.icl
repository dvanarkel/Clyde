module Clyde

/* Fix launch... when we have been playing with info.plist
lsregister -f Clyde.app
*/

import StdEnv

from System.CommandLine					import setReturnCode

from Cocoa.Foundation					import ::NSApplication, sharedApplication , runApplication, makeUnbundledLaunchable

from Clyde.projwindowcontroller			import makeProjWindowControllerClass
from Clyde.ClydeApplicationController	import createAppDelegate, initAppDelegate, swizzleAboutPanel
from Clyde.textdocument					import createTextDocument
from Clyde.projdocument					import createProjDocumentClass
from Clyde.menus						import populateMainMenu
from Clyde.DebugClyde					import installDebug

Start world
	#!	world			= installDebug world
		(app,world) 	= sharedApplication world
		world			= swizzleAboutPanel world
		world			= populateMainMenu world
		world			= createAppDelegate world				// register AppDelegate class
		world			= createTextDocument world				// register TextDocument class
		world			= createProjDocumentClass world			// register ProjDocument class
		world			= makeProjWindowControllerClass world	// register ProjWindowController class
		world			= makeUnbundledLaunchable app world		// not sure if this is working or changes anything?!
		world			= initAppDelegate app world
		world			= runApplication app world
		ret				= 42
		world			= setReturnCode ret world
	= world


/*
TODO items for Clyde:

[MUST before release]
=> update project window after build... (so file watcher?)
=> errwin & messwin & typewin
=> project global search
* syncol issues
* after first save next required save not recognised immediately?!

X auto turn off substitutions in text windows (otherwise eg syncol causes errors (as ellipsis is 1 storage character but 3 in string so we may colour past end of string))

X why does opening document create two windows?
X shift double click should open edit window for definition module (context menu in project window?)
? doubleAction broken again in project window?? fixed, but still have to detect shift key for double-click
* make projects open in the project window but openable 'as text'
X 4 char tabs
* make project a functioning document type
* validate & make context sensitive menu entries for project window
X ask save edited files before close...

* figure out why callback is not invoked when starting from UI but does in Terminal??

* update module mangling so that hierarchical modules are part of directory name instead of module name
	eg				{Project}.Cocoa		dyncall
	rather than		{Project}			Cocoa.dyncall

* cascade windows when opening
X stop creating 'untitled' when opening app
* document isn't marked as 'dirty' when editing
* strange window movement when resizing
* add line numbers
* clean sensitive find functionality

X make menu titles take name from bundle rather than hardcoding

X sigh... crashing again on logwindows... now is the issue in logwindows or in how my windows are set up???
	ok => not in the windows as I have same issue without _any_ of my own windows open
	was issue in logwindows (incorrectly retrieving NSRect result)

X still need to figure out why textview content rect incorrect...
	=> adding inset rect fixes this??

X change project window to outline view
X double click projwin entry should open edit window for implementation module

W cross -compilation
W xref
W hoogle
W api documentation
*/

