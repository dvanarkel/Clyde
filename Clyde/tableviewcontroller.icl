implementation module Clyde.tableviewcontroller

import StdEnv
import StdDebug
import System._Pointer
import System._Unsafe
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation
import Cocoa.dyncall
import Clyde.controls

tableViewControllerMethods :: [(!String,!Int,!String)]
tableViewControllerMethods =
	[ ("numberOfRowsInTableView:",					imp_numberOfRowsInTableView,	"i@:@\0")
	, ("tableView:objectValueForTableColumn:row:",	exportedCBHandler,				"@@:@@i\0")
	, ("tableView:isGroupRow:",						imp_tableView_isGroupRow,		"i@:@i\0")	// lies about return type...
	, ("tableView:shouldSelectRow:",				imp_tableView_shouldSelectRow,	"i@:@i\0")	// lies about return type...
	]

// excised from application delegate...
createTableViewController :: !Pointer !*World -> *World
createTableViewController adc world
	#!	(sel,world)		= sel_getUid "numberOfRowsInTableView:\0" world
		imp				= imp_numberOfRowsInTableView
		(ok,world)		= class_addMethod adc sel imp "i@:@\0" world
//		world = trace_n ("Installing numberOfRowsInTableView...\t"+++toString sel+++"\t"+++toString imp+++"\t"+++toString ok) world

		(sel,world)		= sel_getUid "tableView:objectValueForTableColumn:row:\0" world
//		(imp,world)		= imp_tableView_objectValueForTableColumn_row world
		imp				= exportedCBHandler
	| imp == 0
		#	(err,world)	= dcGetError imp world
		= abort "\nempty exportedCBHandler\n\n"
	#	(ok,world)		= class_addMethod adc sel imp "@@:@@i\0" world

		(sel,world)		= sel_getUid "tableView:isGroupRow:\0" world
		imp				= imp_tableView_isGroupRow
		(ok,world)		= class_addMethod adc sel imp "i@:@i\0" world		// lying about return type here...

		(sel,world)		= sel_getUid "tableView:shouldSelectRow:\0" world
		imp				= imp_tableView_shouldSelectRow
		(ok,world)		= class_addMethod adc sel imp "i@:@i\0" world		// lying about return type here...
		
	= world
	
createPLView :: !Pointer !Pointer !*a -> *a
createPLView delegate container env
	#!	(bounds,env)	= getBounds container env
		(scroll,env)	= msgC_P "NSScrollView\0" "alloc\0" env
		(scroll,env)	= msgIS_P scroll "initWithFrame:\0" NSRectType bounds env
		env				= msgII_V scroll "setBorderType:\0" NSBezelBorder env
		env				= msgII_V scroll "setHasVerticalScroller:\0" YES env
		env				= msgII_V scroll "setHasHorizontalScroller:\0" YES env
//		env				= msgII_V scroll "setAutohidesScrollers:\0" NO env
		env				= msgII_V scroll "setAutohidesScrollers:\0" YES env
    //This allows the view to be resized by the view holding it 
//    [tableContainer setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		env				= msgII_V scroll "setAutoresizingMask:\0" (NSViewWidthSizable + NSViewHeightSizable) env
// TODO: Hmmmm... now horizontal scrollbar appears if I resize column... but not if I resize window???

		(cont,env)		= msgI_P scroll "contentView\0" env
		(bounds1,env)	= getBounds cont env
//		env = trace_n ("cont "+++toString cont) env
		origin_x		= readReal8 bounds1 0
		origin_y		= readReal8 bounds1 8
		size_w			= readReal8 bounds1 16
		size_h			= readReal8 bounds1 24
//		env = trace_n (toString origin_x+++"\t"+++toString origin_y) env
//		env = trace_n (toString size_w+++"\t"+++toString size_h) env
	#!	bounds1			= cgRect 0.0 0.0 298.0 298.0

		(outl,env)		= msgC_P "NSTableView\0" "alloc\0" env
		(outl,env)		= msgIS_P outl "initWithFrame:\0" NSRectType bounds1 env
		
		env				= msgII_V outl "setUsesAlternatingRowBackgroundColors:\0" YES env
// - (void)setUsesAlternatingRowBackgroundColors:(BOOL)useAlternatingRowColors; // FOR Profile viewers...

/*
		(ocol1,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol1,env)		= msgIP_P ocol1 "initWithIdentifier:\0" (c2ns "columnOne\0") env
		(header,env)	= msgI_P ocol1 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "One\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env				// can we set a min width that propagates upwards?
		env				= msgIP_V outl "addTableColumn:\0" ocol1 env

		(ocol2,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(ocol2,env)		= msgIP_P ocol2 "initWithIdentifier:\0" (c2ns "columnTwo\0") env
		(header,env)	= msgI_P ocol2 "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (c2ns "Two\0") env
//		env				= msgIR_V ocol1 "setWidth:\0" 200.0 env
		env				= msgIP_V outl "addTableColumn:\0" ocol2 env
*/		
//		env				= addTableColumn outl "columnOne" "One" env
//		env				= addTableColumn outl "columnTwo" "Two" env
		env				= addColumnHeaders outl headerline env
		
	// set outl delegate & datasource...
		env				= msgIP_V outl "setDelegate:\0" delegate env		// ==> ahh.. probably arrives at app delegate by default so for proper view controller _do_ want to set
		env				= msgIP_V outl "setDataSource:\0" delegate env

		env				= msgIP_V scroll "setDocumentView:\0" outl env
		
		env				= msgIP_V container "addSubview:\0" scroll env
		env				= msgI_V scroll "release\0" env
		env				= msgI_V outl "reloadData\0" env
	= env

addColumnHeaders table [] env	= env
addColumnHeaders table [col:cols] env
	#!	env				= addTableColumn table col col env
	= addColumnHeaders table cols env

addTableColumn table col_identifier col_label env
	#!	(tcol,env)		= msgC_P "NSTableColumn\0" "alloc\0" env
		(tcol,env)		= msgIP_P tcol "initWithIdentifier:\0" (p2ns col_identifier) env
		(header,env)	= msgI_P tcol "headerCell\0" env
		env				= msgIP_V header "setStringValue:\0" (p2ns col_label) env
//		env				= msgIR_V tcol "setWidth:\0" 200.0 env				// can we set a min width that propagates upwards?
		env				= msgIP_V table "addTableColumn:\0" tcol env
	= env


foreign export tableView_isGroupRow 

tableView_isGroupRow :: !Int !Int !Int !Int -> Int
tableView_isGroupRow self cmd tv row
//	| trace_n ("tableView_isGroupRow called\t"+++toString row) False = undef
	#!	(ret,world)		= callback newWorld
	= ret
where
	callback :: !*World -> (!Int,!*World)
	callback env
		// for now don't allow profile expanding
		| True	= (0,env)
/*		
		// check if row is folder or module...
		// return 1 if folder, 0 if module
		#!  res			= case (listItems!!row).[0] of 
								'/'		-> 1
								'\\'	-> 1
								_		-> 0
//		| trace_n ("isGroupRow\t"+++.(listItems!!row)+++"\t"+++toString res) False = undef
		= (res,env)
*/
imp_tableView_isGroupRow :: IMP
imp_tableView_isGroupRow = code {
			pushLc 	tableView_isGroupRow
		}

foreign export tableView_shouldSelectRow

tableView_shouldSelectRow :: !Int !Int !Int !Int -> Int
tableView_shouldSelectRow self cmd tv row
//	| trace_n ("tableView_shouldSelectRow called\t"+++toString row) False = undef
	#!	res		= 1 - tableView_isGroupRow self cmd tv row
	// abuse to set text in 'edit' window??? 
	// 	-	how to find 'edit' window / textview so that we can manipulate
	= res

imp_tableView_shouldSelectRow :: IMP
imp_tableView_shouldSelectRow = code {
			pushLc 	tableView_shouldSelectRow
		}

foreign export numberOfRowsInTableView

numberOfRowsInTableView :: !Int !Int !Int -> Int
numberOfRowsInTableView self cmd tv
//	| trace_n ("entering numberOfRowsInTableView "+++toString tv) False = undef
	= accUnsafe callback
where
	callback :: !*World -> (!Int,!*World)
	callback env
		= (size lines,env)

imp_numberOfRowsInTableView :: IMP
imp_numberOfRowsInTableView = code {
		pushLc 	numberOfRowsInTableView
	}

foreign export myCBHandler

tableView_objectValueForTableColumn_row self cmd tv col row
//	| False	//True
//		= p2ns (listItems!!row)

//	===> THIS IS THE ONE TO UPDATE FOR TIME PROFILE!!
//	headerline :: [String]
//	readProfileLines :: !String !*World -> (![[String]],!*World)
//	so _do_ need to which column is which now
//	-> look by identifier

	#!	env				= newWorld
		(colidp,env)	= msgI_P col "identifier\0" env
		colid			= ns2cls colidp

//	|	trace_n ("tableView_objectValueForTableColumn_row"+++"\t"+++colid+++"\t"+++toString row) False = undef
	#!
		colidx			= hd [ x \\ id <- headerline & x <- [0..] | id == colid ]
		therow			= lines.[row]
		elem			= therow!!colidx
	= p2ns elem

profile =: "/Users/dvanarkelmaccom/Documents/CleanLab/hex Time Profile.pcl"
lines :: {[String]}
lines =: {elm \\ elm <- fst (readProfileLines profile newWorld)}


myCBHandler :: !Pointer !Pointer !Pointer !Pointer -> Int
myCBHandler cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(tv,env)	= dcbArgPointer args_ env
		(col,env)	= dcbArgPointer args_ env
		(row,env)	= dcbArgInt args_ env
		result		= tableView_objectValueForTableColumn_row self cmd tv col row
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'i')

addrMyCBHandler :: !Int
addrMyCBHandler = code {
		pushLc 	myCBHandler
	.d 0 1 i
		rtn
	}

exportedCBHandler :: Pointer
exportedCBHandler
	= dcbNewCallback "iiiii)i\0" addrMyCBHandler 0

// model implementation

import Clyde.timeprofile
// headerLine
// readProfileLines :: !String !*World -> (![String],!*World)
// * create columns for headerline..
// * create entries for profilelines...

/*
from PmProject		import :: Project(..), ReadProjectFile
from PmPath			import :: Pathname, :: Modulename, GetModuleName
import UtilStrictLists

listItems =: accUnsafe (accFiles readProject)

readProject :: !*Files -> ([String],!*Files)
readProject files
	#!	path						= "/Users/dvanarkelmaccom/Documents/CleanLab/Clyde.prj"
		appd						= "/usr/local/Cellar/clean-itasks/20151022"
		((proj,succ,errmsg),files)	= ReadProjectFile path appd files
	#	earlyout	= False
//	#!	mods						= PR_GetModuleStuff proj
//	| trace_n ("Modules#: " +++ toString (LLength mods)) False = undef
//	#!	earlyout					= traceMods` mods True
		
		items						= ffi_items proj

//	| trace_n ("Items#: " +++ toString (length items)) False = undef
//	#!	earlyout = traceItems items earlyout
//		earlyout					= force True earlyout
	= (items,files)
where
	traceMods [|] env	= env
	traceMods [|h:t] env
		| trace_n (h.mdn_dir +++ "\t" +++ h.mdn_name) False = undef
		= traceMods t env

	traceMods` [|] env	= env
	traceMods` [|(m1,d1,m2,d2):t] env
		| trace_n (m1 +++ "\t" +++ d1 +++ "\t" +++ m2 +++ "\t" +++ d2) False = undef
		= traceMods` t env
		
	traceItems [] env = env
	traceItems [h:t] env
		| trace_n h False = undef
		= traceItems t env

/*
* relative paths give issues in app...
* what to do with new hierarchical modules??
*/
ffi_items proj
	#!	prjPath						= "./."
		prjPath						= "/Users/dvanarkelmaccom/Documents/CleanLab"
		appPath						= "/usr/local/Cellar/clean-itasks/20151022"
		ppaths						= StrictListToList (PR_GetPaths proj)
		epaths						= ["/usr/local/Cellar/clean-itasks/20151022/lib/StdEnv"]		// get from IDEEnvs...
		srcpaths					= ppaths ++ epaths // list of project search paths ++ environment search paths
		srcpaths					= ListToStrictList srcpaths
		srcpaths					= Map (\d -> (True,d)) srcpaths		// fold/unfold status... doesn't work with new hierarchical modules as we want to fold/unfold within hierarchy as well!
		modules						= PR_GetModuleStuff proj
	= items srcpaths appPath prjPath modules //shift
where
	items srcpaths appPath prjPath modules //shift
		# mods			= StrictListToList modules
		| isEmpty mods
			= []
		# [(root,rootdir,_,_):mods]	= mods
		# mods					= filter isInPaths mods
		# mods					= sortBy (\(a,b,_,_) (c,d,_,_) -> less a b c d) mods
		# moditems				= makenice True "" mods
		# rootitem				= (GetModuleName root)		//, open_imp rootdir (MakeImpPathname root), openif rootdir root)
		= [rootitem:moditems]
	where
		isInPaths (_,p,_,_) = any p srcpaths
		where
			any p [|]
				= False
			any p [|(_,b) : tl]
				= p == b || any p tl

		less a b c d
			| before b d = True		// use < -ordening of searchpaths...
			| b == d
				= a < c
			= False

		before x y = bf srcpaths
		where
			bf [|] = False
			bf [|(_,p):r]
				| p == y = False
				| p == x = True
				= bf r

		makenice _ _ [] = []
		makenice u s l=:[(imp_mod_name_with_ext,b,_,_):r]
			| s <> b	// new directory
				# u`			= isUnfoldedDir b srcpaths
				  dir			= symPath appPath prjPath b
				  pw_separator	= if u`
				  					("\\\\--- "+++dir)		//, pm_update_project_window_interactive o updFstate b False,id)
				  					("//--- "+++dir)		//, pm_update_project_window_interactive o updFstate b True,id)
				= [pw_separator : makenice u` b l]
			| u
				= [(GetModuleName imp_mod_name_with_ext)	//,f imp_mod_name_with_ext, f` imp_mod_name_with_ext)
				  :makenice u s r]	// add seperators...
				= makenice u s r
		where
			isUnfoldedDir d _ = True	// default to unfold until we have fold-state memoisation in place again
			isUnfoldedDir d [|] = False
			isUnfoldedDir d [|(u,d`):ds]
				| d == d` = u
				= isUnfoldedDir d ds
*/
///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x


///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

