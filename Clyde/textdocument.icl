implementation module Clyde.textdocument

import StdEnv
import StdDebug
import System._Posix
import System._Pointer
import Cocoa.objc
import Cocoa.msg
import Cocoa.dyncall
import Cocoa.Foundation
import Clyde.textwindowcontroller
import Clyde.coloured_line

createTextDocument :: !*a -> *a
createTextDocument env
	| trace_n ("createTextDocument") False = undef
	#!	(cls,env)		= objc_getClass "NSDocument\0" env
		(adc,env)		= objc_allocateClassPair cls "TextDocument\0" 0 env

	#!	(sel,env)		= sel_getUid "readFromURL:ofType:error:\0" env
	| trace_n ("createTextDocument, selector = "+++toString sel) False = undef
//	#!	(ok,env)		= class_addMethod adc sel imp_readFromURL_ofType_error "i@:@@@\0" env
	#!	(ok,env)		= class_addMethod adc sel exportedCBHandler2 "i@:@@@\0" env
	| trace_n ("createTextDocument, added = "+++toString ok) False = undef

	#!	(sel,env)		= sel_getUid "writeToURL:ofType:error:\0" env
	| trace_n ("createTextDocument, selector = "+++toString sel) False = undef
//	#!	(ok,env)		= class_addMethod adc sel imp_writeToURL_ofType_error "i@:@@@\0" env
	#!	(ok,env)		= class_addMethod adc sel exportedCBHandler3 "i@:@@@\0" env
	| trace_n ("createTextDocument, added = "+++toString ok) False = undef

	#!	(sel,env)		= sel_getUid "init\0" env
	| trace_n ("createTextDocument, selector = "+++toString sel) False = undef
	#!	(ok,env)		= class_addMethod adc sel imp_init "@@:\0" env
	| trace_n ("createTextDocument, added = "+++toString ok) False = undef

	#!	(sel,env)		= sel_getUid "makeWindowControllers\0" env
	| trace_n ("createTextDocument, selector = "+++toString sel) False = undef
	#!	(ok,env)		= class_addMethod adc sel imp_makeWindowControllers "i@:@\0" env
	| trace_n ("createTextDocument, added = "+++toString ok) False = undef

	#!	(sel,env)		= sel_getUid "_shouldShowAutosaveButtonForWindow\0" env
	#!	(ok,env)		= class_addMethod adc sel imp_shouldShow "i@:@\0" env
	| trace_n ("_shouldShowAutosaveButtonForWindow, added = "+++toString ok) False = undef




// create the ivar for storing the document content..
//	#!	(ok,env)		= class_addIvar adc "contentString\0" size alignment types env
	#!	(ok,env)		= class_addIvar adc "contentString\0" 8 3 "@\0" env

	#!	env				= objc_registerClassPair adc env
	| trace_n ("exit createTextDocument") False = undef
	= env

/*
char *texPosEncoding = @encode(UITextPosition);
NSUInteger textPosSize, textPosAlign;
NSGetSizeAndAlignment(textPosEncoding, &textPosSize, &textPosAlign);
class_addIvar(yourClass, "yourIvarName", textPosSize, textPosAlign, textPosEncoding);
*/

imp_shouldShow :: Int
imp_shouldShow = code {
		pushLc shouldShow
	}
	
foreign export shouldShow

shouldShow :: !Int !Int !Int -> Int
shouldShow self cmd window
	| trace_n ("shouldShow!") False = undef
	= 1

imp_init :: Int
imp_init = code {
		pushLc init_
	}
	
foreign export init_

class_getSuperclass :: !Class -> Class
class_getSuperclass _ = code {
		ccall class_getSuperclass "p:p"
	}

init_ :: !Int !Int -> Int
init_ self cmd
	| trace_n ("entering TextDocument.init") False = undef
	| trace_n ("self class: "+++object_getClassName self+++"\t"+++toString self) False = undef
//	#!	(sel,_)	= sel_getUid "init\0" newWorld
//	= objc_msgSendSuper (class_getSuperclass self) sel
//	= objc_msgSendSuper self sel
	#!	cls	= fst (objc_getClass "TextDocument\0" newWorld)
		ptr	= malloc 16
		ptr	= writeInt ptr 0 self
		ptr	= writeInt ptr 8 cls
//	= fst (msgS_P self cmd newWorld)
		(ret,world)		= msgS_P ptr cmd newWorld
	| ret == 0
		= ret
//	#!	world			= msgI_V self "makeWindowControllers\0" world
	#!	nsstring		= p2ns ""
	#!	(_,world)		= object_setInstanceVariable self "contentString\0" nsstring world
	#!	(str`,world)	= object_getInstanceVariable self "contentString\0" world
	| trace_n ("set contentString: "+++toString nsstring+++"\t"+++toString str`) False = undef
	#!	world			= msgII_V self "_setShowAutosaveButton:\0" 1 world
	= force world ret

// readFromURL_ofType_error

//dynCallback cb_ args_ result_ userdata_
// so what is in cb_? and in userdata_??

////////////

foreign export myCBHandler2

myCBHandler2 :: !Pointer !Pointer !Pointer !Pointer -> Int
myCBHandler2 cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(tv,env)	= dcbArgPointer args_ env
		(col,env)	= dcbArgPointer args_ env
		(row,env)	= dcbArgInt args_ env
		result		= readFromURL_ofType_error self cmd tv col row
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'i')

addrMyCBHandler2 :: Int
addrMyCBHandler2 = code {
		pushLc 	myCBHandler2
	.d 0 1 i
		rtn
	}

exportedCBHandler2 :: Pointer
exportedCBHandler2
	= dcbNewCallback "iiiii)i\0" addrMyCBHandler2 0

////////////

foreign export myCBHandler3

myCBHandler3 :: !Pointer !Pointer !Pointer !Pointer -> Int
myCBHandler3 cb_ args_ result_ userdata_
	#!	env			= newWorld
		(self,env)	= dcbArgPointer args_ env
		(cmd,env)	= dcbArgPointer args_ env
		(tv,env)	= dcbArgPointer args_ env
		(col,env)	= dcbArgPointer args_ env
		(row,env)	= dcbArgInt args_ env
		result		= writeToURL_ofType_error self cmd tv col row
		result_		= writeInt result_ 0  result
	| result_ <> result_ = undef
	= force env (toInt 'i')

addrMyCBHandler3 :: Int
addrMyCBHandler3 = code {
		pushLc 	myCBHandler3
	.d 0 1 i
		rtn
	}

exportedCBHandler3 :: Pointer
exportedCBHandler3
	= dcbNewCallback "iiiii)i\0" addrMyCBHandler3 0


//foreign export readFromURL_ofType_error

imp_readFromURL_ofType_error :: Int
imp_readFromURL_ofType_error = code {
		pushLc readFromURL_ofType_error
	}

readFromURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
readFromURL_ofType_error self cmd absoluteURL typeName outError
	| trace_n ("readFromURL_ofType_error called") False = undef
	| trace_n ("self class: "+++object_getClassName self+++"\t"+++toString self) False = undef
	#!	env				= newWorld
		(fileB,env)		= msgI_I absoluteURL "isFileURL\0" env
		(pathN,env)		= msgI_P absoluteURL "path\0" env
	| trace_n ("url is file: "+++toString fileB) False = undef
	| trace_n ("path: '"+++ ns2cls pathN+++"'") False = undef
	| trace_n ("type: '"+++ ns2cls typeName +++ "'") False = undef
	#!	errorHdl		= malloc 8	// do we also need to alloc the Ptr??
		errorHdl		= writeInt errorHdl 0 0
		(str,env)		= msgCPPP_P "NSString\0" "stringWithContentsOfURL:encoding:error:\0" absoluteURL NSUTF8StringEncoding errorHdl env
		(error,env)		= getError errorHdl env

/*
		string			= ns2cls str
	| trace_n ("Error: '"+++error+++"'") False = undef
//	| trace_n ("String: '"+++string+++"'") False = undef
	| trace_n ("self class: "+++object_getClassName self) False = undef
	#!	(wind,env)		= msgI_P self "windowForSheet\0" env
	| trace_n ("window class: '"+++object_getClassName wind+++"'\t"+++toString wind) False = undef
	#!	(cont,env)		= msgI_P wind "contentView\0" env
	| trace_n ("content class: '"+++object_getClassName cont+++"'\t"+++toString cont) False = undef
	#!	(docv,env)		= msgI_P cont "documentView\0" env
	| trace_n ("document class: '"+++object_getClassName docv+++"'\t"+++toString docv) False = undef
	#!	env				= msgIP_V docv "setString:\0" str env
//	| True
//		= abort ("\nstopping...\n\n"+++toString wind+++"\n\n"+++toString cont+++"\n\n")
	#! (nss,env)		= msgI_P docv "string\0" env
	| trace_n ("textv string length "+++toString (size (ns2cls nss))) False = undef
	// so we need to know the textview in order to set its contents...
*/
	#!	(_,env)		= object_setInstanceVariable self "contentString\0" str env			// HUH??? Doesn't appear to actually work?!
		env			= msgI_V str "retain\0" env
	#!	(str`,env)			= object_getInstanceVariable self "contentString\0" env
// need to release previous one if present...
	| trace_n ("set contentString to: "+++toString str+++"\t"+++toString str`) False = undef
	= YES

getError errorHdl env
	#!	errorObj	= readInt errorHdl 0
	| errorObj == 0
		= ("",env)
	#!	(err,env)	= msgI_P errorObj "localizedDescription\0" env
	= (ns2cls err,env)


//foreign export writeToURL_ofType_error

imp_writeToURL_ofType_error :: Int
imp_writeToURL_ofType_error = code {
		pushLc writeToURL_ofType_error
	}

writeToURL_ofType_error :: !Int !Int !Int !Int !Int -> Int
writeToURL_ofType_error self cmd absoluteURL typeName outError
	| trace_n ("writeToURL_ofType_error called") False = undef
	| trace_n ("self class: "+++object_getClassName self) False = undef
	#!	env				= newWorld
		(fileB,env)		= msgI_I absoluteURL "isFileURL\0" env
		(pathN,env)		= msgI_P absoluteURL "path\0" env
	| trace_n ("url is file: "+++toString fileB) False = undef
	| trace_n ("path: '"+++ ns2cls pathN+++"'") False = undef
	| trace_n ("type: '"+++ ns2cls typeName +++ "'") False = undef

	#!	(wind,env)		= msgI_P self "windowForSheet\0" env
	| trace_n ("window class: '"+++object_getClassName wind+++"'\t"+++toString wind) False = undef
	#!	(cont,env)		= msgI_P wind "contentView\0" env
	| trace_n ("content class: '"+++object_getClassName cont+++"'\t"+++toString cont) False = undef
	#!	(docv,env)		= msgI_P cont "documentView\0" env
	| trace_n ("document class: '"+++object_getClassName docv+++"'\t"+++toString docv) False = undef
	#! (nss,env)		= msgI_P docv "string\0" env
	| trace_n ("textv string length "+++toString (size (ns2cls nss))) False = undef

	#!	//errorHdl		= malloc 8	// do we also need to alloc the Ptr??	// no... but need to malloc sizeof NSError?!
		errorHdl		= malloc 40
		errorHdl		= writeInt errorHdl 0 0
//		(str,env)		= msgCPPP_P "NSString\0" "stringWithContentsOfURL:encoding:error:\0" absoluteURL NSUTF8StringEncoding errorHdl env		
		(ok,env)		= msgIPPPP_I nss "writeToURL:atomically:encoding:error:\0" absoluteURL YES NSUTF8StringEncoding errorHdl env
		
		(error,env)		= getError errorHdl env

		string			= ns2cls nss
	| trace_n ("Ok: '"+++toString ok+++"'") False = undef
	| trace_n ("Error: '"+++error+++"'") False = undef
//	| trace_n ("String: '"+++string+++"'") False = undef

	= YES

	

//- (BOOL)readFromURL:(NSURL *)absoluteURL
//             ofType:(NSString *)typeName
//              error:(NSError * _Nullable *)outError

/*
outlineViewObjectValueForTableColumnByItem :: !Int !Int !Int !Int !Int -> Int
outlineViewObjectValueForTableColumnByItem self cmd ov column item


		path			= "/Users/dvanarkelmaccom/Documents/CleanLab/Clyde.icl\0"
		(string,env)	= msgCPPP_P "NSString\0" "stringWithContentsOfFile:usedEncoding:error:\0" (c2ns path) encodingPtr errorHdl env

		env			= msgIP_V dc "noteNewRecentDocumentURL:\0" url env
*/

/*
- (void)makeWindowControllers
Description	
Subclasses may override this method to create the initial window controller(s) for the 
document.
The base class implementation creates an NSWindowController object with windowNibName and 
with the document as the file’s owner if windowNibName returns a name. If you override 
this method to create your own window controllers, be sure to use addWindowController: to 
add them to the document after creating them.
This method is called by the NSDocumentController open... methods, but you might want to 
call it directly in some circumstances.
*/

foreign export makeWindowControllers

imp_makeWindowControllers :: Int
imp_makeWindowControllers = code {
		pushLc makeWindowControllers
	}

makeWindowControllers :: !Int !Int -> Int
makeWindowControllers self cmd
	| trace_n ("makeWindowControllers called") False = undef
	| trace_n ("self class: "+++object_getClassName self+++"\t"+++toString self) False = undef
	#!	world				= newWorld


		(delegate,world)	= msgI_P application "delegate\0" world
		(nstype,world)		= msgI_P self "fileType\0" world

		(wind,world)		= populateTextWindow delegate (ns2cls nstype) world
//		world				= msgIP_V self "setWindow:\0" wind world
		(wctrl,world)		= msgC_P "NSWindowController\0" "alloc\0" world
		world				= msgIB_V wctrl "setShouldCascadeWindows:\0" True world
		(wctrl,world)		= msgIP_P wctrl "initWithWindow:\0" wind world
		world				= msgIB_V wctrl "setShouldCascadeWindows:\0" True world
// need to actually generate a window controller...

	#!	(str,world)			= object_getInstanceVariable self "contentString\0" world
		string				= ns2cls str
//	| trace_n ("contentString:\t"+++toString str+++"\n"+++string) False = undef
	| trace_n ("window class: '"+++object_getClassName wind+++"'\t"+++toString wind) False = undef
	#!	(cont,world)		= msgI_P wind "contentView\0" world
	| trace_n ("content class: '"+++object_getClassName cont+++"'\t"+++toString cont) False = undef
	#!	(docv,world)		= msgI_P cont "documentView\0" world
	| trace_n ("document class: '"+++object_getClassName docv+++"'\t"+++toString docv) False = undef
	#!	world				= msgIP_V docv "setString:\0" str world
	#! (nss,world)			= msgI_P docv "string\0" world
	| trace_n ("textv string length "+++toString (size (ns2cls nss))) False = undef

	#!	world				= msgIP_V self "addWindowController:\0" wctrl world

	#!	world				= msgII_V self "_setShowAutosaveButton:\0" 1 world
//		(url,env)		= msgCP_P "NSURL\0" "fileURLWithPath:\0" (p2ns "/Users/dvanarkelmaccom/Documents/CleanLab/icfp2015/hex.icl") env
//		env				= msgIP_V wind "setRepresentedURL:\0" url env
		env				= world
		view			= cont
		(vbut,env)		= msgCII_P "NSWindow\0" "standardWindowButton:forStyleMask:\0" NSWindowDocumentVersionsButton 0 env
		(titb,env)		= msgI_P view "superview\0" env
		(tcont,env)		= findView "NSTitlebarContainerView" titb env
		(ttitb,env)		= findView "NSTitlebarView" tcont env
		(ttext,env)		= findView "NSTextField" ttitb env
		(bounds,env)	= getBounds ttext env
		origin_x		= readReal8 bounds 0
		origin_y		= readReal8 bounds 8
		size_w			= readReal8 bounds 16
		size_h			= readReal8 bounds 24
//		env				= msgIRR_V vbut "setFrameOrigin:" (origin_x + size_w) origin_y env
		env				= msgIRR_V vbut "setFrameOrigin:" (459.0 + 124.0) 3.0 env
// fiddling with frame...
//		env				= msgIP_V titb "_addKnownSubview:\0" vbut env
		env				= msgIP_V ttitb "addSubview:\0" vbut env

		(act,env)		= msgI_P vbut "action\0" env
		env = trace_n ("action\t"+++toString act +++"\t"+++ sel_getName act) env
		world			= env
	= force world 42

NSWindowDocumentVersionsButton = 6

findView name view env
	#!	(subs,env)		= msgI_P view "subviews\0" env
		(count,env)		= msgI_I subs "count\0" env
	= logsubs 0 count subs env
where
	logsubs :: !Int !Int !Pointer !*a -> (!Pointer,!*a)
	logsubs index count subs env
		| index >= count
			= (0,env)
		#!	(view,env)		= msgII_P subs "objectAtIndex:\0" index env
		| object_getClassName view == name
			= (view,env)
//		#!	env				= logviews (inc nest) view env
		= logsubs (inc index) count subs env

// capture standard editor settings here...
textParagraphStyle env
//	alignment	= NSLeftTextAlignment
// set tabStops
// set lineBreakMode	= NSLineBreakByTruncatingTail (or NSLineBreakByClipping)
	= env
// backgroundColour at document level?


/* SYNTAX COLOURING...
You should add your controller as the delegate of the NSTextStorage object of the 
NSTextView ([textView textStorage]) and then implement the delegate method 
‑textStorageDidProcessEditing:. This is called whenever the text changes.

In the delegate method you need to get the current NSTextStorage object from the text 
view using the -textStorage method of NSTextView. NSTextStorage is a subclass of 
NSAttributedString and contains the attributed contents of the view.

Your code must then parse the string and apply coloring to whatever ranges of text are 
interesting to you. You apply color to a range using something like this, which will 
apply a yellow color to the whole string:

//get the range of the entire run of text
NSRange area = NSMakeRange(0, [textStorage length]);

//remove existing coloring
[textStorage removeAttribute:NSForegroundColorAttributeName range:area];

//add new coloring
[textStorage addAttribute:NSForegroundColorAttributeName 
                    value:[NSColor yellowColor] 
                    range:area];

LARGER EXAMPLE:

- (void)textStorageDidProcessEditing:(NSNotification *)notification {
    NSTextStorage *textStorage = notification.object;
    NSString *string = textStorage.string;
    NSUInteger n = string.length;
    [textStorage removeAttribute:NSForegroundColorAttributeName range:NSMakeRange(0, n)];
    for (NSUInteger i = 0; i < n; i++) {
        unichar c = [string characterAtIndex:i];
        if (c == '\\') {
            [textStorage addAttribute:NSForegroundColorAttributeName value:[NSColor lightGrayColor] range:NSMakeRange(i, 1)];
            i++;
        } else if (c == '$') {
            NSUInteger l = ((i < n - 1) && isdigit([string characterAtIndex:i+1])) ? 2 : 1;
            [textStorage addAttribute:NSForegroundColorAttributeName value:[NSColor redColor] range:NSMakeRange(i, l)];
            i++;
        }
    }
}
*/


textStorageDidProcess :: (!String,!Int,!String)
textStorageDidProcess = ("textStorageDidProcessEditing:", imp_textStorageDidProcessEditing, "v@:@\0")

foreign export textStorageDidProcessEditing

addSyncolDelegate :: !Int !*a -> *a
addSyncolDelegate adc env
	#!	(sel,env)		= sel_getUid "textStorageDidProcessEditing:\0" env
	| trace_n ("createTextDocument, selector = "+++toString sel) False = undef
	#!	(ok,env)		= class_addMethod adc sel imp_textStorageDidProcessEditing "v@:@\0" env
//	#!	(ok,env)		= class_addMethod adc sel imp_textStorageDidProcessEditing "i@:{s=q,l=q}\0" env
	| trace_n ("createTextDocument, added = "+++toString ok) False = undef
	= env

imp_textStorageDidProcessEditing :: Int
imp_textStorageDidProcessEditing = code {
		pushLc textStorageDidProcessEditing
	}

textStorageDidProcessEditing :: !Int !Int !Int -> Int
textStorageDidProcessEditing self cmd notification
//	| trace_n ("textStorageDidProcessEditing called") False = undef
//	| trace_n ("self class: "+++object_getClassName self) False = undef
	#!	env				= newWorld
		(storage,env)	= msgI_P notification "object\0" env

		env				= msgI_V storage "retain\0" env

		(nsstring,env)	= msgI_P storage "string\0" env

		env				= msgI_V nsstring "retain\0" env

		(n,env)			= msgI_I nsstring "length\0" env
		
//		env = trace_n ("storage class: "+++object_getClassName storage) env
//		env = trace_n ("nsstring class: "+++object_getClassName nsstring) env
		
//		env				= msgIII_V storage "removeAttribute:range:\0" 
// lie about message type..
	#!	string			= ns2cls nsstring
		(idcs,lines)	= unzip (toLines 0 0 0 0 string)
	| length lines <> length (toLines` 0 0 string)
		= abort "\ntoLines messing up linecount\n\n"
//	#!	info			= slToList (firstParse (slFromList lines))
//		astrings		= map string2attribString info
	#!	astrings		= attributeStrings lines
		env = trace_n ("string length "+++toString (size string)) env
	#!	(_,env)			= remForeground storage 0 n env
//	| trace_n ("@@@ "+++toString (n == size string)+++" @@@\t"+++toString n+++"\t"+++toString (size string)) False = undef
	| trace_n ("@2") False = undef
		
	#!	env				= setAstrings storage 1 idcs astrings string env
		env				= msgI_V nsstring "release\0" env
		env				= msgI_V storage "release\0" env
	= force env 42

// Need to convert UTF-8 range to UTF-16 range... (which we can't really know since we are unaware how NSString converts UTF-8...

toLines` s  e  string
	| e >= size string
		| e > s
			= [s]
		= []
	#!	c	= string.[e]
	| c == '\n'
		= [s : toLines` (inc e) (inc e) string]
	= toLines` s (inc e) string

/* toLines
	s	: start byte index of line
	s`	: start guessed utf-16 codeunit index
	e	: current byte index
	e`	: current guessed utf-16 codeunit index
*/
toLines s s` e e` string
	| e >= size string
		| e > s
			= [(s`,string%(s,size string - 1))]
		= []
	#!	c	= string.[e]
	| c == '\n'
		#!	line	= (s`,string%(s,e-1))
		= [line : toLines (inc e) (inc e`) (inc e) (inc e`) string]
	#!	c`	= toInt c
	| c` < 0x80
		= toLines s s` (inc e) (inc e`) string
	| c` < 0xe0	// assume no malformed utf-8
		= toLines s s` (e+2) (inc e`) string
	| c` < 0xf0
		= toLines s s` (e+3) (inc e`) string
	= toLines s s` (e+4) (e`+2) string

range_8_to_range_16 (s,e) string
	#	idcs	= idxs 0 0
		idcs	= drop s idcs
		s`		= hd idcs
		idcs	= drop (e-s) idcs
		e`		= hd idcs
	= (s`,e`)
where	// E2 88 88
	idxs i i`
		#!	c`	= toInt string.[i]
		| c` < 0x80	// single utf-8 byte -> single utf-16 word
			= [i` : idxs (inc i) (inc i`)]
		| c` < 0xe0	// double utf-8 byte -> single utf-16 word		assume no malformed utf-8
			= [i`,i` : idxs (i+2) (inc i`)]
		| c` < 0xf0	// triple utf-8 byte -> single utf-16 word
			= [i`,i`,i` : idxs (i+3) (inc i`)]
		// quad utf-8 -> double utf-16
		= [i`,i`,inc i`,inc i` : idxs (i+4) (i`+2)]

/* from RFC 3629:
UTF8-octets = *( UTF8-char )
   UTF8-char   = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
   UTF8-1      = %x00-7F
   UTF8-2      = %xC2-DF UTF8-tail
   UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
                 %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
   UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
                 %xF4 %x80-8F 2( UTF8-tail )
   UTF8-tail   = %x80-BF
*/

remForeground :: !Pointer !Int !Int !*a -> (!Int,!*a)
/*
remForeground storage location length env = code {
		ccall remForeground "GpII:I:A"
	}
*/
remForeground storage location length env	
//	#!	env				= msgIPA_V storage "removeAttribute:range:\0" NSForegroundColorAttributeName {#location,length} env
	#!	env				= msgIPII_V storage "removeAttribute:range:\0" NSForegroundColorAttributeName location length env
//	#!	(_,env)			= msgIPPP_P storage "removeAttribute:range:\0" NSForegroundColorAttributeName location length env
	= (42,env)

addForeground :: !Pointer !Int !Int !Pointer !*a -> (!Int,!*a)
addForeground storage location length colour env
//			(_,env)		= msgIPPS_P storage "addAttribute:value:range:\0" NSForegroundColorAttributeName value NSRangeType range env
	#!	env				= msgIPPII_V storage "addAttribute:value:range:\0" NSForegroundColorAttributeName colour location length env
	= (42,env)
/*
addForeground storage location length colour env = code {
		ccall addForeground "GpIIp:I:A"
	}
*/

/*
import code from "foreground.o"

foreground.m

remForeground storage location length
	[storage removeAttribute: NSForegroundColorAttributeName range: NSMakeRange location length]

addForeground storage from to colour
	[storage addAttribute: NSForegroundColorAttributeName value: colour range: NSMakeRange location length]
*/

NSRangeType :: DCStruct
NSRangeType =: makeNSRangeType
where
	makeNSRangeType :: Int
	makeNSRangeType
		#!	(rectT,env)		= dcNewStruct 2 16 newWorld
			type			= toInt 'd'
			alignment		= 8	// default alignment
			env				= dcStructField rectT type alignment 1 env
			env				= dcStructField rectT type alignment 1 env
			env				= dcCloseStruct rectT env
//			env = trace_n ("NSSizeType: "+++logDCStruct rectT) env
			fields	= readInt rectT 0
			fields	= writeInt fields 0 0 + 48
			fields	= writeInt fields 0 8 + 48
		| fields == 0 = undef
//		#!	env = trace_n ("NSRangeType: "+++logDCStruct rectT) env
		= force env rectT

setAstrings :: !Pointer !Int ![Int] ![[(Range,Style)]] !String !*a -> *a
setAstrings storage line [] [] string env	= env
setAstrings storage line [idx:idcs] [asl:asls] string env
//	| trace_n ("("+++toString line+++","+++toString idx+++")") False = undef
	#!	env	= setAstring line idx asl env
	= setAstrings storage (inc line) idcs asls string env
where
	setAstring :: !Int !Int ![(Range,Style)] !*a -> *a
	setAstring line idx [] env
		= env
	setAstring line idx [((s,e),Normal):asls] env
		= setAstring line idx asls env
	setAstring line idx [((s,e),style):asls] env
 		#!	(s`,e`)		= range_8_to_range_16 (s,e) string
		#!	(n,env)		= msgI_I storage "length\0" env
//		| trace_n (toString idx+++"\t"+++toString s+++","+++toString e+++"\t"+++toString style+++"\t"+++toString n) False = undef
//		| trace_n ("now\t"+++toString s`+++"\t"+++toString e`) False = undef
		| e` <= s`
			= setAstring line idx asls env
		#!	location	= idx + s`
			length		= e`-s`+1
			value		= style2col style
			colour		= value
//		| trace_n ("addForeground\t"+++toString line+++"\t"+++toString location+++"\t"+++toString length+++"\t"+++toString style) False = undef
		#!	(sub,env)	= debugSubstring storage location length env
//		| line rem 10 == 0 && trace_n ("'"+++sub+++"'") False = undef
		#!	(_,env)		= addForeground storage location length colour env
		= setAstring line idx asls env

//    NSString *subs = [[storage string] substringWithRange: range];

debugSubstring storage location length env
	#!	(str,env)	= msgI_P storage "string\0" env
		(sstr,env)	= msgIII_P str "substringWithRange:\0" location length env
	= (ns2cls sstr,env)



NSForegroundColorAttributeName =: p2ns "NSColor"
//NSForegroundColorAttributeName =: dlsym (-2) "NSForegroundColorAttributeName\0"

//		(col,env)		= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" 1.0 1.0 (215.0/255.0) 1.0 env

style2col :: !Style -> Pointer
style2col s
	= case s of
		Normal		-> normalColour
		Comment		-> commentColour
		Keyword		-> keywordColour
		StringLit	-> stringlitColour
		CharsLit	-> charslitColour
		Typedef		-> typedefColour
		Typedecl	-> typedeclColour

//toString :: !Style -> String
instance toString Style where
	toString s
		= case s of
			Normal		-> "Normal"
			Comment		-> "Comment"
			Keyword		-> "Keyword"
			StringLit	-> "StringLit"
			CharsLit	-> "CharsLit"
			Typedef		-> "Typedef"
			Typedecl	-> "Typedecl"

normalColour	=: getAndRetain 0.0 0.0 0.0	 1.0
commentColour	=: getAndRetain 0.0 0.0 (195.0/255.0) 1.0
keywordColour	=: getAndRetain 0.5 0.0 0.5 1.0
stringlitColour	=: getAndRetain 0.0 (155.0/255.0) 0.0 1.0
charslitColour	=: getAndRetain (155.0/255.0) 0.0 (155.0/255.0) 1.0
typedefColour	=: getAndRetain (195.0/255.0) 0.0 0.0 1.0
typedeclColour	=: getAndRetain (195.0/255.0) 0.0 0.0 1.0

getAndRetain :: !Real !Real !Real !Real -> Pointer
getAndRetain r g b a
	#!	(col,env)	= msgCRRRR_P "NSColor\0" "colorWithCalibratedRed:green:blue:alpha:\0" r g b a newWorld
		env			= msgI_V col "retain\0" env
	= force env col
	
///// setting the paragraph style...

setMyParagraphStyle :: !Int !Int !*a -> *a
setMyParagraphStyle textview myFont env
	#!	(paragraphStyle,env)	= getDefaultParagraphStyle textview env
		(cWidth,cHeight,env)	= advancementForGlyph myFont (NSGlyph ' ') env
		env						= msgIR_V paragraphStyle "setDefaultTabInterval:\0" (cWidth * 4.0) env
		(array,env)				= msgC_P "NSArray\0" "array\0" env
		env						= msgIP_V paragraphStyle "setTabStops:\0" array env
		env						= msgIP_V textview "setDefaultParagraphStyle:\0" paragraphStyle env

/*
   NSMutableDictionary* typingAttributes = [[myTextView typingAttributes] mutableCopy];
   [typingAttributes setObject:paragraphStyle forKey:NSParagraphStyleAttributeName];
   [typingAttributes setObject:scriptFont forKey:NSFontAttributeName];
   [myTextView setTypingAttributes:typingAttributes];
*/
	| trace_n ("about to set typing attributes (textdocument:setMyParagraphStyle)") False = undef
	#!	(typingAttr,env)		= msgI_P textview "typingAttributes\0" env
	| trace_n "1" False = undef	
	#!	(typingAttr,env)		= msgI_P typingAttr "mutableCopy\0" env
	| trace_n "2" False = undef	
	#!	env						= msgIPP_V typingAttr "setObject:forKey:\0" paragraphStyle (p2ns "defaultParagraphStyle") env
	| trace_n "3" False = undef	
	#!	env						= msgIPP_V typingAttr "setObject:forKey:\0" myFont (p2ns "Helvetica(Neue) 12") env
	| trace_n "4" False = undef	
	#!	env						= msgIP_V textview "setTypingAttributes:\0" typingAttr env
	| trace_n "done setting attrs" False = undef	

	#!	env						= msgI_V paragraphStyle "release\0" env
	= env

getDefaultParagraphStyle textview env
	#!	(paragraphStyle,env)	= msgI_P textview "defaultParagraphStyle\0"  env
	| paragraphStyle <> 0
		= (paragraphStyle,env)
	#!	(paragraphStyle,env)	= msgC_P "NSParagraphStyle\0" "defaultParagraphStyle\0" env
	= msgI_P paragraphStyle "mutableCopy\0" env

/*
- (void)updateMyTextViewTextAttributes
{
   NSMutableParagraphStyle* paragraphStyle = [[myTextView defaultParagraphStyle] mutableCopy];

   if (paragraphStyle == nil) {
       paragraphStyle = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
   }

   float charWidth = [[myFont screenFontWithRenderingMode:NSFontDefaultRenderingMode] advancementForGlyph:(NSGlyph) ' '].width;
   [paragraphStyle setDefaultTabInterval:(charWidth * 4)];
   [paragraphStyle setTabStops:[NSArray array]];

   [myTextView setDefaultParagraphStyle:paragraphStyle];
*** PROBABLY REQUIRED IF YOU ARE CHANIGING TABSTOP SIZE...
   NSMutableDictionary* typingAttributes = [[myTextView typingAttributes] mutableCopy];
   [typingAttributes setObject:paragraphStyle forKey:NSParagraphStyleAttributeName];
   [typingAttributes setObject:scriptFont forKey:NSFontAttributeName];
   [myTextView setTypingAttributes:typingAttributes];

   /** ADDED CODE BELOW **/
   NSRange rangeOfChange = NSMakeRange(0, [[myTextView string] length]);
   [myTextView shouldChangeTextInRange:rangeOfChange replacementString:nil];
   [[myTextView textStorage] setAttributes:typingAttributes range:rangeOfChange];
   [myTextView didChangeText];

   [paragraphStyle release];
   [typingAttributes release];
}
alt: (theoretically better than above but apparently doesn't work)
float charWidth = [myFont advancementForGlyph:(NSGlyph) ' '].width;

*/
advancementForGlyph :: !Pointer !Int !*env -> (!Real,!Real,!*env)
advancementForGlyph font glyph env
	#!	dim				= malloc 16
		dim				= writeReal8 dim 0 0.0
		dim				= writeReal8 dim 8 0.0
		(sel,env)		= sel_getUid "advancementForGlyph:\0" env
	| True
		= doCall` font sel glyph env
	#!	(ret,env)		= doCall dim font sel glyph env
	| force ret False = undef	
	#!	width			= readReal8 dim 0
		height			= readReal8 dim 8
	= (width,height,env)
where
	doCall :: !Pointer !Pointer !Pointer !Int !*a -> (!Int,!*a)
	doCall _ _ _ _ _ = code {
			ccall objc_msgSend_stret "GpppI:I:A"
		}
	doCall` :: !Pointer !Pointer !Int !*a -> (!Real,!Real,!*a)
	doCall` _ _ _ _ = code {
			ccall objc_msgSend "GppI:RR:A"
		}

NSGlyph :== toInt
:: NSFontRenderingMode :== Int

NSFontDefaultRenderingMode							:== 0
NSFontAntialiasedRenderingMode						:== 1
NSFontIntegerAdvancementsRenderingMode				:== 2
NSFontAntialiasedIntegerAdvancementsRenderingMode	:== 3
	
///// SAFE LOCAL DEFS

force :: !.a !.b -> .b
force _ x = x

///// UNSAFE LOCAL DEFS

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

