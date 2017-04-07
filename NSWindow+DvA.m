#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#import <objc/runtime.h>
#import "NSTextView+JSDExtensions.h"
//cascadePoint [window cascadeTopLeftFromPoint:cascadePoint]
NSPoint lastPoint;

void doCascade( NSWindow *window)
{
    if(lastPoint.x == 0.0 && lastPoint.y == 0.0) {
		NSRect screenFrame	= [[NSScreen mainScreen] visibleFrame];
		lastPoint.x = NSMinX(screenFrame);
		lastPoint.y = NSMaxY(screenFrame);
//		[window setFrameOrigin:lastPoint];
    }
//    else
//    {
    lastPoint = [window cascadeTopLeftFromPoint:lastPoint];
//	}
}
/*
void cascadeWindows()
{
  int i;
  NSRect screenFrame = [[NSScreen mainScreen] visibleFrame];
  NSApplication *app	= [NSApplication sharedApplication];
  lastPoint = NSMakePoint( NSMinX(screenFrame), NSMaxY(screenFrame) );
      
  for (i = 0; i < num_windows; i++) 
    {
      if (window[i])
        {        
          lastPoint = [window[i] cascadeTopLeftFromPoint:lastPoint];
          [window[i] makeKeyAndOrderFront:app];
        }
    }
}
*/

@interface CustomModalWindowController:NSWindowController
{
  
}

@end


@interface NSTextView (DvAExtensions)

//@property (strong) GoToLineViewController *viewController;
@property (strong) NSWindowController *myCustomModalWindowController;
//- (void) gotoLocation;

@end

static char const * const DvAtagLine = "DvAtagLine";

@implementation NSTextView (DvAExtensions)

- (NSWindowController*)myCustomModalWindowController
{
  NSWindowController *item = objc_getAssociatedObject(self, DvAtagLine);

  return item;
/*  if (item != nil)
  {
    return [item integerValue];

  }
  else
  {
    return 0;
  }
*/
}

- (void)setMyCustomModalWindowController:(NSWindowController *)line
{
//objc_setAssociatedObject(self, DvAtagLine, @(line), OBJC_ASSOCIATION_COPY_NONATOMIC);
  objc_setAssociatedObject(self, DvAtagLine, line, OBJC_ASSOCIATION_RETAIN);
}

/*
/// show Go To sheet
- (IBAction) gotoLocation:(id)sender {
  
  self.viewController = [ GoToLineViewController textView: self]
  
  [self.window beginSheet:self.myCustomModalWindowController.window  completionHandler:^(NSModalResponse returnCode) {
     
  }];

}
*/
- (IBAction)didTapOpenButton:(id)sender {
   NSLog(@"Goto line menu...");
//   self.myCustomModalWindowController = [[CustomModalWindowController alloc] initWithWindowNibName:@"GoToLineView"];

//  NSView *view = [[NSView alloc] initWithFrame:NSMakeRect(100, 100, 300, 300)];
  NSView *view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 200, 100)];
  NSRect rect;
  NSButton *button = [[NSButton alloc] initWithFrame:rect];
  [button setButtonType:NSMomentaryPushInButton];
  [button setBezelStyle:NSRoundedBezelStyle];
  [button setTitle:@"Okay"];
  [button setAction:@selector (ok:)];
  [button setKeyEquivalent:@"\r"];
  [button sizeToFit];
  NSPoint point;
  point.x = 10; point.y = 10;
  [button setFrameOrigin:point];  
  [view addSubview:button];

    button = [[NSButton alloc] initWithFrame:rect];
    [button setButtonType:NSMomentaryPushInButton];
    [button setBezelStyle:NSRoundedBezelStyle];
    [button setTitle:@"Cancel"];
    [button setAction:@selector (cancel:)];
    [button sizeToFit];
    point.x = 100; point.y = 10;
    [button setFrameOrigin:point];  
    [view addSubview:button];

    rect.size.width = 80;
    rect.size.height = 24;
     NSTextField *textfield = [[NSTextField alloc] initWithFrame:rect];
//     NSTextField *textfield = [[NSTextField alloc] labelWithString:@"Line No:"];
      point.x = 10; point.y = 50;
      [textfield setFrameOrigin:point]; 
      [textfield setStringValue:@"Line No:"];
      [textfield setBezeled:FALSE];
     [textfield setEditable:FALSE];
     [textfield setDrawsBackground:FALSE];
      [view addSubview:textfield];

      textfield = [[NSTextField alloc] initWithFrame:rect];
        point.x = 100; point.y = 50;
        [textfield setFrameOrigin:point]; 
//        [textfield setAutomaticTextCompletionEnabled:FALSE]; 
        [view addSubview:textfield];

//  NSViewController *vc = [[NSViewController alloc] initWithNibName:@"GoToLineView" bundle:[NSBundle mainBundle]];
//  NSView *view = [vc view];
//  [view addSubview:[[NSTextField alloc] ini]
  NSWindow *windowSheet = [[NSWindow alloc] initWithContentRect:[view frame] styleMask:NSTitledWindowMask backing:NSBackingStoreBuffered defer:YES];
  [windowSheet setContentView:view];
  self.myCustomModalWindowController = [[CustomModalWindowController alloc] initWithWindow:windowSheet];
 

  [self.window beginSheet:self.myCustomModalWindowController.window  completionHandler:^(NSModalResponse returnCode) {
     
    NSLog(@"Sheet closed %@", [textfield stringValue]);
    
    switch (returnCode) {
      case NSModalResponseOK:
        NSLog(@"Done button tapped in Custom Sheet");
        [self selectLineToVisible:[textfield integerValue]];
        break;
      case NSModalResponseCancel:
        NSLog(@"Cancel button tapped in Custom Sheet");
        break;
        
      default:
        break;
    }
    
    self.myCustomModalWindowController = nil;
    
  }];

  
}

@end


@implementation CustomModalWindowController

- (IBAction)cancel:(id)sender {
   [self.window.sheetParent endSheet:self.window returnCode:NSModalResponseCancel];
}

- (IBAction)ok:(id)sender {
  [self.window.sheetParent endSheet:self.window returnCode:NSModalResponseOK];
}

@end

// NSWindow+DvA

NSString *SaveDocToolbarItemIdentifier =@"SaveDocToolbarItemIdentifier";
NSString *SearchDocToolbarItemIdentifier = @"SearchDocToolbarItemIdentifier";

@interface NSWindow (DvAExtensions)


@end

@implementation NSWindow (DvAExtensions)

- (NSArray *) toolbarAllowedItemIdentifiers: (NSToolbar *) toolbar {
    return [NSArray arrayWithObjects: SaveDocToolbarItemIdentifier,
            NSToolbarPrintItemIdentifier,
            NSToolbarShowColorsItemIdentifier,
            NSToolbarShowFontsItemIdentifier,
            NSToolbarCustomizeToolbarItemIdentifier,
            NSToolbarFlexibleSpaceItemIdentifier,
            NSToolbarSpaceItemIdentifier,
            NSToolbarSeparatorItemIdentifier, nil];
}

- (NSArray *) toolbarDefaultItemIdentifiers: (NSToolbar *) toolbar {
    return [NSArray arrayWithObjects: SaveDocToolbarItemIdentifier,
            NSToolbarPrintItemIdentifier,
            NSToolbarSeparatorItemIdentifier,
            NSToolbarShowColorsItemIdentifier,
            NSToolbarShowFontsItemIdentifier,
            NSToolbarFlexibleSpaceItemIdentifier,
            NSToolbarSpaceItemIdentifier,
            SearchDocToolbarItemIdentifier, nil];
}

- (NSToolbarItem *) toolbar:(NSToolbar *)toolbar
      itemForItemIdentifier:(NSString *)itemIdentifier
  willBeInsertedIntoToolbar:(BOOL)flag
{
    NSToolbarItem *toolbarItem = [[NSToolbarItem alloc] initWithItemIdentifier: itemIdentifier];
    
    if ([itemIdentifier isEqual: SaveDocToolbarItemIdentifier]) {
        // Set the text label to be displayed in the
        // toolbar and customization palette
        [toolbarItem setLabel:@"Save"];
        [toolbarItem setPaletteLabel:@"Save"];
        
        // Set up a reasonable tooltip, and image
        // you will likely want to localize many of the item's properties
        [toolbarItem setToolTip:@"Save Your Document"];
        [toolbarItem setImage:[NSImage imageNamed:@"SaveDocumentItemImage"]];
        
        // Tell the item what message to send when it is clicked
        [toolbarItem setTarget:self];
        [toolbarItem setAction:@selector(saveDocument:)];
    } else  {
        // itemIdentifier referred to a toolbar item that is not
        // provided or supported by us or Cocoa
        // Returning nil will inform the toolbar
        // that this kind of item is not supported
        toolbarItem = nil;
    }
    return toolbarItem;
}
@end
