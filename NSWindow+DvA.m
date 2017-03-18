#import <Cocoa/Cocoa.h>

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