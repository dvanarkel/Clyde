implementation module Cocoa.viewcontroller

import StdEnv
import Cocoa.objc
import Cocoa.msg
import Cocoa.Foundation

:: NSViewController			=: NSViewController Int

// - (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil;

initWithNibNameBundle :: !NSViewController !String !NSBundle !*env -> (!NSViewController,!*env)
initWithNibNameBundle (NSViewController ctrl) windowNibName (NSBundle bundle) env
	#	(v,env)	= msgIPP_P ctrl "initWithNibName:bundle:\0" (p2ns windowNibName) bundle env
	= (NSViewController v,env)

// - (void)loadView;

loadView :: !NSViewController !*env -> *env
loadView (NSViewController ctrl) env
	= msgI_V ctrl "loadView\0" env

// @property(strong) id representedObject;
// @property(strong, readonly) NSBundle *nibBundle;
// @property(copy, readonly) NSString *nibName;
// @property(strong) NSView *view;
// @property(copy) NSString *title;
// - (void)commitEditingWithDelegate:(id)delegate didCommitSelector:(SEL)didCommitSelector contextInfo:(void *)contextInfo;
// - (BOOL)commitEditing;
// - (void)discardEditing;
// @property(readonly, strong) NSStoryboard *storyboard;
// - (IBAction)dismissController:(id)sender;
// - (void)viewDidLoad;
// @property(readonly, getter=isViewLoaded) BOOL viewLoaded;
// - (void)viewWillAppear;
// - (void)viewDidAppear;
// - (void)viewWillDisappear;
// - (void)viewDidDisappear;
// @property NSSize preferredContentSize;
// - (void)updateViewConstraints;
// - (void)viewWillLayout;
// - (void)viewDidLayout;
// - (void)addChildViewController:(NSViewController *)childViewController;
// @property(copy) NSArray<__kindof NSViewController *> *childViewControllers;
// - (void)transitionFromViewController:(NSViewController *)fromViewController 
//                     toViewController:(NSViewController *)toViewController 
//                              options:(NSViewControllerTransitionOptions)options 
//                    completionHandler:(void (^)(void))completion;
// - (void)insertChildViewController:(NSViewController *)childViewController atIndex:(NSInteger)index;
// - (void)removeChildViewControllerAtIndex:(NSInteger)index;
// - (void)removeFromParentViewController;
// - (void)preferredContentSizeDidChangeForViewController:(NSViewController *)viewController;
// - (void)presentViewController:(NSViewController *)viewController animator:(id<NSViewControllerPresentationAnimator>)animator;
// - (void)dismissViewController:(NSViewController *)viewController;
// - (void)presentViewController:(NSViewController *)viewController 
//       asPopoverRelativeToRect:(NSRect)positioningRect 
//                        ofView:(NSView *)positioningView 
//                 preferredEdge:(NSRectEdge)preferredEdge 
//                      behavior:(NSPopoverBehavior)behavior;
// - (void)presentViewControllerAsModalWindow:(NSViewController *)viewController;
// - (void)presentViewControllerAsSheet:(NSViewController *)viewController;
// @property(readonly) NSViewController *parentViewController;
// @property(readonly) NSArray<__kindof NSViewController *> *presentedViewControllers;
// @property(readonly, assign) NSViewController *presentingViewController;

:: NSViewControllerTransitionOptions =: NSViewControllerTransitionOptions Int
NSViewControllerTransitionNone					= NSViewControllerTransitionOptions 0x0		// No animation (the default)
NSViewControllerTransitionCrossfade				= NSViewControllerTransitionOptions 0x1		// Fades the new view in and simultaneously fades the old view out. Can be combined with any of the slide options.
NSViewControllerTransitionSlideUp				= NSViewControllerTransitionOptions 0x10	// Slides the old view up while the new view comes into view from the bottom.
NSViewControllerTransitionSlideDown				= NSViewControllerTransitionOptions 0x20	// Slides the old view down while the new view comes into view from the top.
NSViewControllerTransitionSlideLeft				= NSViewControllerTransitionOptions 0x40	// Slides the old view to the left while the new view comes into view from the right.
NSViewControllerTransitionSlideRight			= NSViewControllerTransitionOptions 0x80	// Slides the old view to the right while the new view comes into view from the left.
NSViewControllerTransitionSlideForward			= NSViewControllerTransitionOptions 0x140	// Slide left/right as appropriate for UI layout direction
NSViewControllerTransitionSlideBackward			= NSViewControllerTransitionOptions 0x180	// Slide left/right as appropriate for UI layout direction
NSViewControllerTransitionAllowUserInteraction	= NSViewControllerTransitionOptions 0x1000	// Allow user interaction during the transition

// - (instancetype)initWithCoder:(NSCoder *)coder;

initWithCoder :: !NSViewController !NSCoder !*env -> (!NSViewController,!*env)
initWithCoder (NSViewController ctrl) (NSCoder coder) env
	#	(r,env)	= msgIP_P ctrl "initWithCoder:\0" coder env
	= (NSViewController r,env)

// @property(strong) IBOutlet NSView *sourceItemView;
// - (void)presentViewControllerInWidget:(NSViewController *)viewController;
