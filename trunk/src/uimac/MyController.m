/* Copyright (c) 2003, see file COPYING for details. */

#import "MyController.h"
#import "ProfileController.h"
#import "PreferencesController.h"
#import "NotificationController.h"
#import "ReconItem.h"
#import "ReconTableView.h"
#import "UnisonToolbar.h"

#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

extern value Callback_checkexn(value,value);
extern value Callback2_checkexn(value,value,value);

@implementation MyController

static MyController *me; // needed by reloadTable and displayStatus, below

- (id)init
{
    if (([super init])) {
	
        /* Initialize locals */
        me = self;
        doneFirstDiff = NO;
        newStatusText = [[NSMutableString alloc] initWithCapacity:1024];
        newProgress = 0.;
        shouldResetSelection = NO;
	
        /* Ocaml initialization */
        caml_reconItems = preconn = Val_int(0);
        caml_register_global_root(&caml_reconItems);
        caml_register_global_root(&preconn);

        /* Cross-thread notification support to ensure GUI updates 
           only happen in main thread */
        [self setupThreadingSupport];

        [[NSNotificationCenter defaultCenter] addObserver:self
            selector:@selector(processNotification:)
            name:@"statusTextNeedsUpdate" object:nil];

        [[NSNotificationCenter defaultCenter] addObserver:self
            selector:@selector(processNotification:)
            name:@"tableViewNeedsUpdate" object:nil];	

        [[NSNotificationCenter defaultCenter] addObserver:self
            selector:@selector(processNotification:)
            name:@"toolbarNeedsUpdate" object:nil];
	
        [[NSNotificationCenter defaultCenter] addObserver:self
            selector:@selector(processNotification:)
            name:@"progressBarNeedsUpdate" object:nil];

        [[NSNotificationCenter defaultCenter] addObserver:self
            selector:@selector(processNotification:)
            name:@"tableViewSelectionDidChange" object:nil];
    }
    return self;
}

- (void) setupThreadingSupport
{
    if ( notifications ) return;

    notifications      = [[NSMutableArray alloc] init];
    notificationLock   = [[NSLock alloc] init];
    notificationThread = [[NSThread currentThread] retain];

    notificationPort = [[NSMachPort alloc] init];
    [notificationPort setDelegate:self];
    [[NSRunLoop currentRunLoop] addPort:notificationPort
            forMode:(NSString *) kCFRunLoopCommonModes];
}

- (void) handleMachMessage:(void *) msg {
    [notificationLock lock];

    while ( [notifications count] ) {
        NSNotification *notification = [[notifications objectAtIndex:0] retain];
        [notifications removeObjectAtIndex:0];
        [notificationLock unlock];
        [self processNotification:notification];
        [notification release];
        [notificationLock lock];
    };

    [notificationLock unlock];
}
 
- (void) processNotification:(NSNotification *) notification
{
    /* Handle GUI update requests from subsidiary threads. 
       If we're not the main thread, forward the notification
       to it and exit */
    if( [NSThread currentThread] != notificationThread ) {
        // Forward the notification to the correct thread
        [notificationLock lock];
        [notifications addObject:notification];
        [notificationLock unlock];
        [notificationPort sendBeforeDate:[NSDate date]
            components:nil from:nil reserved:0];
    }
    else {
        if ([[notification name] isEqual:@"statusTextNeedsUpdate"]) {
            [statusText setStringValue:newStatusText];
        }
        else if ([[notification name] isEqual:@"tableViewNeedsUpdate"]) {
            [tableView reloadData]; 
            if (shouldResetSelection) {
                [tableView selectRow:0 byExtendingSelection:NO];
                shouldResetSelection = NO;
            }
            [updatesView setNeedsDisplay:YES];	    
        }
        else if ([[notification name] isEqual:@"toolbarNeedsUpdate"]) {
           [toolbar validateVisibleItems];
           [updatesView setNeedsDisplay:YES];
        }
        else if ([[notification name] isEqual:@"progressBarNeedsUpdate"]) {
            [progressBar incrementBy:(newProgress - [progressBar doubleValue])];
        }
        else if ([[notification name] isEqual:@"tableViewSelectionDidChange"]) {
            [self tableViewSelectionDidChange:notification];
        }
    }
}

- (void)awakeFromNib
{
    // Window positioning
    NSRect screenFrame = [[mainWindow screen] visibleFrame];
    [mainWindow cascadeTopLeftFromPoint:
        NSMakePoint(screenFrame.origin.x, 
        screenFrame.origin.y+screenFrame.size.height)];
    
    blankView = [[NSView alloc] init];

    /* Double clicking in the profile list will open the profile */
    [[profileController tableView] setTarget:self];
    [[profileController tableView] setDoubleAction:@selector(openButton:)];

    /* Set up the version string in the about box.  We use a custom
       about box just because PRCS doesn't seem capable of getting the
       version into the InfoPlist.strings file; otherwise we'd use the
       standard about box. */
    value *f = NULL;
    f = caml_named_value("unisonGetVersion");
    [versionText setStringValue:
        [NSString stringWithCString:
        String_val(Callback_checkexn(*f, Val_unit))]];
    
    /* Command-line processing */
    f = caml_named_value("unisonInit0");
    value clprofile = Callback_checkexn(*f, Val_unit);
    
    /* Add toolbar */
    toolbar = [[[UnisonToolbar alloc] 
        initWithIdentifier: @"unisonToolbar" :self :tableView] autorelease];
    [mainWindow setToolbar: toolbar];

    /* Set up the first window the user will see */
    if (Is_block(clprofile)) {
        /* A profile name was given on the command line */
        value caml_profile = Field(clprofile,0);
        [self profileSelected:[NSString 
            stringWithCString:String_val(caml_profile)]];

        /* If invoked from terminal we need to bring the app to the front */
        [NSApp activateIgnoringOtherApps:YES];
        [mainWindow orderFront:self];

        /* Start the connection */
        [self connect:caml_profile];
    }
    else {
        /* If invoked from terminal we need to bring the app to the front */
        [NSApp activateIgnoringOtherApps:YES];
        /* Bring up the dialog to choose a profile */
        [self chooseProfiles];
    }
}

- (void)chooseProfiles
{
    [mainWindow setContentView:blankView];
    [self resizeWindowToSize:[chooseProfileView frame].size];
    [mainWindow setContentMinSize:
        NSMakeSize(NSWidth([[mainWindow contentView] frame]),150)];
    [mainWindow setContentMaxSize:NSMakeSize(FLT_MAX, FLT_MAX)];
    [mainWindow setContentView:chooseProfileView];
    [toolbar setView:@"chooseProfileView"];
    // profiles get keyboard input
    [mainWindow makeFirstResponder:[profileController tableView]];
}

- (IBAction)createButton:(id)sender
{
    [preferencesController reset];
    [mainWindow setContentView:blankView];
    [self resizeWindowToSize:[preferencesView frame].size];
    [mainWindow setContentMinSize:
        NSMakeSize(400,NSHeight([[mainWindow contentView] frame]))];
    [mainWindow setContentMaxSize:
        NSMakeSize(FLT_MAX,NSHeight([[mainWindow contentView] frame]))];
    [mainWindow setContentView:preferencesView];
    [toolbar setView:@"preferencesView"];
}

- (IBAction)saveProfileButton:(id)sender
{
    if ([preferencesController validatePrefs]) {
        // so the list contains the new profile
        [profileController initProfiles];
        [self chooseProfiles];
    }
}

- (IBAction)cancelProfileButton:(id)sender
{
    [self chooseProfiles];
}

/* Only valid once a profile has been selected */
- (NSString *)profile
{
    return myProfile;
}

- (void)profileSelected:(NSString *)aProfile
{
    [aProfile retain];
    [myProfile release];
    myProfile = aProfile;
    [updatesText setStringValue:
        [NSString stringWithFormat:@"Synchronizing profile '%@'", myProfile]];
}

- (IBAction)restartButton:(id)sender
{
    [tableView setEditable:NO];
    [self chooseProfiles];
}

- (IBAction)rescan:(id)sender
{
    /* There is a delay between turning off the button and it
       actually being disabled. Make sure we don't respond. */
    if ([self validateItem:@selector(rescan:)]) [self afterOpen];
}

- (IBAction)openButton:(id)sender
{
    NSString *profile = [profileController selected];
    [self profileSelected:profile];
    const char *s = [profile cString];
    value caml_s = caml_copy_string(s);
    [self connect:caml_s];
    return;
}

- (void)connect:(value)profileName
{
    // contact server, propagate prefs
    NSLog(@"Connecting...");

    // Switch to ConnectingView
    [mainWindow setContentView:blankView];
    [self resizeWindowToSize:[updatesView frame].size];
    [mainWindow setContentMinSize:NSMakeSize(150,150)];
    [mainWindow setContentMaxSize:NSMakeSize(FLT_MAX, FLT_MAX)];
    [mainWindow setContentView:ConnectingView];
    [toolbar setView:@"connectingView"];

    // Update (almost) immediately
    [ConnectingView display];

    thisProfileName = profileName;

    syncable = NO;
    afterSync = NO;    
    
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
        selector:@selector(afterOpen:)
        name:NSThreadWillExitNotification object:nil];
    [NSThread detachNewThreadSelector:@selector(doOpenThread:)
        toTarget:self withObject:nil];

}

- (void)doOpenThread:(id)whatever
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    value *f = NULL;
    f = caml_named_value("unisonInit1");
    preconn = Callback_checkexn(*f, thisProfileName);
    if (preconn == Val_unit) {
        NSLog(@"Connected.");
        [pool release];
        return;
    }
    // prompting required
    preconn = Field(preconn,0); // value of Some
    f = caml_named_value("openConnectionPrompt");
    value prompt = Callback_checkexn(*f, preconn);
    if (prompt == Val_unit) {
        // turns out, no prompt needed, but must finish opening connection
        f = caml_named_value("openConnectionEnd");
        Callback_checkexn(*f, preconn);
        NSLog(@"Connected.");
        [pool release];
        return;
    }
    [self raisePasswordWindow:
        [NSString stringWithCString:String_val(Field(prompt,0))]];
    
    NSLog(@"Connected.");
    [pool release];
}

- (void)raisePasswordWindow:(NSString *)prompt
{
    // FIX: some prompts don't ask for password, need to look at it
    NSLog(@"Got the prompt: '%@'",prompt);
    value *f = caml_named_value("unisonPasswordMsg");
    value v = Callback_checkexn(*f, caml_copy_string([prompt cString]));
    if (v == Val_true) {
        [passwordPrompt setStringValue:@"Please enter your password"];
        [NSApp beginSheet:passwordWindow
            modalForWindow:mainWindow
            modalDelegate:nil
            didEndSelector:nil
            contextInfo:nil];
        return;
    }
    f = caml_named_value("unisonPassphraseMsg");
    v = Callback_checkexn(*f, caml_copy_string([prompt cString]));
    if (v == Val_true) {
        [passwordPrompt setStringValue:@"Please enter your passphrase"];
        [NSApp beginSheet:passwordWindow
            modalForWindow:mainWindow
            modalDelegate:nil
            didEndSelector:nil
            contextInfo:nil];
        return;
    }
    f = caml_named_value("unisonAuthenticityMsg");
    v = Callback_checkexn(*f, caml_copy_string([prompt cString]));
    if (v == Val_true) {
        int i = NSRunAlertPanel(@"New host",prompt,@"Yes",@"No",nil);
        if (i == NSAlertDefaultReturn) {
            f = caml_named_value("openConnectionReply");
            Callback2_checkexn(*f, preconn, caml_copy_string("yes"));
            f = caml_named_value("openConnectionPrompt");
            value prompt = Callback_checkexn(*f, preconn);
            if (prompt == Val_unit) {
                // all done with prompts, finish opening connection
                f = caml_named_value("openConnectionEnd");
                Callback_checkexn(*f, preconn);
                return;
            }
            else {
                [self raisePasswordWindow:
                    [NSString stringWithCString:String_val(Field(prompt,0))]];
                return;
            }
        }
        if (i == NSAlertAlternateReturn) {
            f = caml_named_value("openConnectionCancel");
            Callback_checkexn(*f, preconn);
            return;
        }
        else {
            NSLog(@"Unrecognized response '%d' from NSRunAlertPanel",i);
            f = caml_named_value("openConnectionCancel");
            Callback_checkexn(*f, preconn);
            return;
        }
    }
    NSLog(@"Unrecognized message from ssh: %@",prompt);
    f = caml_named_value("openConnectionCancel");
    Callback_checkexn(*f, preconn);
}

// The password window will invoke this when Enter occurs, b/c we
// are the delegate.
- (void)controlTextDidEndEditing:(NSNotification *)notification
{
    NSNumber *reason = [[notification userInfo] objectForKey:@"NSTextMovement"];
    int code = [reason intValue];
    if (code == NSReturnTextMovement)
        [self endPasswordWindow:self];
}
// Or, the Continue button will invoke this when clicked
- (IBAction)endPasswordWindow:(id)sender
{
    [passwordWindow orderOut:self];
    [NSApp endSheet:passwordWindow];
    if ([sender isEqualTo:passwordCancelButton]) {
        value *f = caml_named_value("openConnectionCancel");
        Callback_checkexn(*f, preconn);
        [self chooseProfiles];
        return;
    }
    NSString *password = [passwordText stringValue];
    value *f = NULL;
    const char *s = [password cString];
    value caml_s = caml_copy_string(s);
    f = caml_named_value("openConnectionReply");
    Callback2_checkexn(*f, preconn, caml_s);
    f = caml_named_value("openConnectionPrompt");
    value prompt = Callback_checkexn(*f, preconn);
    if (prompt == Val_unit) {
        // all done with prompts, finish opening connection
        f = caml_named_value("openConnectionEnd");
        Callback_checkexn(*f, preconn);
    }
    else [self raisePasswordWindow:
        [NSString stringWithCString:String_val(Field(prompt,0))]];
}

- (void)afterOpen:(NSNotification *)notification
{
    [[NSNotificationCenter defaultCenter] removeObserver:self
        name:NSThreadWillExitNotification
        object:nil];

    [self afterOpen];
}

- (void)afterOpen
{
    // move to updates window after clearing it
    [self clearDetails];
    [reconItems release];
    reconItems = nil;
    [mainWindow setContentView:blankView];
    [self resizeWindowToSize:[updatesView frame].size];
    [mainWindow setContentMinSize:
        NSMakeSize(NSWidth([[mainWindow contentView] frame]),200)];
    [mainWindow setContentMaxSize:NSMakeSize(FLT_MAX, FLT_MAX)];
    [mainWindow setContentView:updatesView];
    [toolbar setView:@"updatesView"];

    syncable = NO;
    afterSync = NO;

    [tableView deselectAll:self];
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];

    newProgress = 0.;
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"progressBarNeedsUpdate"
        object:self];
    
    // this should depend on the number of reconitems, and is now done
    // in updateReconItems:
    // reconItems table gets keyboard input
    //[mainWindow makeFirstResponder:tableView];
    [tableView scrollRowToVisible:0];

    [[NSNotificationCenter defaultCenter] addObserver:self
        selector:@selector(afterUpdate:)
        name:NSThreadWillExitNotification object:nil];
    [NSThread detachNewThreadSelector:@selector(doUpdateThread:)
        toTarget:self withObject:nil];
}

- (void)doUpdateThread:(id)whatever
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    preconn = Val_unit; // so old preconn can be garbage collected
    value *f = caml_named_value("unisonInit2");
    caml_reconItems = Callback_checkexn(*f, Val_unit);
    [pool release];
}

- (void)afterUpdate:(NSNotification *)notification
{
    [[NSNotificationCenter defaultCenter] removeObserver:self
        name:NSThreadWillExitNotification
        object:nil];
        [notificationController updateFinishedFor:[self profile]];
    [self updateReconItems];

    // label the left and right columns with the roots
    NSTableHeaderCell *left = 
        [[[tableView tableColumns] objectAtIndex:0] headerCell];
    value *f = caml_named_value("unisonFirstRootString");
    [left setObjectValue:[NSString stringWithCString:
        String_val(Callback_checkexn(*f, Val_unit))]];

    NSTableHeaderCell *right = 
        [[[tableView tableColumns] objectAtIndex:2] headerCell];
    f = caml_named_value("unisonSecondRootString");
    [right setObjectValue:[NSString stringWithCString:
        String_val(Callback_checkexn(*f, Val_unit))]];
    
    [tableView sortReconItemsByColumn:
        [tableView tableColumnWithIdentifier:@"direction"]];

    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"tableViewNeedsUpdate"
        object:self];

    if ([reconItems count] > 0)
        shouldResetSelection = YES;

    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];

}

- (IBAction)syncButton:(id)sender
{
    [tableView setEditable:NO];
    syncable = NO;
    duringSync = YES;
 
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
        selector:@selector(afterSync:)
        name:NSThreadWillExitNotification object:nil];
    [NSThread detachNewThreadSelector:@selector(doSyncThread:)
        toTarget:self withObject:nil];
}

- (void)doSyncThread:(id)whatever
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    value *f = caml_named_value("unisonSynchronize");
    Callback_checkexn(*f, Val_unit);
    [pool release];
}

- (void)afterSync:(NSNotification *)notification
{
    [[NSNotificationCenter defaultCenter] removeObserver:self
        name:NSThreadWillExitNotification
        object:nil];
    [notificationController syncFinishedFor:[self profile]];
    duringSync = NO;
    afterSync = YES;
    
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];
    
    int i;
    for (i = 0; i < [reconItems count]; i++) {
        [[reconItems objectAtIndex:i] resetProgress];
    }

    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"tableViewSelectionDidChange"
        object:self];

    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"tableViewNeedsUpdate"
        object:self];
}

// A function called from ocaml
CAMLprim value reloadTable(value row)
{
    int i = Int_val(row);
    [me updateTableView:i]; // we need 'me' to access its instance variables
    return Val_unit;
}

- (void)updateTableView:(int)i
{
    [[reconItems objectAtIndex:i] resetProgress];
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"tableViewNeedsUpdate"
        object:self];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    if (!reconItems) return 0;
    else return [reconItems count];
}

- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex
{
    if (!reconItems) {
        return @"[internal error]";
    }
    if (rowIndex >= 0 && rowIndex < [reconItems count]) {
        NSString *identifier = [aTableColumn identifier];
        ReconItem *ri = [reconItems objectAtIndex:rowIndex];
        NSObject *s = [ri valueForKey:identifier];
        return s;
    }
    else return @"[internal error!]";
}

- (void)tableViewSelectionDidChange:(NSNotification *)note
{
    int n = [tableView numberOfSelectedRows];
    if (n == 1) [self displayDetails:[tableView selectedRow]];
    else [self clearDetails];
}

- (void)tableView:(NSTableView *)aTableView 
    didClickTableColumn:(NSTableColumn *)tableColumn
{
    if ([aTableView isEqual:tableView]) {
        [tableView sortReconItemsByColumn:tableColumn];
        [[NSNotificationCenter defaultCenter]
            postNotificationName:@"tableViewNeedsUpdate"
            object:self];
    }
}

- (NSMutableArray *)reconItems // used in ReconTableView only
{
    return reconItems;
}

- (void)updateReconItems
{
    [reconItems release];
    reconItems = [[NSMutableArray alloc] init];
    int j = 0;
    int n = Wosize_val(caml_reconItems);
    for (; j<n; j++) {
        [reconItems insertObject:
            [ReconItem initWithRiAndIndex:Field(caml_reconItems,j) index:j]
            atIndex:j];
    }
      
    // Only enable sync if there are reconitems
    if ([reconItems count]>0) {
        [tableView setEditable:YES];

        // reconItems table gets keyboard input
        [mainWindow makeFirstResponder:tableView];

        // Make sure details get updated
        [[NSNotificationCenter defaultCenter]
            postNotificationName:@"tableViewSelectionDidChange"
            object:self];

        syncable = YES;
    }
    else {
        [tableView setEditable:NO];
        afterSync = YES; // rescan should be enabled
	
        // reconItems table no longer gets keyboard input
        [mainWindow makeFirstResponder:nil];
    }
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"toolbarNeedsUpdate"
        object:self];
}

- (int)updateForIgnore:(int)i
{
    value *f = caml_named_value("unisonUpdateForIgnore");
    int j = Int_val(Callback_checkexn(*f,Val_int(i)));
    f = caml_named_value("unisonState");
    caml_reconItems = Callback_checkexn(*f, Val_unit);
    [self updateReconItems];
    return j;
}

// A function called from ocaml
CAMLprim value displayStatus(value s)
{
    [me statusTextSet:[NSString stringWithCString:String_val(s)]];
//    NSLog(@"dS: %s",String_val(s));
    return Val_unit;
}

- (void)statusTextSet:(NSString *)s {
    /* filter out strings with # reconitems, and empty strings */
    if (!NSEqualRanges([s rangeOfString:@"reconitems"], 
         NSMakeRange(NSNotFound,0))) return;
    /* store the new value and call the main thread 
       to actually do the display */
    [newStatusText setString:s];
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"statusTextNeedsUpdate"
        object:self];
}

// Called from ocaml to dislpay progress bar
CAMLprim value displayGlobalProgress(value p)
{
    [me setGlobalProgressToValue:Double_val(p)];
    return Val_unit;
}

- (void)setGlobalProgressToValue:(double) progress
{
    newProgress = progress;
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"progressBarNeedsUpdate"
        object:self];
}

// Called from ocaml to display diff
CAMLprim value displayDiff(value s, value s2)
{
    [me diffViewTextSet:
        [NSString stringWithCString:String_val(s)]
        bodyText:[NSString stringWithCString:String_val(s2)]];
    return Val_unit;
}

// Called from ocaml to display diff error messages
CAMLprim value displayDiffErr(value s)
{
    NSString * str = [NSString stringWithCString:String_val(s)];
    str = [[str componentsSeparatedByString:@"\n"] 
        componentsJoinedByString:@" "];
    [me statusTextSet:str];
    return Val_unit;
}

- (void)diffViewTextSet:(NSString *)title bodyText:(NSString *)body {
   [diffWindow setTitle:title];
   [diffView setFont:[NSFont fontWithName:@"Monaco" size:10]];
   [diffView setString:body];
   if (!doneFirstDiff) {
       /* On first open, position the diff window to the right of
       the main window, but without going off the mainwindow's screen */
       float screenOriginX = [[mainWindow screen] visibleFrame].origin.x;
       float screenWidth = [[mainWindow screen] visibleFrame].size.width;
       float mainOriginX = [mainWindow frame].origin.x;
       float mainOriginY = [mainWindow frame].origin.y;
       float mainWidth = [mainWindow frame].size.width;
       float mainHeight = [mainWindow frame].size.height;       
       float diffWidth = [diffWindow frame].size.width;

       float diffX = mainOriginX+mainWidth;
       float maxX = screenOriginX+screenWidth-diffWidth;
       if (diffX > maxX) diffX = maxX;
       float diffY = mainOriginY + mainHeight;
       
       NSPoint diffOrigin = NSMakePoint(diffX,diffY);
       [diffWindow cascadeTopLeftFromPoint:diffOrigin];
       
       doneFirstDiff = YES;
   }
   [diffWindow orderFront:nil];
}

- (void)displayDetails:(int)i
{
    if (i >= 0 && i < [reconItems count])
        {
        [detailsTextView setFont:[NSFont fontWithName:@"Monaco" size:10]];
        [detailsTextView setString:[[reconItems objectAtIndex:i] details]];
    }
}
- (void)clearDetails
{
    [detailsTextView setString:@""];
}

- (IBAction)raiseAboutWindow:(id)sender
{
    NSRect mainWindowFrame = [mainWindow frame];
    NSRect aboutWindowFrame = [aboutWindow frame];
    
    float aboutX = mainWindowFrame.origin.x + 
        (mainWindowFrame.size.width - aboutWindowFrame.size.width)/2;
    float aboutY = mainWindowFrame.origin.y + 
        (mainWindowFrame.size.height + aboutWindowFrame.size.height)/2;
    
    [aboutWindow cascadeTopLeftFromPoint:
        NSMakePoint(aboutX,aboutY)];
    
    [aboutWindow makeKeyAndOrderFront:nil];
}

- (IBAction)onlineHelp:(id)sender
{
    [[NSWorkspace sharedWorkspace]
        openURL:[NSURL URLWithString:@"http://www.cis.upenn.edu/~bcpierce/unison/docs.html"]];
}

/* from http://developer.apple.com/documentation/Security/Conceptual/authorization_concepts/index.html */
#include <Security/Authorization.h>
#include <Security/AuthorizationTags.h>
- (IBAction)installCommandLineTool:(id)sender
{
  /* Install the command-line tool in /usr/bin/unison.
     Requires root privilege, so we ask for it and 
     pass the task off to /bin/sh. */

  OSStatus myStatus;

  AuthorizationFlags myFlags = kAuthorizationFlagDefaults;
  AuthorizationRef myAuthorizationRef;
  myStatus = AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment,
				 myFlags, &myAuthorizationRef);
  if (myStatus != errAuthorizationSuccess) return;

  {
    AuthorizationItem myItems = {kAuthorizationRightExecute, 0,
				 NULL, 0};
    AuthorizationRights myRights = {1, &myItems};
    myFlags = kAuthorizationFlagDefaults |
      kAuthorizationFlagInteractionAllowed |
      kAuthorizationFlagPreAuthorize |
      kAuthorizationFlagExtendRights;
    myStatus =
      AuthorizationCopyRights(myAuthorizationRef,&myRights,NULL,myFlags,NULL);
  }
  if (myStatus == errAuthorizationSuccess) {
    NSBundle *bundle = [NSBundle mainBundle];
    NSString *bundle_path = [bundle bundlePath];
    NSString *exec_path =
      [bundle_path stringByAppendingString:@"/Contents/MacOS/cltool"];
    // Not sure why but this doesn't work:
    // [bundle pathForResource:@"cltool" ofType:nil];

    if (exec_path == nil) return;
    char *args[] = { "-f", (char *)[exec_path cString], 
		     "/usr/bin/unison", NULL };

    myFlags = kAuthorizationFlagDefaults;
    myStatus = AuthorizationExecuteWithPrivileges
      (myAuthorizationRef, "/bin/cp", myFlags, args,
       NULL);
  }
  AuthorizationFree (myAuthorizationRef, kAuthorizationFlagDefaults);

  /*
  if (myStatus == errAuthorizationCanceled)
    NSLog(@"The attempt was canceled\n");
  else if (myStatus) 
      NSLog(@"There was an authorization error: %ld\n", myStatus);
  */
}

- (BOOL)validateItem:(IBAction *) action
{
    if (action == @selector(syncButton:)) return syncable;
    // FIXME Restarting during sync is disabled because it causes UI corruption
    else if (action == @selector(restartButton:))
	return !duringSync;
    else if (action == @selector(rescan:))
	return ((syncable && !duringSync) || afterSync);
    else return YES;
}

- (BOOL)validateMenuItem:(NSMenuItem *)menuItem
{
    return [self validateItem:[menuItem action]];
}

- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem
{
    return [self validateItem:[toolbarItem action]];
}

- (void)resizeWindowToSize:(NSSize)newSize
{
    NSRect aFrame;

    float newHeight = newSize.height+[self toolbarHeightForWindow:mainWindow];
    float newWidth = newSize.width;

    aFrame = [NSWindow contentRectForFrameRect:[mainWindow frame]
                       styleMask:[mainWindow styleMask]];

    aFrame.origin.y += aFrame.size.height;
    aFrame.origin.y -= newHeight;
    aFrame.size.height = newHeight;
    aFrame.size.width = newWidth;

    aFrame = [NSWindow frameRectForContentRect:aFrame
                       styleMask:[mainWindow styleMask]];

    [mainWindow setFrame:aFrame display:YES animate:YES];
}

- (float)toolbarHeightForWindow:(NSWindow *)window
{
    NSToolbar *aToolbar;
    float toolbarHeight = 0.0;
    NSRect windowFrame;

    aToolbar = [window toolbar];
    if(aToolbar && [aToolbar isVisible])
    {
        windowFrame = [NSWindow contentRectForFrameRect:[window frame]
            styleMask:[window styleMask]];
        toolbarHeight = NSHeight(windowFrame)
            - NSHeight([[window contentView] frame]);
    }
    return toolbarHeight;
}

@end
