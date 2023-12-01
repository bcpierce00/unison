/* Copyright (c) 2003, 2016, see file COPYING for details. */

#import "MyController.h"

/* The following two define are a workaround for an incompatibility between
Ocaml 3.11.2 (and older) and the Mac OS X header files */
#define uint64 uint64_caml
#define int64 int64_caml

#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

@interface NSString (_UnisonUtil)
- (NSString *)trim;
@end

@implementation MyController

static MyController *me; // needed by reloadTable and displayStatus, below

// BCP (11/09): Added per Onne Gorter:
// if user closes main window, terminate app, instead of keeping an empty app around with no window
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication {
  return YES;
}

- (id)init
{
  if (([super init])) {

    /* Initialize locals */
    me = self;
    doneFirstDiff = NO;

    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    NSDictionary *appDefaults = [NSDictionary dictionaryWithObjectsAndKeys:
                                 /* By default, invite user to install cltool */
                                 @"YES",  @"CheckCltool",
                                 @"NO", @"openProfileAtStartup",
                                 @"",   @"profileToOpen",
                                 @"NO", @"deleteLogOnExit",
                                 @"",   @"detailsFont",
                                 @"",   @"diffFont",
                                 nil];

    [defaults registerDefaults:appDefaults];
    fontChangeTarget = nil;
  }

  return self;
}

- (void) applicationWillTerminate:(NSNotification *)aNotification {
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults setObject:[NSArchiver archivedDataWithRootObject:[detailsTextView font]] forKey:@"detailsFont"];
  [defaults setObject:[NSArchiver archivedDataWithRootObject:[diffView font]] forKey:@"diffFont"];
  [defaults synchronize];
}

- (void)awakeFromNib
{
  [splitView setAutosaveName:@"splitView"];

  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  NSFont *defaultFont = [NSFont fontWithName:@"Monaco" size:11];
  NSData *detailsFontData = [defaults dataForKey:@"detailsFont"];
  if (detailsFontData) {
    NSFont *tmpFont = (NSFont*) [NSUnarchiver unarchiveObjectWithData:detailsFontData];
    if (tmpFont)
      [detailsTextView setFont:tmpFont];
    else
      [detailsTextView setFont:defaultFont];
  } else
    [detailsTextView setFont:defaultFont];
  [detailsTextView.cell setBackgroundStyle:NSBackgroundStyleRaised];
  [detailsTextView.cell setBackgroundStyle:NSBackgroundStyleRaised];

  NSColor *startColor = [NSColor colorWithCalibratedRed:0.613 green:0.665 blue:0.715 alpha:1.000];
  NSColor *endColor = [NSColor colorWithCalibratedRed:0.439 green:0.496 blue:0.548 alpha:1.000];

  [detailsTextViewGradient setStartingColor:startColor];
  [detailsTextViewGradient setEndingColor:endColor];
  [detailsTextViewGradient setAngle:270];
  [connectingViewGradient setStartingColor:startColor];
  [connectingViewGradient setEndingColor:endColor];
  [connectingViewGradient setAngle:270];

  NSData *diffFontData = [defaults dataForKey:@"diffFont"];
  if (diffFontData) {
    NSFont *tmpFont = (NSFont*) [NSUnarchiver unarchiveObjectWithData:diffFontData];
    if (tmpFont)
      [diffView setFont:tmpFont];
    else
      [diffView setFont:defaultFont];
  } else
    [diffView setFont:defaultFont];

  blankView = [[NSView alloc] init];

  /* Double clicking in the profile list will open the profile */
  [[profileController tableView] setTarget:self];
  [[profileController tableView] setDoubleAction:@selector(openButton:)];

        [tableView setAutoresizesOutlineColumn:NO];

        // use combo-cell for path
  [[tableView tableColumnWithIdentifier:@"path"] setDataCell:[[[ImageAndTextCell alloc] init] autorelease]];

        // Custom progress cell
        ProgressCell *progressCell = [[[ProgressCell alloc] init] autorelease];
        [[tableView tableColumnWithIdentifier:@"percentTransferred"] setDataCell:progressCell];

  /* Set up the version string in the about box.  We use a custom
   about box just because PRCS doesn't seem capable of getting the
   version into the InfoPlist.strings file; otherwise we'd use the
   standard about box. */
  [versionText setStringValue:ocamlCall("S", "unisonGetVersion")];

  /* Command-line processing */
        OCamlValue *clprofile = (id)ocamlCall("@", "unisonInit0");

        BOOL areRootsSet = (long)ocamlCall("i", "areRootsSet") ? YES : NO;
        /*
        if (areRootsSet) {
                NSLog(@"Roots are on the command line");
        }
        else {
                NSLog(@"Roots are not set on the command line");
        }
        */

  /* Add toolbar */
  toolbar = [[[UnisonToolbar alloc]
              initWithIdentifier: @"unisonToolbar" :self :tableView] autorelease];
  [mainWindow setToolbar: toolbar];
        [toolbar takeTableModeView:tableModeSelector];
        [self initTableMode];


  /* Set up the first window the user will see */
  if (clprofile) {
    /* A profile name was given on the command line */
                NSString *profileName = [clprofile getField:0 withType:'S'];
    [self profileSelected:profileName];

    /* If invoked from terminal we need to bring the app to the front */
    [NSApp activateIgnoringOtherApps:YES];

    /* Start the connection */
    [self connect:profileName];
  }
  else if (areRootsSet) {
        /* If invoked from terminal we need to bring the app to the front */
        [NSApp activateIgnoringOtherApps:YES];
        /* Start the connection with the empty profile name, indicating roots only */
        [self connect:@""];
  }
  else {
    /* If invoked from terminal we need to bring the app to the front */
    [NSApp activateIgnoringOtherApps:YES];
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"openProfileAtStartup"]) {
      NSString *profileToOpen = [[NSUserDefaults standardUserDefaults]
                                 stringForKey:@"profileToOpen"];
      if ([[profileToOpen trim] compare:@""] != NSOrderedSame &&
          [[profileController getProfiles] indexOfObject:profileToOpen] != NSNotFound) {
        [self profileSelected:profileToOpen];
        [self connect:profileToOpen];
      } else {
        /* Bring up the dialog to choose a profile */
        [self chooseProfiles];
      }
    } else {
      /* Bring up the dialog to choose a profile */
      [self chooseProfiles];
    }
  }

  [mainWindow display];
  [mainWindow makeKeyAndOrderFront:nil];

  /* unless user has clicked Don't ask me again, ask about cltool */
  if ( ([[NSUserDefaults standardUserDefaults] boolForKey:@"CheckCltool"]) &&
          (![[NSFileManager defaultManager]
              /* BCP 6/2016: Changed from /usr/bin/unison for El Capitan, per
                 suggestion from Alan Shutko */
                 fileExistsAtPath:@"/usr/local/bin/unison"]) )
          [self raiseCltoolWindow:nil];
}

- (IBAction) checkOpenProfileChanged:(id)sender {
  [profileBox setEnabled:[checkOpenProfile state]];
  if ([profileBox isEnabled] && [profileBox indexOfSelectedItem] < 0) {
    [profileBox selectItemAtIndex:0];
    [[NSUserDefaults standardUserDefaults] setObject:[profileBox itemObjectValueAtIndex:0] forKey:@"profileToOpen"];
  }
}

- (IBAction) chooseFont:(id)sender {
  [[NSFontPanel sharedFontPanel] makeKeyAndOrderFront:self];
  [[NSFontManager sharedFontManager] setDelegate:self];
  fontChangeTarget = sender;
}

- (void) changeFont:(id)sender {
  NSFont *newFont = [sender convertFont:[detailsTextView font]];
  if (fontChangeTarget == chooseDetailsFont)
    [detailsTextView setFont:newFont];
  else if (fontChangeTarget == chooseDiffFont)
    [diffView setFont:newFont];
  [self updateFontDisplay];
}

- (void) updateFontDisplay {
  NSFont *detailsFont = [detailsTextView font];
  NSFont *diffFont = [diffView font];
  [detailsFontLabel setStringValue:[NSString stringWithFormat:@"%@ : %ld", [detailsFont displayName], (long) [detailsFont pointSize]]];
  [diffFontLabel setStringValue:[NSString stringWithFormat:@"%@ : %ld", [diffFont displayName], (long) [diffFont pointSize]]];
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
    [mainWindow setTitle:@"Unison"];

    // profiles get keyboard input
    [mainWindow makeFirstResponder:[profileController tableView]];
    [chooseProfileView display];
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
- (NSString *)profile {
    return myProfile;
}

- (void)profileSelected:(NSString *)aProfile
{
    [aProfile retain];
    [myProfile release];
    myProfile = aProfile;
    [mainWindow setTitle: [NSString stringWithFormat:@"Unison: %@", myProfile]];
}

- (IBAction)showPreferences:(id)sender {
  [profileBox removeAllItems];
  [profileBox addItemsWithObjectValues:[profileController getProfiles]];
  NSUInteger index = [[profileController getProfiles] indexOfObject:
                      [[NSUserDefaults standardUserDefaults]
                       stringForKey:@"profileToOpen"]];
  if (index == NSNotFound) {
    [checkOpenProfile setState:NSOffState];
    [profileBox setStringValue:@""];
  } else
    [profileBox selectItemAtIndex:index];

  [profileBox setEnabled:[checkOpenProfile state]];
  if ([profileBox isEnabled] && [profileBox indexOfSelectedItem] < 0)
    [profileBox selectItemAtIndex:0];

  [self updateFontDisplay];

  [self raiseWindow:preferencesWindow];
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
    if ([self validateItem:@selector(rescan:)]) {
        waitingForPassword = NO;
        [self afterOpen];
    }
}

- (IBAction)openButton:(id)sender
{
    NSString *profile = [profileController selected];
    if (profile) {
        [self profileSelected:profile];
        [self connect:profile];
    }
    return;
}

- (void)updateToolbar
{
  [toolbar validateVisibleItems];
        [tableModeSelector setEnabled:((syncable && !duringSync) || afterSync)];

        // Why?
    [updatesView setNeedsDisplay:YES];
}

- (void)updateTableViewWithReset:(BOOL)shouldResetSelection
{
        [tableView reloadData];
        if (shouldResetSelection) {
                [tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
                shouldResetSelection = NO;
        }
        [updatesView setNeedsDisplay:YES];
}

- (void)updateProgressBar:(NSNumber *)newProgress
{
        // NSLog(@"Updating progress bar: %i - %i", (int)[newProgress doubleValue], (int)[progressBar doubleValue]);
        [progressBar incrementBy:([newProgress doubleValue] - [progressBar doubleValue])];
}

- (void)updateTableViewSelection
{
    NSInteger n = [tableView numberOfSelectedRows];
    if (n == 1) [self displayDetails:[tableView itemAtRow:[tableView selectedRow]]];
    else [self clearDetails];
}

- (void)outlineViewSelectionDidChange:(NSNotification *)note
{
        [self updateTableViewSelection];
}

- (void)connect:(NSString *)profileName
{
  // contact server, propagate prefs
  /* NSLog(@"Connecting to %@...", profileName); */

  // Switch to ConnectingView
  [mainWindow setContentView:blankView];
  [self resizeWindowToSize:[updatesView frame].size];
  [mainWindow setContentMinSize:NSMakeSize(150,150)];
  [mainWindow setContentMaxSize:NSMakeSize(FLT_MAX, FLT_MAX)];
  [mainWindow setContentView:ConnectingView];
  [toolbar setView:@"connectingView"];

  // Update (almost) immediately
  [ConnectingView display];
  [connectingAnimation startAnimation:self];

  syncable = NO;
  afterSync = NO;

        [self updateToolbar];

        // will spawn thread on OCaml side and callback when complete
  (void)ocamlCall("xS", "unisonInit1", profileName);
}

CAMLprim value unisonInit1Complete(value v)
{
  id pool = [[NSAutoreleasePool alloc] init];
  if (v == Val_unit) {
    /* NSLog(@"Connected."); */
    [me->connectingAnimation stopAnimation:me];
                [me->preconn release];
                me->preconn = NULL;
    [me performSelectorOnMainThread:@selector(afterOpen:) withObject:nil waitUntilDone:FALSE];
  } else {
    // prompting required
                me->preconn = [[OCamlValue alloc] initWithValue:Field(v,0)]; // value of Some
                [me performSelectorOnMainThread:@selector(unisonInit1Complete:) withObject:nil waitUntilDone:FALSE];
        }
  [pool release];
  return Val_unit;
}

- (void)unisonInit1Complete:(id)ignore
{
        @try {
                OCamlValue *prompt = ocamlCall("@@", "openConnectionPrompt", preconn);
                if (!prompt) {
                        // turns out, no prompt needed, but must finish opening connection
                        ocamlCall("x@", "openConnectionEnd", preconn);
                        // NSLog(@"Connected.");
                        waitingForPassword = NO;
                        [self afterOpen];
                        return;
                }
                waitingForPassword = YES;

                [self raisePasswordWindow:[prompt getField:0 withType:'S']];
        } @catch (NSException *ex) {
            NSRunAlertPanel(@"Connection Error", @"%@", @"OK", nil, nil, [ex description]);
                [self chooseProfiles];
                return;
        }

        // NSLog(@"Connected.");
}

- (void)raisePasswordWindow:(NSString *)prompt
{
    // FIX: some prompts don't ask for password, need to look at it
    /* NSLog(@"Got the prompt: '%@'",prompt); */
    if ((long)ocamlCall("iS", "unisonPasswordMsg", prompt)) {
        [passwordPrompt setStringValue:@"Please enter your password"];
        [NSApp beginSheet:passwordWindow
            modalForWindow:mainWindow
            modalDelegate:nil
            didEndSelector:nil
            contextInfo:nil];
        return;
    }
    if ((long)ocamlCall("iS", "unisonPassphraseMsg", prompt)) {
        [passwordPrompt setStringValue:@"Please enter your passphrase"];
        [NSApp beginSheet:passwordWindow
            modalForWindow:mainWindow
            modalDelegate:nil
            didEndSelector:nil
            contextInfo:nil];
        return;
    }
    if ((long)ocamlCall("iS", "unisonAuthenticityMsg", prompt)) {
        NSInteger i = NSRunAlertPanel(@"New host",@"%@",@"Yes",@"No",nil,prompt);
        if (i == NSAlertDefaultReturn) {
                        ocamlCall("x@s", "openConnectionReply", preconn, "yes");
                        prompt = ocamlCall("S@", "openConnectionPrompt", preconn);
            if (!prompt) {
                // all done with prompts, finish opening connection
                                ocamlCall("x@", "openConnectionEnd", preconn);
                waitingForPassword = NO;
                [self afterOpen];
                return;
            }
            else {
                                [self raisePasswordWindow:[NSString
                    stringWithUTF8String:String_val(Field(prompt,0))]];
                return;
            }
        }
        if (i == NSAlertAlternateReturn) {
                        ocamlCall("x@", "openConnectionCancel", preconn);
            return;
        }
        else {
            NSLog(@"Unrecognized response '%ld' from NSRunAlertPanel",(long)i);
                        ocamlCall("x@", "openConnectionCancel", preconn);
            return;
        }
    }
    /* Unison uimac versions <= 2.51.5 always produce an NSLog message
     * "Calling nonGuiStartup". Previously, this was just hidden from the
     * user. Starting Unison uimac version 2.51.5, error messages are
     * displayed to the user. Since this is not an error message, and it
     * is a known message to ignore, silently drop it here */
    if (NSEqualRanges([prompt rangeOfString:@"Calling nonGuiStartup"],
                      NSMakeRange(NSNotFound, 0))) {
        NSLog(@"Unrecognized message from ssh: '%@'",prompt);
        NSInteger i = NSRunAlertPanel(@"Connection Error", @"Unrecognized message from ssh: '%@'",
                          @"Continue", @"Cancel", nil, prompt);
	if (i == NSAlertAlternateReturn) {
            ocamlCall("x@", "openConnectionCancel", preconn);
            [self chooseProfiles];
            return;
	}
    }
    /* Unrecognized message from ssh does not immediately mean connection
     * failure. Continue. */
    prompt = ocamlCall("S@", "openConnectionPrompt", preconn);
    if (!prompt) {
        // all done with prompts, finish opening connection
        ocamlCall("x@", "openConnectionEnd", preconn);
        waitingForPassword = NO;
        [self afterOpen];
        return;
    } else {
        [self raisePasswordWindow:[NSString
            stringWithUTF8String:String_val(Field(prompt, 0))]];
        return;
    }
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
                ocamlCall("x@", "openConnectionCancel", preconn);
        [self chooseProfiles];
        return;
    }
    NSString *password = [passwordText stringValue];
        ocamlCall("x@S", "openConnectionReply", preconn, password);

    OCamlValue *prompt = ocamlCall("@@", "openConnectionPrompt", preconn);
    if (!prompt) {
        // all done with prompts, finish opening connection
                ocamlCall("x@", "openConnectionEnd", preconn);
        waitingForPassword = NO;
        [self afterOpen];
    }
    else {
                [self raisePasswordWindow:[prompt getField:0 withType:'S']];
    }
}

- (void)afterOpen:(id)ignore
{
        [self afterOpen];
}

- (void)afterOpen
{
    if (waitingForPassword) return;
    // move to updates window after clearing it
        [self updateReconItems:nil];
        [progressBar setDoubleValue:0.0];
        [progressBar stopAnimation:self];
    // [self clearDetails];
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
        [self updateToolbar];
        [self updateProgressBar:[NSNumber numberWithDouble:0.0]];

    // this should depend on the number of reconitems, and is now done
    // in updateReconItems:
    // reconItems table gets keyboard input
    //[mainWindow makeFirstResponder:tableView];
    [tableView scrollRowToVisible:0];

        [preconn release];
    preconn = nil; // so old preconn can be garbage collected
        // This will run in another thread spawned in OCaml and will return immediately
        // We'll get a call back to unisonInit2Complete() when it is complete
        ocamlCall("x", "unisonInit2");
}

- (void)doSync
{
    [tableView setEditable:NO];
    syncable = NO;
    duringSync = YES;

        [self updateToolbar];

        // This will run in another thread spawned in OCaml and will return immediately
        // We'll get a call back to syncComplete() when it is complete
        ocamlCall("x", "unisonSynchronize");
}

- (IBAction)syncButton:(id)sender
{
        [self doSync];
}


- (void)afterUpdate:(id)retainedReconItems
{
        // NSLog(@"In afterUpdate:...");
    [self updateReconItems:retainedReconItems];
        [retainedReconItems release];

    [notificationController updateFinishedFor:[self profile]];

    // label the left and right columns with the roots
        NSString *leftHost = [(NSString *)ocamlCall("S", "unisonFirstRootString") trim];
        NSString *rightHost = [(NSString *)ocamlCall("S", "unisonSecondRootString") trim];
        /*
    [[[tableView tableColumnWithIdentifier:@"left"] headerCell] setObjectValue:lefthost];
    [[[tableView tableColumnWithIdentifier:@"right"] headerCell] setObjectValue:rightHost];
    */
    [mainWindow setTitle: [NSString stringWithFormat:@"Unison: %@ (%@ <-> %@)",
                        [self profile], leftHost, rightHost]];

        // initial sort
        [tableView setSortDescriptors:[NSArray arrayWithObjects:
                [[tableView tableColumnWithIdentifier:@"fileSizeString"] sortDescriptorPrototype],
                [[tableView tableColumnWithIdentifier:@"path"] sortDescriptorPrototype],
                nil]];

        [self updateTableViewWithReset:([reconItems count] > 0)];
        [self updateToolbar];
        isBatchSet = (long)ocamlCall("i", "isBatchSet") ? YES : NO;
        /*
        if (isBatchSet) {
                NSLog(@"batch set on the command line");
        }
        else {
                NSLog(@"batch not set on the command line");
        }
        */

        if (isBatchSet) {
                [self doSync];
        }
}

CAMLprim value unisonInit2Complete(value v)
{
  id pool = [[NSAutoreleasePool alloc] init];
  [me performSelectorOnMainThread:@selector(afterUpdate:) withObject:[[OCamlValue alloc] initWithValue:v] waitUntilDone:FALSE];
  [pool release];
  return Val_unit;
}

- (void)afterSync:(id)ignore
{
    [notificationController syncFinishedFor:[self profile]];
    duringSync = NO;
    afterSync = YES;
    [self updateToolbar];

    int i;
    for (i = 0; i < [reconItems count]; i++) {
        [[reconItems objectAtIndex:i] resetProgress];
    }

        [self updateTableViewSelection];

        [self updateTableViewWithReset:FALSE];
}

- (void)alertDidEnd:(NSAlert *)alert returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
{
    [_timer invalidate];

    switch (returnCode) {
        case NSAlertAlternateReturn:
            return;
            break;

        default:
            [[NSApplication sharedApplication] performSelector: @selector(terminate:) withObject: nil afterDelay: 0.0];
            break;
    }
}

- (void)updateCountdown
{
    if (_secondsRemaining == 0) {
        [_timer invalidate];
        [[_timeoutAlert window] orderOut: nil];
        [self alertDidEnd: _timeoutAlert returnCode: NSAlertDefaultReturn contextInfo: nil];
    } else {
        [_timeoutAlert setMessageText: [NSString stringWithFormat: @"Unison will quit in %lu seconds", _secondsRemaining]];
        _secondsRemaining--;
    }
}


- (void)quitIfBatch:(id)ignore
{
        if (isBatchSet) {
          // NSLog(@"Automatically quitting because of -batch");
                _timeoutAlert = [NSAlert alertWithMessageText: @"" defaultButton: @"Quit" alternateButton: @"Cancel" otherButton: nil informativeTextWithFormat: @""];

                _secondsRemaining = 10;

                _timer = [NSTimer scheduledTimerWithTimeInterval: 1 target: self selector: @selector(updateCountdown) userInfo: nil repeats: YES];

                [_timeoutAlert beginSheetModalForWindow: mainWindow modalDelegate: self didEndSelector: @selector(alertDidEnd:returnCode:contextInfo:) contextInfo: NULL];
        }
}

// TODO: (BCP, 3/2012) Note that the string literal "~/unison.log" here is wrong --
// this is a user-settable preference (in ubase/trace.ml) and we should ask for its value.
CAMLprim value syncComplete()
{
  id pool = [[NSAutoreleasePool alloc] init];
  [me performSelectorOnMainThread:@selector(afterSync:) withObject:nil waitUntilDone:FALSE];
  if ([[NSUserDefaults standardUserDefaults] boolForKey:@"deleteLogOnExit"])
    [[NSFileManager defaultManager] removeItemAtPath:[@"~/unison.log" stringByExpandingTildeInPath] error:nil];
  [pool release];

  [me performSelectorOnMainThread:@selector(quitIfBatch:) withObject:nil waitUntilDone:FALSE];

  return Val_unit;
}

// A function called from ocaml
- (void)reloadTable:(NSNumber *)i
{
        // NSLog(@"*** ReloadTable: %i", [i intValue]);

    [[reconItems objectAtIndex:[i intValue]] resetProgress];
        [self updateTableViewWithReset:FALSE];
}

CAMLprim value reloadTable(value row)
{
  id pool = [[NSAutoreleasePool alloc] init];
        // NSLog(@"OCaml says... ReloadTable: %i", Int_val(row));
        NSNumber *num = [[NSNumber alloc] initWithInt:Int_val(row)];
  [me performSelectorOnMainThread:@selector(reloadTable:) withObject:num waitUntilDone:FALSE];
        [num release];
  [pool release];
  return Val_unit;
}

- (NSUInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item {
        if (item == nil) item = rootItem;
        return [[item children] count];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item {
    return [item isKindOfClass:[ParentReconItem class]];
}

- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item {
        if (item == nil) item = rootItem;
        return [[item children] objectAtIndex:index];
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item {
    NSString *identifier = [tableColumn identifier];
        if (item == nil) item = rootItem;

        if ([identifier isEqualToString:@"percentTransferred"] && (!duringSync && !afterSync)) return nil;

        return [item valueForKey:identifier];
}

static NSDictionary *_SmallGreyAttributes = nil;

- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(NSCell *)cell forTableColumn:(NSTableColumn *)tableColumn item:(id)item {
        NSString *identifier = [tableColumn identifier];
    if ([identifier isEqualToString:@"path"]) {
                // The file icon
                [(ImageAndTextCell*)cell setImage:[item fileIcon]];

                // For parents, format the file count into the text
                long fileCount = [item fileCount];
                if (fileCount > 1) {
                        NSString *countString = [NSString stringWithFormat:@"  (%ld files)", fileCount];
                        NSString *fullString = [(NSString *)[cell objectValue] stringByAppendingString:countString];
                        NSMutableAttributedString *as = [[NSMutableAttributedString alloc] initWithString:fullString];

                        if (!_SmallGreyAttributes) {
                                NSColor *txtColor = [NSColor grayColor];
                                NSFont *txtFont = [NSFont systemFontOfSize:9.0];
                                _SmallGreyAttributes = [[NSDictionary dictionaryWithObjectsAndKeys:txtFont,
                                        NSFontAttributeName, txtColor, NSForegroundColorAttributeName,  nil] retain];
                        }
                        [as setAttributes:_SmallGreyAttributes range:NSMakeRange([fullString length] - [countString length], [countString length])];
                        [cell setAttributedStringValue:as];
                        [as release];
                }
    } else if ([identifier isEqualToString:@"percentTransferred"]) {
                [(ProgressCell*)cell setIcon:[item direction]];
                [(ProgressCell*)cell setStatusString:[item progressString]];
                [(ProgressCell*)cell setIsActive:[item isKindOfClass:[LeafReconItem class]]];
    }
}

- (void)outlineView:(NSOutlineView *)outlineView
      sortDescriptorsDidChange:(NSArray *)oldDescriptors {
        NSArray *originalSelection = [outlineView selectedObjects];

        // do we want to catch case of object changes to allow resort in same direction for progress / direction?
        // Could check if our objects change and if the first item at the head of new and old were the same
        [rootItem sortUsingDescriptors:[outlineView sortDescriptors]];
        [outlineView reloadData];
        [outlineView setSelectedObjects:originalSelection];
}

// Delegate methods

- (BOOL)outlineView:(NSOutlineView *)outlineView shouldEditTableColumn:(NSTableColumn *)tableColumn item:(id)item {
    return NO;
}

- (NSMutableArray *)reconItems // used in ReconTableView only
{
    return reconItems;
}

- (NSInteger)tableMode
{
        return [tableModeSelector selectedSegment];
}

- (IBAction)tableModeChanged:(id)sender
{
        [[NSUserDefaults standardUserDefaults] setInteger:[self tableMode]+1 forKey:@"TableLayout"];
        [self updateForChangedItems];
}

- (void)initTableMode
{
        long mode = [[NSUserDefaults standardUserDefaults] integerForKey:@"TableLayout"] - 1;
        if (mode == -1) mode = 1;
        [tableModeSelector setSelectedSegment:mode];
}

- (void)updateReconItems:(OCamlValue *)caml_reconItems
{
    [reconItems release];
    reconItems = [[NSMutableArray alloc] init];
        long i, n =[caml_reconItems count];
    for (i=0; i<n; i++) {
                LeafReconItem *item = [[LeafReconItem alloc] initWithRiAndIndex:(id)[caml_reconItems getField:i withType:'@'] index:i];
        [reconItems addObject:item];
                [item release];
    }
        [self updateForChangedItems];
}

- (void)expandConflictedParent:(ParentReconItem *)parent
{
        if ([parent hasConflictedChildren]) {
                // NSLog(@"Expanding conflictedParent: %@", [parent fullPath]);
                [tableView expandItem:parent expandChildren:NO];
                NSArray *children = [parent children];
                NSUInteger i = 0, count = [children count];
                for (;i < count; i++) {
                        id child = [children objectAtIndex:i];
                        if ([child isKindOfClass:[ParentReconItem class]]) [self expandConflictedParent:child];
                }
        }
}

- (void)updateForChangedItems
{
        NSInteger tableMode = [self tableMode];

        [rootItem release];
        ParentReconItem *root = rootItem = [[ParentReconItem alloc] init];

        if (tableMode != 0 && [reconItems count]) {
                // Special roll-up root item for outline displays
                root = [[ParentReconItem alloc] init];
                [rootItem addChild:root nested:NO];
                [root setPath:@"All Changes..."];
                [root setFullPath:@""];
                [root release];
        }

    NSUInteger j = 0, n =[reconItems count];
    for (; j<n; j++) {
                [root addChild:[reconItems objectAtIndex:j] nested:(tableMode != 0)];
    }

        if (tableMode == 1) [root collapseParentsWithSingleChildren:YES];

        [tableView reloadData];

        if (NO) {
                // Pre-expand entire tree
                int i = [[rootItem children] count];
                while (i--) {
                        [tableView expandItem:[[rootItem children] objectAtIndex:i] expandChildren:YES];
                }
        } else if (tableMode != 0) {
                // Always open root node
                [tableView expandItem:rootItem expandChildren:NO];

                // then smart expand to reveal conflicts / changes in direction
                [self expandConflictedParent:root];

                // then open more levels if we can do so without causing scrolling
                [tableView expandChildrenIfSpace];
        }

    // Make sure details get updated (or cleared)
        [self updateTableViewSelection];

    // Only enable sync if there are reconitems
    if ([reconItems count]>0) {
        [tableView setEditable:YES];

        // reconItems table gets keyboard input
        [mainWindow makeFirstResponder:tableView];

        syncable = YES;
    }
    else {
        [tableView setEditable:NO];
        afterSync = YES; // rescan should be enabled

        // reconItems table no longer gets keyboard input
        [mainWindow makeFirstResponder:nil];
    }
        [self updateToolbar];
}

- (id)updateForIgnore:(id)item
{
    long j = (long)ocamlCall("ii", "unisonUpdateForIgnore", [reconItems indexOfObjectIdenticalTo:item]);
    // NSLog(@"Updating for ignore...");
    [self updateReconItems:(OCamlValue *)ocamlCall("@", "unisonState")];
    return [reconItems objectAtIndex:j];
}

// A function called from ocaml
CAMLprim value displayStatus(value s)
{
  id pool = [[NSAutoreleasePool alloc] init];
        NSString *str = [[NSString alloc] initWithUTF8String:String_val(s)];
    // NSLog(@"displayStatus: %@", str);
    [me performSelectorOnMainThread:@selector(statusTextSet:) withObject:str waitUntilDone:FALSE];
        [str release];
  [pool release];
  return Val_unit;
}

- (void)statusTextSet:(NSString *)s {
    /* filter out strings with # reconitems, and empty strings */
    if (!NSEqualRanges([s rangeOfString:@"reconitems"],
         NSMakeRange(NSNotFound,0))) return;
    [statusText setStringValue:s];
}

// Called from ocaml to display progress bar
CAMLprim value displayGlobalProgress(value p)
{
  id pool = [[NSAutoreleasePool alloc] init];
        NSNumber *num = [[NSNumber alloc] initWithDouble:Double_val(p)];
  [me performSelectorOnMainThread:@selector(updateProgressBar:)
                withObject:num waitUntilDone:FALSE];
        [num release];
  [pool release];
  return Val_unit;
}

// Called from ocaml to display diff
CAMLprim value displayDiff(value s, value s2)
{
  id pool = [[NSAutoreleasePool alloc] init];
  [me performSelectorOnMainThread:@selector(diffViewTextSet:)
                                                withObject:[NSArray arrayWithObjects:[NSString stringWithUTF8String:String_val(s)],
                                                                                        [NSString stringWithUTF8String:String_val(s2)], nil]
                                                waitUntilDone:FALSE];
  [pool release];
  return Val_unit;
}

// Called from ocaml to display diff error messages
CAMLprim value displayDiffErr(value s)
{
  id pool = [[NSAutoreleasePool alloc] init];
  NSString * str = [NSString stringWithUTF8String:String_val(s)];
  str = [[str componentsSeparatedByString:@"\n"] componentsJoinedByString:@" "];
        [me->statusText performSelectorOnMainThread:@selector(setStringValue:)
                                withObject:str waitUntilDone:FALSE];
  [pool release];
  return Val_unit;
}

- (void)diffViewTextSet:(NSArray *)args
{
        [self diffViewTextSet:[args objectAtIndex:0] bodyText:[args objectAtIndex:1]];
}

- (void)diffViewTextSet:(NSString *)title bodyText:(NSString *)body {
   if ([body length]==0) return;
   [diffWindow setTitle:title];
   //[diffView setFont:diffFont];
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

- (void)displayDetails:(ReconItem *)item
{
        //[detailsTextView setFont:diffFont];
        NSString *text = [item details];
        if (!text) text = @"";
        [detailsTextView setStringValue:text];
}

- (void)clearDetails
{
    [detailsTextView setStringValue:@""];
}

- (IBAction)raiseCltoolWindow:(id)sender
{
  [cltoolPref setState:[[NSUserDefaults standardUserDefaults] boolForKey:@"CheckCltool"] ? NSOffState : NSOnState];
  [self raiseWindow: cltoolWindow];
}

- (IBAction)cltoolYesButton:(id)sender;
{
  [[NSUserDefaults standardUserDefaults] setBool:([cltoolPref state] != NSOnState) forKey:@"CheckCltool"];
  [self installCommandLineTool:self];
  [cltoolWindow close];
}

- (IBAction)cltoolNoButton:(id)sender;
{
  [[NSUserDefaults standardUserDefaults] setBool:([cltoolPref state] != NSOnState) forKey:@"CheckCltool"];
  [cltoolWindow close];
}

- (IBAction)raiseAboutWindow:(id)sender
{
    [self raiseWindow: aboutWindow];
}

- (void)raiseWindow:(NSWindow *)theWindow
{
    NSRect screenFrame = [[mainWindow screen] visibleFrame];
    NSRect mainWindowFrame = [mainWindow frame];
    NSRect theWindowFrame = [theWindow frame];

    float winX = mainWindowFrame.origin.x +
        (mainWindowFrame.size.width - theWindowFrame.size.width)/2;
    float winY = mainWindowFrame.origin.y +
        (mainWindowFrame.size.height + theWindowFrame.size.height)/2;

    if (winX<screenFrame.origin.x) winX=screenFrame.origin.x;
    float maxX = screenFrame.origin.x+screenFrame.size.width-
        theWindowFrame.size.width;
    if (winX>maxX) winX=maxX;
    float minY = screenFrame.origin.y+theWindowFrame.size.height;
    if (winY<minY) winY=minY;
    float maxY = screenFrame.origin.y+screenFrame.size.height;
    if (winY>maxY) winY=maxY;

    [theWindow cascadeTopLeftFromPoint:
        NSMakePoint(winX,winY)];

    [theWindow makeKeyAndOrderFront:nil];
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
    char *args[] = { "-f", (char *)[exec_path UTF8String],
                     "/usr/local/bin/unison", NULL };

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

- (BOOL)validateItem:(SEL) action
{
    if (action == @selector(syncButton:)) return syncable;
    // FIXME Restarting during sync is disabled because it causes UI corruption
    else if (action == @selector(restartButton:)) return !duringSync;
    else if (action == @selector(rescan:)) return ((syncable && !duringSync) || afterSync);
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

CAMLprim value fatalError(value s)
{
        NSString *str = [[NSString alloc] initWithUTF8String:String_val(s)];

        [me performSelectorOnMainThread:@selector(fatalError:) withObject:str waitUntilDone:FALSE];
        [str release];
    return Val_unit;
}

- (void)fatalError:(NSString *)msg {
        NSRunAlertPanel(@"Fatal error", @"%@", @"Exit", nil, nil, msg);
        exit(1);
}

/* Returns true if we need to exit, false if we proceed */

CAMLprim value warnPanel(value s)
{
        NSString *str = [[NSString alloc] initWithUTF8String:String_val(s)];

  [me performSelectorOnMainThread:@selector(warnPanel:) withObject:str waitUntilDone:TRUE];
        [str release];
  if (me -> shouldExitAfterWarning) {
    return Val_true;
  } else {
    return Val_false;
  }
}

- (void)warnPanel:(NSString *)msg {
  NSInteger warnVal = NSRunAlertPanel(@"Warning", @"%@", @"Proceed", @"Exit", nil, msg);
  NSLog(@"Warning Panel Returned %ld",(long)warnVal);
  if (warnVal == NSAlertAlternateReturn) {
    shouldExitAfterWarning = YES;
  } else {
    shouldExitAfterWarning = FALSE;
  }
}

@end

@implementation NSString (_UnisonUtil)
- (NSString *)trim
{
        NSCharacterSet *ws = [NSCharacterSet whitespaceCharacterSet];
        NSUInteger len = [self length], i = len;
        while (i && [ws characterIsMember:[self characterAtIndex:i-1]]) i--;
        return (i == len) ? self : [self substringToIndex:i];
}
@end
