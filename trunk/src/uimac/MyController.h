/* MyController */
/* Copyright (c) 2003, see file COPYING for details. */

#import <Cocoa/Cocoa.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

@class ProfileController, PreferencesController, NotificationController, ReconTableView, UnisonToolbar;

@interface MyController : NSObject
{
    IBOutlet NSWindow *mainWindow;
    UnisonToolbar *toolbar;

    BOOL doneFirstDiff;
    IBOutlet NSWindow *diffWindow;
    IBOutlet NSTextView *diffView;

    IBOutlet ProfileController *profileController;
    IBOutlet NSView *chooseProfileView;
    NSSize chooseProfileSize;

    IBOutlet PreferencesController *preferencesController;
    IBOutlet NSView *preferencesView;
    NSSize preferencesSize;

    IBOutlet NSView *updatesView;
    NSSize updatesSize;

    IBOutlet NSView *ConnectingView;
    NSSize ConnectingSize;

    IBOutlet ReconTableView *tableView;
    IBOutlet NSTextField *updatesText;

    IBOutlet NSWindow *passwordWindow;
    IBOutlet NSTextField *passwordPrompt;
    IBOutlet NSTextField *passwordText;
    IBOutlet NSTextView *detailsTextView;
    IBOutlet NSTextField *statusText;

    IBOutlet NSButton *passwordCancelButton;

    IBOutlet NSWindow *aboutWindow;
    IBOutlet NSTextField *versionText;

	IBOutlet NotificationController *notificationController;
	NSString *myProfile;

    NSView *blankView;
    value thisProfileName;
    value caml_reconItems;
    NSMutableArray *reconItems;
    value preconn;

    NSString *pName;

    BOOL syncable;
    BOOL duringSync;	
}

- (IBAction)createButton:(id)sender;
- (IBAction)saveProfileButton:(id)sender;
- (IBAction)cancelProfileButton:(id)sender;
- (IBAction)openButton:(id)sender;
- (IBAction)restartButton:(id)sender;
- (IBAction)rescan:(id)sender;
- (IBAction)syncButton:(id)sender;
- (IBAction)onlineHelp:(id)sender;
- (void)doOpenThread:(id)whatever;
- (void)afterOpen:(NSNotification *)notification;
- (void)afterOpen;
- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex;
- (void)raisePasswordWindow:(NSString *)prompt;
- (IBAction)raiseAboutWindow:(id)sender;
- (void)controlTextDidEndEditing:(NSNotification *)notification;
- (IBAction)endPasswordWindow:(id)sender;
- (NSMutableArray *)reconItems;
- (int)updateForIgnore:(int)i;
- (void)displayDetails:(int)i;
- (IBAction)installCommandLineTool:(id)sender;
- (void)profileSelected:(NSString *)aProfile;
- (NSString *)profile;
- (BOOL)validateItem:(IBAction *) action;
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem;
- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem;
- (void)forceUpdatesViewRefresh;

@end
