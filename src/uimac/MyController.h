/* MyController */
/* Copyright (c) 2003, see file COPYING for details. */

#import <Cocoa/Cocoa.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

@class ProfileController, PreferencesController, NotificationController, 
    ReconTableView, UnisonToolbar;

@interface MyController : NSObject
{
    IBOutlet NSWindow *mainWindow;
    UnisonToolbar *toolbar;

    IBOutlet ProfileController *profileController;
    IBOutlet NSView *chooseProfileView;
    NSSize chooseProfileSize;
    NSString *myProfile;
    value thisProfileName;

    IBOutlet PreferencesController *preferencesController;
    IBOutlet NSView *preferencesView;
    NSSize preferencesSize;

    IBOutlet NSView *updatesView;
    NSSize updatesSize;

    IBOutlet NSView *ConnectingView;
    NSSize ConnectingSize;

    NSView *blankView;

    IBOutlet ReconTableView *tableView;
    IBOutlet NSTextField *updatesText;
    IBOutlet NSTextView *detailsTextView;
    IBOutlet NSTextField *statusText;

    IBOutlet NSWindow *passwordWindow;
    IBOutlet NSTextField *passwordPrompt;
    IBOutlet NSTextField *passwordText;
    IBOutlet NSButton *passwordCancelButton;

    IBOutlet NSWindow *aboutWindow;
    IBOutlet NSTextField *versionText;

    IBOutlet NotificationController *notificationController;

    BOOL syncable;
    BOOL duringSync;	
    BOOL afterSync;

    value caml_reconItems;
    NSMutableArray *reconItems;
    value preconn;

    BOOL doneFirstDiff;
    IBOutlet NSWindow *diffWindow;
    IBOutlet NSTextView *diffView;
}

- (void)awakeFromNib;

- (void)chooseProfiles;
- (IBAction)createButton:(id)sender;
- (IBAction)saveProfileButton:(id)sender;
- (IBAction)cancelProfileButton:(id)sender;
- (NSString *)profile;
- (void)profileSelected:(NSString *)aProfile;

- (IBAction)restartButton:(id)sender;
- (IBAction)rescan:(id)sender;

- (IBAction)openButton:(id)sender;
- (void)connect:(value)profileName;
- (void)doOpenThread:(id)whatever;
- (void)raisePasswordWindow:(NSString *)prompt;
- (void)controlTextDidEndEditing:(NSNotification *)notification;
- (IBAction)endPasswordWindow:(id)sender;
- (void)afterOpen:(NSNotification *)notification;
- (void)afterOpen;

- (void)doUpdateThread:(id)whatever;
- (void)afterUpdate:(NSNotification *)notification;

- (IBAction)syncButton:(id)sender;
- (void)doSyncThread:(id)whatever;
- (void)afterSync:(NSNotification *)notification;

- (void)updateTableView:(int)i;
- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex;
- (void)tableViewSelectionDidChange:(NSNotification *)note;
- (void)tableView:(NSTableView *)aTableView 
    didClickTableColumn:(NSTableColumn *)tableColumn;

- (NSMutableArray *)reconItems;
- (void)updateReconItems;
- (int)updateForIgnore:(int)i;

- (void)statusTextSet:(NSString *)s;
- (void)diffViewTextSet:(NSString *)title bodyText:(NSString *)body;
- (void)displayDetails:(int)i;
- (void)clearDetails;

- (IBAction)raiseAboutWindow:(id)sender;
- (IBAction)onlineHelp:(id)sender;
- (IBAction)installCommandLineTool:(id)sender;

- (BOOL)validateItem:(IBAction *) action;
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem;
- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem;

- (void)forceUpdatesViewRefresh;
- (void)resizeWindowToSize:(NSSize)newSize;

@end
