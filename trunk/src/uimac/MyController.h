/* MyController */
/* Copyright (c) 2003, see file COPYING for details. */

#import <Cocoa/Cocoa.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

@class ProfileController, PreferencesController, NotificationController, 
    ReconTableView, UnisonToolbar;

@interface MyController : NSObject
{
    NSMutableArray *notifications;
    NSThread *notificationThread;
    NSLock *notificationLock;
    NSMachPort *notificationPort;

    IBOutlet NSWindow *mainWindow;
    UnisonToolbar *toolbar;

    IBOutlet ProfileController *profileController;
    IBOutlet NSView *chooseProfileView;
    NSString *myProfile;
    value thisProfileName;

    IBOutlet PreferencesController *preferencesController;
    IBOutlet NSView *preferencesView;

    IBOutlet NSView *updatesView;
    IBOutlet NSView *ConnectingView;

    NSView *blankView;

    IBOutlet ReconTableView *tableView;
    IBOutlet NSTextField *updatesText;
    IBOutlet NSTextView *detailsTextView;
    IBOutlet NSTextField *statusText;
    NSMutableString *newStatusText;

    IBOutlet NSWindow *passwordWindow;
    IBOutlet NSTextField *passwordPrompt;
    IBOutlet NSTextField *passwordText;
    IBOutlet NSButton *passwordCancelButton;
    BOOL waitingForPassword;
    NSMutableString *newPasswordPrompt;

    IBOutlet NSWindow *aboutWindow;
    IBOutlet NSTextField *versionText;

    IBOutlet NSProgressIndicator *progressBar;
    double newProgress;

    IBOutlet NotificationController *notificationController;

    BOOL syncable;
    BOOL duringSync;	
    BOOL afterSync;

    value caml_reconItems;
    NSMutableArray *reconItems;
    value preconn;
    BOOL shouldResetSelection;

    BOOL doneFirstDiff;
    IBOutlet NSWindow *diffWindow;
    IBOutlet NSTextView *diffView;
}

- (id)init;
- (void)setupThreadingSupport;
- (void)handleMachMessage:(void *) msg;
- (void)processNotification:(NSNotification *) notification;
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
- (void)raisePasswordWindow:(NSNotification *)notification;
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
- (void)setGlobalProgressToValue:(double) progress;
- (void)diffViewTextSet:(NSString *)title bodyText:(NSString *)body;
- (void)displayDetails:(int)i;
- (void)clearDetails;

- (IBAction)raiseAboutWindow:(id)sender;
- (IBAction)onlineHelp:(id)sender;
- (IBAction)installCommandLineTool:(id)sender;

- (BOOL)validateItem:(IBAction *) action;
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem;
- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem;

- (void)resizeWindowToSize:(NSSize)newSize;
- (float)toolbarHeightForWindow:(NSWindow *)window;

@end
