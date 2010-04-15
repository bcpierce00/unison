/* MyController */
/* Copyright (c) 2003, see file COPYING for details. */

#import <Cocoa/Cocoa.h>

@class ProfileController, PreferencesController, NotificationController,
    ReconItem, ParentReconItem, ReconTableView, UnisonToolbar, OCamlValue;

@interface MyController : NSObject
{
    IBOutlet NSWindow *mainWindow;
    UnisonToolbar *toolbar;

    IBOutlet NSWindow *cltoolWindow;
    IBOutlet NSButton *cltoolPref;

    IBOutlet ProfileController *profileController;
    IBOutlet NSView *chooseProfileView;
    NSString *myProfile;

    IBOutlet PreferencesController *preferencesController;
    IBOutlet NSView *preferencesView;

    IBOutlet NSView *updatesView;
    IBOutlet NSView *ConnectingView;

    NSView *blankView;

    IBOutlet ReconTableView *tableView;
    IBOutlet NSTextField *updatesText;
    IBOutlet NSTextView *detailsTextView;
    IBOutlet NSTextField *statusText;

    IBOutlet NSWindow *passwordWindow;
    IBOutlet NSTextField *passwordPrompt;
    IBOutlet NSTextField *passwordText;
    IBOutlet NSButton *passwordCancelButton;
    BOOL waitingForPassword;

    IBOutlet NSWindow *aboutWindow;
    IBOutlet NSTextField *versionText;

    IBOutlet NSProgressIndicator *progressBar;

    IBOutlet NotificationController *notificationController;

    BOOL syncable;
    BOOL duringSync;	
    BOOL afterSync;

    NSMutableArray *reconItems;
	ParentReconItem *rootItem;
    OCamlValue *preconn;

    BOOL doneFirstDiff;
    IBOutlet NSWindow *diffWindow;
    IBOutlet NSTextView *diffView;
    IBOutlet NSSegmentedControl *tableModeSelector;
}

- (id)init;
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
- (void)connect:(NSString *)profileName;
- (void)raisePasswordWindow:(NSString *)prompt;
- (void)controlTextDidEndEditing:(NSNotification *)notification;
- (IBAction)endPasswordWindow:(id)sender;
- (void)afterOpen;

- (IBAction)syncButton:(id)sender;
- (IBAction)tableModeChanged:(id)sender;
- (void)initTableMode;

- (NSMutableArray *)reconItems;
- (void)updateForChangedItems;
- (void)updateReconItems:(OCamlValue *)items;
- (id)updateForIgnore:(id)i;

- (void)statusTextSet:(NSString *)s;
- (void)diffViewTextSet:(NSString *)title bodyText:(NSString *)body;
- (void)displayDetails:(ReconItem *)item;
- (void)clearDetails;

- (IBAction)raiseCltoolWindow:(id)sender;
- (IBAction)cltoolYesButton:(id)sender;
- (IBAction)cltoolNoButton:(id)sender;

- (IBAction)raiseAboutWindow:(id)sender;
- (IBAction)raiseWindow:(NSWindow *)theWindow;
- (IBAction)onlineHelp:(id)sender;
- (IBAction)installCommandLineTool:(id)sender;

- (BOOL)validateItem:(IBAction *) action;
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem;
- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem;

- (void)resizeWindowToSize:(NSSize)newSize;
- (float)toolbarHeightForWindow:(NSWindow *)window;

@end

