//
//  UnisonToolbar.h
//  
//  Extended NSToolbar with several views
//
//  Created by Ben Willmore on Sun March 12 2006.
//  Copyright (c) 2006, licensed under GNU GPL.
//

#import <AppKit/AppKit.h>

@class ReconTableView, MyController;

@interface UnisonToolbar : NSToolbar
{
	ReconTableView*  tableView;
	MyController*    myController;
	NSString*        currentView;
	NSView*          tableModeView;
}

- initWithIdentifier:(NSString *) identifier :(MyController *) aController :(ReconTableView *) aTableView;
- (NSToolbarItem *) toolbar: (NSToolbar *)toolbar itemForItemIdentifier: (NSString *) itemIdent willBeInsertedIntoToolbar:(BOOL) willBeInserted;
- (NSArray *) itemIdentifiersForView: (NSString *) whichView;
- (NSArray *) toolbarDefaultItemIdentifiers: (NSToolbar *) toolbar;
- (NSArray *) toolbarAllowedItemIdentifiers: (NSToolbar *) toolbar;
- (void) setView: (NSString *) whichView;
- (void)takeTableModeView:(NSView *)view;

@end
