//
//  ReconTableView.h
//  
//  NSTableView extended to handle additional keyboard events for the reconcile window.
//  The keyDown: method is redefined.
//
//  Created by Trevor Jim on Wed Aug 27 2003.
//  Copyright (c) 2003, licensed under GNU GPL.
//

#import <AppKit/AppKit.h>

@interface ReconTableView : NSOutlineView {
    BOOL editable;
}
- (BOOL)editable;
- (void)setEditable:(BOOL)x;
- (BOOL)validateItem:(IBAction *) action;
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem;
- (BOOL)validateToolbarItem:(NSToolbarItem *)toolbarItem;
- (IBAction)ignorePath:(id)sender;
- (IBAction)ignoreExt:(id)sender;
- (IBAction)ignoreName:(id)sender;
- (IBAction)copyLR:(id)sender;
- (IBAction)copyRL:(id)sender;
- (IBAction)leaveAlone:(id)sender;
- (IBAction)forceOlder:(id)sender;
- (IBAction)forceNewer:(id)sender;
- (IBAction)selectConflicts:(id)sender;
- (IBAction)revert:(id)sender;
- (IBAction)merge:(id)sender;
- (IBAction)showDiff:(id)sender;
- (BOOL)canDiffSelection;
@end

@interface NSOutlineView (_UnisonExtras)
- (NSArray *)selectedObjects;
- (NSEnumerator *)selectedObjectEnumerator;
- (void)setSelectedObjects:(NSArray *)selection;

- (void)expandChildrenIfSpace;

@end
