//
//  ReconTableView.m
//  Unison
//
//  Created by Trevor Jim on Wed Aug 27 2003.
//  Copyright (c) 2003.  See file COPYING for details.
//

#import "ReconTableView.h"
#import "ReconItem.h"
#import "MyController.h"

@implementation NSOutlineView (_UnisonExtras)
- (NSArray *)selectedObjects
{
	NSMutableArray *result = [NSMutableArray array];
	NSEnumerator *e = [self selectedRowEnumerator];
    NSNumber *n;
    while (n = [e nextObject]) [result addObject:[self itemAtRow:[n intValue]]]; 
	return result;
}

- (void)setSelectedObjects:(NSArray *)selectedObjects
{
	NSMutableIndexSet *set = [NSMutableIndexSet indexSet];
	int i = [selectedObjects count];
	while (i--) {
		int index = [self rowForItem:[selectedObjects objectAtIndex:i]];
		if (index >= 0)	[set addIndex:index];
	}
	[self selectRowIndexes:set byExtendingSelection:NO];
}

- (NSEnumerator *)selectedObjectEnumerator
{
	return [[self selectedObjects] objectEnumerator];
}

- (int)rowCapacityWithoutScrolling 
{
	float bodyHeight = [self visibleRect].size.height;
	bodyHeight -=  [[self headerView] visibleRect].size.height;
	return bodyHeight / ([self rowHeight] + 2.0);
}

- (BOOL)_canAcceptRowCountWithoutScrolling:(int)rows
{
	return ([self numberOfRows] + rows) <= [self rowCapacityWithoutScrolling];
}

- (BOOL)_expandChildrenIfSpace:(id)parent level:(int)level
{
	BOOL didExpand = NO;
	id dataSource = [self dataSource];
	int count = [dataSource outlineView:self numberOfChildrenOfItem:parent];
	if (level == 0) {
		if (count && ([self isItemExpanded:parent] || [self _canAcceptRowCountWithoutScrolling:count])) {
			[self expandItem:parent expandChildren:NO];
			didExpand = YES;
		}
	} else {
		// try expanding each of our children.  If all expand, then return YES,
		// indicating that it may be worth trying the next level
		int i;
		for (i=0; i < count; i++) {
			id child = [dataSource outlineView:self child:i ofItem:parent];
			didExpand = [self _expandChildrenIfSpace:child level:level-1] || didExpand;
		}
	}

	return didExpand;
}

- (void)expandChildrenIfSpace
{
	int level = 1;
	while ([self _expandChildrenIfSpace:nil level:level]) level++;
}

@end

@implementation ReconTableView

- (BOOL)editable
{
    return editable;
}

- (void)setEditable:(BOOL)x
{
    editable = x;
}

- (BOOL)validateItem:(IBAction *) action
{
    if (action == @selector(selectAll:)
        || action == @selector(selectConflicts:)
        || action == @selector(copyLR:)
        || action == @selector(copyRL:)
        || action == @selector(leaveAlone:)
        || action == @selector(forceNewer:)
        || action == @selector(forceOlder:)
        || action == @selector(revert:)
        || action == @selector(ignorePath:)
        || action == @selector(ignoreExt:)
        || action == @selector(ignoreName:))
        return editable;
    else if (action == @selector(merge:)) {
        if (!editable) return NO;
        else return [self canDiffSelection];
    }
    else if (action == @selector(showDiff:)) {
        if ((!editable) || (!([self numberOfSelectedRows]==1)))
            return NO;
	else return [self canDiffSelection];
    }
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

- (void)doIgnore:(unichar)c
{
    NSEnumerator *e = [self selectedObjectEnumerator];
	ReconItem *item, *last = nil;
    while (item = [e nextObject]) {
        [item doIgnore:c];
		last = item;
    }
    if (last) { // something was selected
        last = [[self dataSource] updateForIgnore:last];
        [self selectRow:[self rowForItem:last] byExtendingSelection:NO];
        [self reloadData];
    }
}

- (IBAction)ignorePath:(id)sender
{
    [self doIgnore:'I'];
}

- (IBAction)ignoreExt:(id)sender
{
    [self doIgnore:'E'];
}

- (IBAction)ignoreName:(id)sender
{
    [self doIgnore:'N'];
}

- (void)doAction:(unichar)c
{
    int numSelected = 0;
    NSEnumerator *e = [self selectedObjectEnumerator];
	ReconItem *item, *last = nil;
    while (item = [e nextObject]) {
        numSelected++;
        [item doAction:c];
		last = item;
    }
    if (numSelected>0) {
		int nextRow = [self rowForItem:last] + 1;
        if (numSelected == 1 && [self numberOfRows] > nextRow && c!='d') {
            // Move to next row, unless already at last row, or if more than one row selected
            [self selectRow:nextRow byExtendingSelection:NO];
            [self scrollRowToVisible:nextRow];
        }
        [self reloadData];
    }
}

- (IBAction)copyLR:(id)sender
{
    [self doAction:'>'];
}

- (IBAction)copyRL:(id)sender
{
    [self doAction:'<'];
}

- (IBAction)leaveAlone:(id)sender
{
    [self doAction:'/'];
}

- (IBAction)forceOlder:(id)sender
{
    [self doAction:'-'];
}

- (IBAction)forceNewer:(id)sender
{
    [self doAction:'+'];
}

- (IBAction)selectConflicts:(id)sender
{
    [self deselectAll:self];
    NSMutableArray *reconItems = [[self dataSource] reconItems];
    int i = 0;
    for (; i < [reconItems count]; i++) {
		ReconItem *item = [reconItems objectAtIndex:i]; 
        if ([item isConflict])
            [self selectRow:[self rowForItem:item] byExtendingSelection:YES];
    }
}

- (IBAction)revert:(id)sender
{
    [self doAction:'R'];
}

- (IBAction)merge:(id)sender
{
    [self doAction:'m'];
}

- (IBAction)showDiff:(id)sender
{
    [self doAction:'d'];
}

/* There are menu commands for these, but we add some shortcuts so you don't
   have to press the Command key */
- (void)keyDown:(NSEvent *)event
{
    /* some keys return zero-length strings */
    if ([[event characters] length] == 0) {
        [super keyDown:event];
        return;
    }

    /* actions are disabled when when menu items are */
    if (!editable) {
        [super keyDown:event];
        return;
    }

    unichar c = [[event characters] characterAtIndex:0];
    switch (c) {
    case '>':
    case NSRightArrowFunctionKey:
        [self doAction:'>'];
        break;
    case '<':
    case NSLeftArrowFunctionKey:
        [self doAction:'<'];
        break;
    case '?':
    case '/':
        [self doAction:'/'];
        break;
    default:
        [super keyDown:event];
        break;
    }
}

- (BOOL)canDiffSelection
{
    BOOL canDiff = YES;
    NSEnumerator *e = [self selectedObjectEnumerator];
	ReconItem *item;
    while (item = [e nextObject]) {
        if (![item canDiff]) canDiff= NO;
    }    
    return canDiff;
}

/* Override default highlight colour because it's hard to see the 
   conflict/resolution icons */
- (id)_highlightColorForCell:(NSCell *)cell
{   
    if(([[self window] firstResponder] == self) &&
        [[self window] isMainWindow] &&
        [[self window] isKeyWindow])

        return [NSColor colorWithCalibratedRed:0.7 green:0.75 blue:0.8 alpha:1.0];
    else return [NSColor colorWithCalibratedRed:0.8 green:0.8 blue:0.8 alpha:1.0];
}

@end
