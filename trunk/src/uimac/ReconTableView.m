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

@implementation ReconTableView

- (id)initWithCoder:(NSCoder *)decoder
{
    if (([super initWithCoder:decoder])) {
        editable = NO;
    }
    return self;
}

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
    NSMutableArray *reconItems = [[self dataSource] reconItems];
    NSEnumerator *e = [self selectedRowEnumerator];
    NSNumber *n = [e nextObject];
    int i = -1;
    for (; n != nil; n = [e nextObject]) {
        i = [n intValue];
        [[reconItems objectAtIndex:i] doIgnore:c];
    }
    if (i>=0) { // something was selected
        i = [[self dataSource] updateForIgnore:i];
        [self selectRow:i byExtendingSelection:NO];
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
    NSEnumerator *e = [self selectedRowEnumerator];
    NSNumber *n = [e nextObject];
    int numSelected = 0;
    int i = -1;
    for (; n != nil; n = [e nextObject]) {
        numSelected++;
        i = [n intValue];
        NSMutableArray *reconItems = [[self dataSource] reconItems];
        [[reconItems objectAtIndex:i] doAction:c];
    }
    if (numSelected>0) {
      if (numSelected == 1 && [self numberOfRows] > i+1 && c!='d') {
            // Move to next row, unless already at last row, or if more than one row selected
            [self selectRow:i+1 byExtendingSelection:NO];
            [self scrollRowToVisible:i+1];
      }
      else [self reloadData];
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
        if ([[reconItems objectAtIndex:i] isConflict])
            [self selectRow:i byExtendingSelection:YES];
    }
}

- (IBAction)revert:(id)sender
{
    NSMutableArray *reconItems = [[self dataSource] reconItems];
    NSEnumerator *e = [self selectedRowEnumerator];
    NSNumber *n = [e nextObject];
    int i;
    for (; n != nil; n = [e nextObject]) {
        i = [n intValue];
        [[reconItems objectAtIndex:i] revertDirection];
    }
    [self reloadData];
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
    NSMutableArray *reconItems = [[self dataSource] reconItems];
    NSEnumerator *e = [self selectedRowEnumerator];
    NSNumber *n = [e nextObject];
    int i;
    BOOL canDiff = YES;
    for (; n != nil; n = [e nextObject]) {
        i = [n intValue];
        if (![[reconItems objectAtIndex:i] canDiff]) canDiff= NO;
    }    
    return canDiff;
}

- (void)sortReconItemsByColumn:(NSTableColumn *)tableColumn
{
   /* Sort the table (e.g. when a column header is clicked) */

    NSMutableArray *reconItems = [[self dataSource] reconItems];
    BOOL ascending;

    /* Most columns can be sorted ascending or descending, and
       flip when you click the column header, using the state of the
       column indicator image to do the flipping.  However,
       we're forcing direction and progress to always sort ascending
       because this means you can click multiple times to update the
       sort order when the items change */

    NSImage *indicatorImage = [self indicatorImageInTableColumn:tableColumn];
    if (([indicatorImage isEqual:[NSImage imageNamed:@"NSAscendingSortIndicator"]]) &&
            (![[tableColumn identifier] isEqual:@"direction"]) &&
            (![[tableColumn identifier] isEqual:@"progress"])) {
        ascending = NO;
        indicatorImage = [NSImage imageNamed:@"NSDescendingSortIndicator"];
    }
    else { 
        ascending = YES;
        indicatorImage = [NSImage imageNamed:@"NSAscendingSortIndicator"];
    }

    /* Get rid of any indicators in other columns */
    NSArray * tableColumns = [self tableColumns];
    int i;
    for (i=0; i<[tableColumns count]; i++) {
        [self setIndicatorImage:NULL 
	    inTableColumn:[tableColumns objectAtIndex:i]];
    }
    
    /* Sort by the selected column, followed by ascending pathname order. 
       The keys correspond to methods of ReconItem */
    NSSortDescriptor *colDescriptor=[[[NSSortDescriptor alloc]
        initWithKey:[NSString stringWithFormat:@"%@SortKey", [tableColumn identifier]]
        ascending:ascending] autorelease];
    NSSortDescriptor *pathDescriptor=[[[NSSortDescriptor alloc]
        initWithKey:@"pathSortKey"
        ascending:YES] autorelease];		    
    NSArray *sortDescriptors=[NSArray 
	arrayWithObjects:colDescriptor, pathDescriptor,  nil];

    [reconItems setArray:[reconItems sortedArrayUsingDescriptors:sortDescriptors]];

    /* Update the column header indicator and redisplay the table */
    [self setIndicatorImage:indicatorImage inTableColumn:tableColumn];
    [self reloadData];
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
