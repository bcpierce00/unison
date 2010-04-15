//
//  UnisonToolbar.h
//  
//  Extended NSToolbar with several views
//
//  Created by Ben Willmore on Sun March 12 2006.
//  Copyright (c) 2006, licensed under GNU GPL.
//

#import "UnisonToolbar.h"
#import "MyController.h"

static NSString*        QuitItemIdentifier      = @"Quit";
static NSString*        OpenItemIdentifier      = @"Open";
static NSString*        NewItemIdentifier       = @"New";
static NSString*        GoItemIdentifier        = @"Go";
static NSString*        CancelItemIdentifier    = @"Cancel";
static NSString*        SaveItemIdentifier      = @"Save";
static NSString*        RestartItemIdentifier   = @"Restart";
static NSString*        RescanItemIdentifier    = @"Rescan";
static NSString*        RToLItemIdentifier      = @"RToL";
static NSString*        MergeItemIdentifier     = @"Merge";
static NSString*        LToRItemIdentifier      = @"LToR";
static NSString*        SkipItemIdentifier      = @"Skip";
static NSString*        DiffItemIdentifier      = @"Diff";
static NSString*        TableModeIdentifier     = @"TableMode";

@implementation UnisonToolbar

- initWithIdentifier:(NSString *) identifier :(MyController *) aController :(ReconTableView *) aTableView {

    if ((self = [super initWithIdentifier: identifier])) {
		[self setAllowsUserCustomization: NO];
		[self setAutosavesConfiguration: NO];
		[self setDelegate: self];
		myController = aController;
		tableView = aTableView;
		currentView = @"";	
    }

    return self;
}

- (void)takeTableModeView:(NSView *)view
{
	tableModeView = [view retain];
	[view setHidden:YES];	
}

- (NSToolbarItem *) toolbar: (NSToolbar *)toolbar itemForItemIdentifier: (NSString *) itemIdent willBeInsertedIntoToolbar:(BOOL) willBeInserted {

    NSToolbarItem *toolbarItem = [[[NSToolbarItem alloc] initWithItemIdentifier: itemIdent] autorelease];
    if ([itemIdent isEqual: QuitItemIdentifier]) {
		[toolbarItem setLabel: @"Quit"];
		[toolbarItem setImage: [NSImage imageNamed: @"quit.tif"]];
		[toolbarItem setTarget:NSApp];
		[toolbarItem setAction:@selector(terminate:)];
	}
	else if ([itemIdent isEqual: OpenItemIdentifier]) {
        [toolbarItem setLabel: @"Open"];
        [toolbarItem setImage: [NSImage imageNamed: @"go.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(openButton:)];
    }
	else if ([itemIdent isEqual: NewItemIdentifier]) {
        [toolbarItem setLabel: @"New"];
        [toolbarItem setImage: [NSImage imageNamed: @"add.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(createButton:)];
    }
	else if ([itemIdent isEqual: CancelItemIdentifier]) {
        [toolbarItem setLabel: @"Cancel"];
        [toolbarItem setImage: [NSImage imageNamed: @"restart.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(chooseProfiles)];
    }
	else if ([itemIdent isEqual: SaveItemIdentifier]) {
        [toolbarItem setLabel: @"Save"];
        [toolbarItem setImage: [NSImage imageNamed: @"save.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(saveProfileButton:)];
    }	
	else if ([itemIdent isEqual: GoItemIdentifier]) {
        [toolbarItem setLabel: @"Go"];
        [toolbarItem setImage: [NSImage imageNamed: @"go.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(syncButton:)];
    }
    else if ([itemIdent isEqual: RestartItemIdentifier]) {
        [toolbarItem setLabel: @"Restart"];
        [toolbarItem setImage: [NSImage imageNamed: @"restart.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(restartButton:)];
    }
    else if ([itemIdent isEqual: RescanItemIdentifier]) {
        [toolbarItem setLabel: @"Rescan"];
        [toolbarItem setImage: [NSImage imageNamed: @"rescan.tif"]];
        [toolbarItem setTarget:myController];
        [toolbarItem setAction:@selector(rescan:)];
    }
    else if ([itemIdent isEqual: RToLItemIdentifier]) {
        [toolbarItem setLabel: @"Right to left"];
        [toolbarItem setImage: [NSImage imageNamed: @"left.tif"]];
        [toolbarItem setTarget:tableView];
        [toolbarItem setAction:@selector(copyRL:)];
    }
    else if ([itemIdent isEqual: MergeItemIdentifier]) {
        [toolbarItem setLabel: @"Merge"];
        [toolbarItem setImage: [NSImage imageNamed: @"merge.tif"]];
        [toolbarItem setTarget:tableView];
	[toolbarItem setAction:@selector(merge:)];
    }
	else if ([itemIdent isEqual: LToRItemIdentifier]) {
        [toolbarItem setLabel: @"Left to right"];
        [toolbarItem setImage: [NSImage imageNamed: @"right.tif"]];
        [toolbarItem setTarget:tableView];
        [toolbarItem setAction:@selector(copyLR:)];
	}
	else if ([itemIdent isEqual: SkipItemIdentifier]) {
        [toolbarItem setLabel: @"Skip"];
        [toolbarItem setImage: [NSImage imageNamed: @"skip.tif"]];
        [toolbarItem setTarget:tableView];
        [toolbarItem setAction:@selector(leaveAlone:)];
	}
	else if ([itemIdent isEqual: DiffItemIdentifier]) {
        [toolbarItem setLabel: @"Diff"];
        [toolbarItem setImage: [NSImage imageNamed: @"diff.tif"]];
        [toolbarItem setTarget:tableView];
        [toolbarItem setAction:@selector(showDiff:)];
	}
	else if ([itemIdent isEqual: TableModeIdentifier]) {
		[toolbarItem setLabel:@"Layout"];
		[toolbarItem setToolTip:@"Switch table nesting"];
		[tableModeView setHidden:NO];	
		[toolbarItem setView:tableModeView];
		[toolbarItem setMinSize:NSMakeSize(NSWidth([tableModeView frame]),NSHeight([tableModeView frame])+10)];
		[toolbarItem setMaxSize:NSMakeSize(NSWidth([tableModeView frame]),NSHeight([tableModeView frame])+10)];
	}

	return toolbarItem;
}

- (NSArray *) itemIdentifiersForView: (NSString *) whichView {
    if ([whichView isEqual: @"chooseProfileView"]) {
	    return [NSArray arrayWithObjects:   QuitItemIdentifier, NewItemIdentifier, OpenItemIdentifier, nil];
	}
	else if ([whichView isEqual: @"preferencesView"]) {
		return [NSArray arrayWithObjects:   QuitItemIdentifier, SaveItemIdentifier, CancelItemIdentifier, nil];
	}
	else if ([whichView isEqual: @"ConnectingView"]) {
		return [NSArray arrayWithObjects:   QuitItemIdentifier, nil];
	}
	else if ([whichView isEqual: @"updatesView"]) {
		return [NSArray arrayWithObjects:   QuitItemIdentifier,
			RestartItemIdentifier, 
			NSToolbarSeparatorItemIdentifier,
			GoItemIdentifier,
			RescanItemIdentifier,
			NSToolbarSeparatorItemIdentifier,
			RToLItemIdentifier, MergeItemIdentifier, LToRItemIdentifier, 
			SkipItemIdentifier, NSToolbarSeparatorItemIdentifier,
			DiffItemIdentifier, 
			TableModeIdentifier, nil];
	}
	else {
		return [NSArray arrayWithObjects: QuitItemIdentifier, Nil];
	}
}

- (NSArray *) toolbarDefaultItemIdentifiers: (NSToolbar *) toolbar {
    return [NSArray arrayWithObjects:   QuitItemIdentifier, NewItemIdentifier, OpenItemIdentifier, nil];
}

- (NSArray *) toolbarAllowedItemIdentifiers: (NSToolbar *) toolbar {
    return [NSArray arrayWithObjects:   QuitItemIdentifier, OpenItemIdentifier, NewItemIdentifier, 
	    CancelItemIdentifier, SaveItemIdentifier,
	    GoItemIdentifier, RestartItemIdentifier, RescanItemIdentifier,
	    RToLItemIdentifier, MergeItemIdentifier, LToRItemIdentifier, 
	    SkipItemIdentifier, DiffItemIdentifier,
	    NSToolbarSeparatorItemIdentifier, nil];
}

- (void) setView: (NSString *) whichView {
	if ([whichView isEqual:currentView]) return;

	currentView = whichView;

	int i;
	NSArray *identifiers;
	NSString *oldIdentifier;
	NSString *newIdentifier;
	
	identifiers=[self itemIdentifiersForView:whichView];
	for (i=0; i<[identifiers count]; i++) {
		newIdentifier = [identifiers objectAtIndex:i];
		if (i<[[self items] count]) {
			oldIdentifier = [[[self items] objectAtIndex:i] itemIdentifier];
			if ([newIdentifier isEqual: oldIdentifier] ) {
				[[[self items] objectAtIndex:i] setEnabled:YES];
			}
			else {
				[self removeItemAtIndex:i];
				[self insertItemWithItemIdentifier:newIdentifier atIndex:i];
			}
		}
		else {
			[self insertItemWithItemIdentifier:newIdentifier atIndex:i];
		}
	}
	while ([[self items] count] > [identifiers count]) {
		[self removeItemAtIndex:[identifiers count]];
	}
}

@end
