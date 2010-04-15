/* ProfileController */
/* Copyright (c) 2003, see file COPYING for details. */

#import <Cocoa/Cocoa.h>

@interface ProfileController : NSObject
{
    IBOutlet NSTableView *tableView;
    NSMutableArray *profiles;
    int defaultIndex; // -1 if no default, else the index in profiles of @"default"
}
- (void)initProfiles;
- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex;
- (NSString *)selected;
- (NSTableView *)tableView; // allows MyController to set up firstResponder
@end
