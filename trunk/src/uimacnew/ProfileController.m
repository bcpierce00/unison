/* Copyright (c) 2003, see file COPYING for details. */

#import "ProfileController.h"
#import "Bridge.h"

@implementation ProfileController

NSString *unisonDirectory()
{
    return (NSString *)ocamlCall("S", "unisonDirectory");
}

- (void)initProfiles
{
    NSString *directory = unisonDirectory();
    NSArray *files = [[NSFileManager defaultManager] directoryContentsAtPath:directory];
    unsigned int count = [files count];
    unsigned int i,j;
    
    [profiles release];
    profiles = [[NSMutableArray alloc] init];
    defaultIndex = -1;
    
    for (i = j = 0; i < count; i++) {
        NSString *file = [files objectAtIndex:i];
        if ([[file pathExtension] isEqualTo:@"prf"]) {
            NSString *withoutExtension = [file stringByDeletingPathExtension];
            [profiles insertObject:withoutExtension atIndex:j];
            if ([@"default" isEqualTo:withoutExtension]) defaultIndex = j;
            j++;
        }
    }
    if (j > 0)
        [tableView selectRow:0 byExtendingSelection:NO];
}

- (void)awakeFromNib
{
    // start with the default profile selected
    [self initProfiles];
    if (defaultIndex >= 0)
        [tableView selectRow:defaultIndex byExtendingSelection:NO];
    // on awake the scroll bar is inactive, but after adding profiles we might need it;
    // reloadData makes it happen.  Q: is setNeedsDisplay more efficient?
    [tableView reloadData];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    if (!profiles) return 0;
    else return [profiles count];
}

- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex
{
    if (rowIndex >= 0 && rowIndex < [profiles count])
        return [profiles objectAtIndex:rowIndex];
    else return @"[internal error!]";
}

- (NSString *)selected
{
    int rowIndex = [tableView selectedRow];
    if (rowIndex >= 0 && rowIndex < [profiles count])
        return [profiles objectAtIndex:rowIndex];
    else return @"[internal error!]";
}

- (NSTableView *)tableView
{
    return tableView;
}

@end
