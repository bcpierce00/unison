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
#if MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_5
    NSArray *files = [[NSFileManager defaultManager] directoryContentsAtPath:directory];
#else
    NSArray *files = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:directory error:nil];
#endif
    NSUInteger count = [files count];
    NSUInteger i,j;

    [profiles release];
    profiles = [[NSMutableArray alloc] init];
    defaultIndex = -1;

    if (files) {
        for (i = j = 0; i < count; i++) {
            NSString *file = [files objectAtIndex:i];
            if ([[file pathExtension] isEqualTo:@"prf"]) {
                NSString *withoutExtension = [file stringByDeletingPathExtension];
                [profiles insertObject:withoutExtension atIndex:j];
                if ([@"default" isEqualTo:withoutExtension]) defaultIndex = j;
                j++;
            }
        }
        NSSortDescriptor *sort = [NSSortDescriptor sortDescriptorWithKey:nil ascending:YES];
        [profiles sortUsingDescriptors:[NSArray arrayWithObject:sort]];

        if (j > 0)
            [tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];

    }
}

- (void)awakeFromNib
{
    // start with the default profile selected
    [self initProfiles];
    if (defaultIndex >= 0)
        [tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:defaultIndex] byExtendingSelection:NO];
    // on awake the scroll bar is inactive, but after adding profiles we might need it;
    // reloadData makes it happen.  Q: is setNeedsDisplay more efficient?
    [tableView reloadData];
}

- (NSUInteger)numberOfRowsInTableView:(NSTableView *)aTableView
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
    else return nil;
}

- (NSString *)selected
{
    NSInteger rowIndex = [tableView selectedRow];
    if (rowIndex >= 0 && rowIndex < [profiles count])
        return [profiles objectAtIndex:rowIndex];
    else return nil;
}

- (NSTableView *)tableView
{
    return tableView;
}

- (NSMutableArray*)getProfiles {
  return profiles;
}

@end
