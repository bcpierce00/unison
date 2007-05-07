#import "ReconItem.h"
#import "Bridge.h"

@implementation ReconItem

- initWithRiAndIndex:(OCamlValue *)v index:(int)i
{
    [super init];
    ri = [v retain];
	index = i;
    resolved = NO;
	selected = NO; // NB only used/updated during sorts. Not a 
				   // reliable indicator of whether item is selected
    directionSortString = @"";
    return self;
}

-(void)dealloc
{
    [ri autorelease];
    [super dealloc];
}

- (BOOL)selected
{
    return selected;
}

- (void)setSelected:(BOOL)x
{
    selected = x;
}

- (NSString *)path
{
    if (!path) path = [(NSString *)ocamlCall("S@", "unisonRiToPath", ri) retain];
    return path;
}

- (NSString *)left
{
    if (!left) left = [(NSString *)ocamlCall("S@", "unisonRiToLeft", ri) retain];
	return left;
}

- (NSString *)right
{
    if (right) right = [(NSString *)ocamlCall("S@", "unisonRiToRight", ri) retain];    
    return right;
}

- (NSImage *)direction
{
    if (direction) return direction;    
    NSString * dirString = (NSString *)ocamlCall("S@", "unisonRiToDirection", ri);

    BOOL changedFromDefault = [self changedFromDefault];
    
    if ([dirString isEqual:@"<-?->"]) {
        if (changedFromDefault | resolved) {
            direction = [NSImage imageNamed: @"table-skip.tif"];
	    directionSortString = @"3";
	}
        else {
            direction = [NSImage imageNamed: @"table-conflict.tif"];
	    directionSortString = @"2";
        }
    }
    
    else if ([dirString isEqual:@"---->"]) {
        if (changedFromDefault) {
            direction = [NSImage imageNamed: @"table-right-blue.tif"];
            directionSortString = @"6";
        }
	else {
            direction = [NSImage imageNamed: @"table-right-green.tif"];
            directionSortString = @"8";
        }
    }
    
    else if ([dirString isEqual:@"<----"]) {
        if (changedFromDefault) {
            direction = [NSImage imageNamed: @"table-left-blue.tif"];
            directionSortString = @"5";
        }
        else {
            direction = [NSImage imageNamed: @"table-left-green.tif"];
            directionSortString = @"7";
        }
    }

    else if ([dirString isEqual:@"<-M->"]) {
        direction = [NSImage imageNamed: @"table-merge.tif"];
        directionSortString = @"4";
    }

    else {
        direction = [NSImage imageNamed: @"table-error.tif"];
        directionSortString = @"1";
    }
    
    [direction retain];
    return direction;
}

- (void)setDirection:(char *)d
{
    [direction release];
    direction = nil;
	ocamlCall("x@", d, ri);
}

- (void)doAction:(unichar)action
{
    switch (action) {
    case '>':
        [self setDirection:"unisonRiSetRight"];
        break;
    case '<':
        [self setDirection:"unisonRiSetLeft"];
        break;
    case '/':
        [self setDirection:"unisonRiSetConflict"];
        resolved = YES;
        break;
    case '-':
        [self setDirection:"unisonRiForceOlder"];
        break;
    case '+':
        [self setDirection:"unisonRiForceNewer"];
        break;
    case 'm':
        [self setDirection:"unisonRiSetMerge"];
        break;
    case 'd':
        [self showDiffs];
        break;
    default:
        NSLog(@"ReconItem.doAction : unknown action");
        break;
    }
}

- (void)doIgnore:(unichar)action
{
    switch (action) {
    case 'I':
		ocamlCall("x@", "unisonIgnorePath");
        break;
    case 'E':
		ocamlCall("x@", "unisonIgnoreExt");
        break;
    case 'N':
		ocamlCall("x@", "unisonIgnoreName");
        break;
    default:
        NSLog(@"ReconItem.doIgnore : unknown ignore");
        break;
    }
}

- (NSString *)progress
{
    if (!progress) {
		progress = 	[(NSString *)ocamlCall("S@", "unisonRiToProgress", ri) retain];
		if ([progress isEqual:@"FAILED"]) [self updateDetails];
    }
	return progress;
}

- (void)resetProgress
{
    // Get rid of the memoized progress because we expect it to change
    [progress release];
    progress = nil;
}

- (NSString *)details
{
    if (details) return details;
    return [self updateDetails];
}

- (NSString *)updateDetails
{
	[details autorelease];
	details = [(NSString *)ocamlCall("S@", "unisonRiToDetails", ri) retain];
    return details;
}

- (BOOL)isConflict
{
	return ((int)ocamlCall("i@", "unisonRiIsConflict", ri) ? YES : NO);
}

- (BOOL)changedFromDefault
{
	return ((int)ocamlCall("i@", "changedFromDefault", ri) ? YES : NO);
}

- (void)revertDirection
{
	ocamlCall("x@", "unisonRiRevert", ri);
    [direction release];
    direction = nil;
    resolved = NO;
}

- (BOOL)canDiff
{
	return ((int)ocamlCall("i@", "canDiff", ri) ? YES : NO);
}

- (void)showDiffs
{
	ocamlCall("x@i", "runShowDiffs", ri, index);
}

/* Sorting functions. These have names equal to
   column identifiers + "SortKey", and return NSStrings that
   can be automatically sorted with their compare method */

- (NSString *) leftSortKey
{
    return [self replicaSortKey:[self left]];
}

- (NSString *) rightSortKey
{
    return [self replicaSortKey:[self right]];
}

- (NSString *) replicaSortKey:(NSString *)sortString
{
    /* sort order for left and right replicas */

    if ([sortString isEqualToString:@"Created"]) return @"1";
    else if ([sortString isEqualToString:@"Deleted"]) return @"2";
    else if ([sortString isEqualToString:@"Modified"]) return @"3";
    else if ([sortString isEqualToString:@""]) return @"4";
    else return @"5";
}

- (NSString *) directionSortKey
{
    /* Since the direction indicators are unsortable images, use
       directionSortString instead */

    if ([directionSortString isEqual:@""])
        [self direction];
    return directionSortString;
}

- (NSString *) progressSortKey
{
    /* Percentages, "done" and "" are sorted OK without help,
       but "start " should be sorted after "" and before "0%" */

    NSString * progressString = [self progress];
    if ([progressString isEqualToString:@"start "]) progressString = @" ";
    return progressString;
}

- (NSString *) pathSortKey
{
    /* default alphanumeric sort is fine for paths */
    return [self path];
}

@end
