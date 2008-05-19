#import "ReconItem.h"
#import "Bridge.h"

#import <Carbon/Carbon.h>

@implementation ReconItem

- init {
	[super init];
	selected = NO; // NB only used/updated during sorts. Not a 
				   // reliable indicator of whether item is selected
	fileSize = -1;
	bytesTransferred = -1;
	return self;
}

- (void)dealloc
{
    [path release];
    [fullPath release];
    // [direction release];  // assuming retained by cache, so not retained
    // [directionSortString release];  // no retain/release necessary because is constant
	[super dealloc];
}

- (ReconItem *)parent 
{
	return parent;
}

- (void)setParent:(ReconItem *)p
{
	parent = p;
}

- (void)willChange
{
	// propagate up parent chain
	[parent willChange];
}

- (NSArray *)children
{
	return nil;
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
    return path;
}

- (void)setPath:(NSString *)aPath
{
	[path autorelease];
	path = [aPath retain];
	
	// invalidate
	[fullPath autorelease];
	fullPath = nil;
}

- (NSString *)fullPath
{
	if (!fullPath) {
		NSString *parentPath = [parent fullPath];
		[self setFullPath:(([parentPath length] > 0) ? [parentPath stringByAppendingFormat:@"/%@", path] : path)];
	}

    return fullPath;
}

- (void)setFullPath:(NSString *)p
{
	[fullPath autorelease];
	fullPath = [p retain];
}

- (NSString *)left
{
    return nil;
}

- (NSString *)right
{
    return nil;
}

static NSMutableDictionary *_ChangeIconsByType = nil;

- (NSImage *)changeIconFor:(NSString *)type other:(NSString *)other
{
	if (![type length]) {
		if ([other isEqual:@"Created"]) {
			type = @"Absent";
		} else if ([other length]) {
			type = @"Unmodified";		
		} else
			return nil;
	}

	NSImage *result = [_ChangeIconsByType objectForKey:type];
	if (!result) {
		NSString *imageName = [NSString stringWithFormat:@"Change_%@.png", type];
		result = [NSImage imageNamed:imageName];
		if (!_ChangeIconsByType) _ChangeIconsByType = [[NSMutableDictionary alloc] init];
		[_ChangeIconsByType setObject:result forKey:type];
	}
	return result;
}

- (NSImage *)leftIcon
{
	return [self changeIconFor:[self left] other:[self right]];
}

- (NSImage *)rightIcon
{
	return [self changeIconFor:[self right] other:[self left]];
}


- (int)computeFileSize
{
	return 0;
}

- (int)bytesTransferred
{
	return 0;
}

- (int)fileCount
{
	return 1;
}

- (int)fileSize
{
	if (fileSize == -1) fileSize = [self computeFileSize];
	return fileSize;
}

- (NSString *)formatFileSize:(int)intSize
{
	float size = (float)intSize;
	if (size == 0) return @"--";
	if (size < 1024) return @"< 1KB"; // return [NSString stringWithFormat:@"%i bytes", size];
	size /= 1024;
	if (size < 1024) return [NSString stringWithFormat:@"%i KB", (int)size];
	size /= 1024;
	if (size < 1024) return [NSString stringWithFormat:@"%1.1f MB", size];
	size = size / 1024;
	return [NSString stringWithFormat:@"%1.1f GB", size];
}

- (NSString *)fileSizeString
{
	return [self formatFileSize:[self fileSize]];
}

- (NSString *)bytesTransferredString
{
	return [self formatFileSize:[self bytesTransferred]];
}

- (NSNumber *)percentTransferred
{
	int size = [self computeFileSize];
	return (size > 0) ? [NSNumber numberWithFloat:(((float)[self bytesTransferred]) / (float)size) * 100.0]
					  : nil;
}

static NSMutableDictionary *_iconsByExtension = nil;

- (NSImage *)iconForExtension:(NSString *)extension
{
	NSImage *icon = [_iconsByExtension objectForKey:extension];
	if (!_iconsByExtension) _iconsByExtension = [[NSMutableDictionary alloc] init];
	if (!icon) {
		icon = [[NSWorkspace sharedWorkspace] iconForFileType:extension];
		[icon setSize:NSMakeSize(16.0, 16.0)];
		[_iconsByExtension setObject:icon forKey:extension];
	}
	return icon;
}

- (NSImage *)fileIcon
{
	return [self iconForExtension:NSFileTypeForHFSTypeCode(kOpenFolderIcon)];
}

- (NSString *)dirString
{
	return @"<-?->";
}

- (NSImage *)direction
{
    if (direction) return direction;    
    NSString * dirString = [self dirString];

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
    
	else if ([dirString isEqual:@"<--->"]) {
		direction = [NSImage imageNamed: @"table-mixed.tif"];
		directionSortString = @"9";
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
    [direction autorelease];
    direction = nil;
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
    case 'R':
        [self revertDirection];
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
		ocamlCall("xS", "unisonIgnorePath", [self fullPath]);
        break;
    case 'E':
		ocamlCall("xS", "unisonIgnoreExt", [self path]);
        break;
    case 'N':
		ocamlCall("xS", "unisonIgnoreName", [self path]);
        break;
    default:
        NSLog(@"ReconItem.doIgnore : unknown ignore");
        break;
    }
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

- (NSString *)progress
{
	return nil;
}

- (BOOL)transferInProgress
{
	int soFar = [self bytesTransferred];
	return (soFar > 0) && (soFar != [self fileSize]);
}

- (void)resetProgress
{
}

- (NSString *)progressString
{
	NSString *progress = [self progress];
	if ([progress length] == 0 || [progress hasSuffix:@"%"])
		progress = [self transferInProgress] ? [self bytesTransferredString] : @"";
	else if ([progress isEqual:@"done"]) progress = @"";
	return progress;
}

- (NSString *)details
{
	return nil;
}

- (NSString *)updateDetails
{
    return [self details];
}

- (BOOL)isConflict
{
	return NO;
}

- (BOOL)changedFromDefault
{
	return NO;
}

- (void)revertDirection
{
	[self willChange];
    [direction release];
    direction = nil;
    resolved = NO;
}

- (BOOL)canDiff
{
	return NO;
}

- (void)showDiffs
{
}

- (ReconItem *)collapseParentsWithSingleChildren:(BOOL)isRoot
{
	return self;
}
@end


// --- Leaf items -- actually corresponding to ReconItems in OCaml
@implementation LeafReconItem

- initWithRiAndIndex:(OCamlValue *)v index:(int)i
{
    [super init];
    ri = [v retain];
	index = i;
    resolved = NO;
    directionSortString = @"";
    return self;
}

-(void)dealloc
{
    [ri release];
    [left release];
    [right release];
    [progress release];
    [details release];
	
    [super dealloc];
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
    if (!right)	right = [(NSString *)ocamlCall("S@", "unisonRiToRight", ri) retain];    
	return right;
}

- (int)computeFileSize
{
	return (int)ocamlCall("i@", "unisonRiToFileSize", ri);
}

- (int)bytesTransferred
{
	if (bytesTransferred == -1) {
		// need to force to fileSize if done, otherwise may not match up to 100%
		bytesTransferred = ([[self progress] isEqual:@"done"]) ? [self fileSize]
			: (int)ocamlCall("i@", "unisonRiToBytesTransferred", ri);
	}
	return bytesTransferred;
}

- (NSImage *)fileIcon
{
	NSString *extension = [[self path] pathExtension];
	
	if ([@"" isEqual:extension]) {
		NSString *type = (NSString *)ocamlCall("S@", "unisonRiToFileType", ri);
		extension = [type isEqual:@"dir"] 
			? NSFileTypeForHFSTypeCode(kGenericFolderIcon)
			: NSFileTypeForHFSTypeCode(kGenericDocumentIcon);
	}
	return [self iconForExtension:extension];
}

- (NSString *)dirString
{
	return (NSString *)ocamlCall("S@", "unisonRiToDirection", ri);
}

- (void)setDirection:(char *)d
{
	[self willChange];
    [super setDirection:d];
	ocamlCall("x@", d, ri);
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
	[self willChange];
	bytesTransferred = -1;
    [progress release];
	
	// Force update now so we get the result while the OCaml thread is available
	// [self progress];
	// [self bytesTransferred];
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
	[super revertDirection];
}

- (BOOL)canDiff
{
	return ((int)ocamlCall("i@", "canDiff", ri) ? YES : NO);
}

- (void)showDiffs
{
	ocamlCall("x@i", "runShowDiffs", ri, index);
}

@end

@interface NSImage (TintedImage)
- (NSImage *)tintedImageWithColor:(NSColor *) tint operation:(NSCompositingOperation) op;
@end

@implementation NSImage (TintedImage)

- (NSImage *)tintedImageWithColor:(NSColor *) tint operation:(NSCompositingOperation) op  
{
	NSSize size = [self size];
	NSRect imageBounds = NSMakeRect(0, 0, size.width, size.height);    
	NSImage *newImage = [[NSImage alloc] initWithSize:size];
	
	[newImage lockFocus];
	[self compositeToPoint:NSZeroPoint operation:NSCompositeSourceOver];
	[tint set];
	NSRectFillUsingOperation(imageBounds, op);
	[newImage unlockFocus];
	
	return [newImage autorelease];
}

@end

// ----  Parent nodes in grouped items
@implementation ParentReconItem

- init 
{
	[super init];
	_children = [[NSMutableArray alloc] init];
	return self;
}

- initWithPath:(NSString *)aPath
{
	[self init];
	path = [aPath retain];
	return self;
}

- (void)dealloc
{
	[_children release];
	[super dealloc];
}

- (NSArray *)children;
{
	return _children;
}

- (void)addChild:(ReconItem *)item pathArray:(NSArray *)pathArray level:(int)level
{
	NSString *element = [pathArray count] ? [pathArray objectAtIndex:level] : @"";

	// if we're at the leaf of the path, then add the item
	if (((0 == [pathArray count]) && (0 == level)) || (level == [pathArray count]-1)) {
		[item setParent:self];
		[item setPath:element];
		[_children addObject:item];
		return;
	}
		
	// find / add matching parent node
	ReconItem *last = [_children lastObject];
	if (last == nil || ![last isKindOfClass:[ParentReconItem class]] || ![[last path] isEqual:element]) {
		last = [[ParentReconItem alloc] initWithPath:element];
		[last setParent:self];
		[_children addObject:last];
		[last release];
	}
	
	[(ParentReconItem *)last addChild:item pathArray:pathArray level:level+1];
}

- (void)addChild:(ReconItem *)item nested:(BOOL)nested
{
	[item setPath:nil]; // invalidate/reset
	
	if (nested) {
		[self addChild:item pathArray:[[item path] pathComponents] level:0];
	} else {
		[item setParent:self];
		[_children addObject:item];
	}
}

- (void)sortUsingDescriptors:(NSArray *)sortDescriptors
{
	// sort our children
	[_children sortUsingDescriptors:sortDescriptors];
	
	// then have them sort theirs
	int i = [_children count];
	while (i--) {
		id child = [_children objectAtIndex:i];
		if ([child isKindOfClass:[ParentReconItem class]]) [child sortUsingDescriptors:sortDescriptors];
	}
}

- (ReconItem *)collapseParentsWithSingleChildren:(BOOL)isRoot
{
	// replace ourselves?
	if (!isRoot && [_children count] == 1) {
		ReconItem *child = [_children lastObject];
		[child setPath:[path stringByAppendingFormat:@"/%@", [child path]]];
		return [child collapseParentsWithSingleChildren:NO];
	}
	
	// recurse
	int i = [_children count];
	while (i--) {
		ReconItem *child = [_children objectAtIndex:i];
		ReconItem *replacement = [child collapseParentsWithSingleChildren:NO];
		if (child != replacement) {
			[_children replaceObjectAtIndex:i withObject:replacement];
			[replacement setParent:self];
		}
	}
	return self;
}

- (void)willChange
{
	// invalidate child-based state
	// Assuming caches / constant, so not retained / released
	// [direction autorelease]; 
	// [directionSortString autorelease]; 
	direction = nil;
	directionSortString = nil;
	bytesTransferred = -1;
	// fileSize = -1;
    // resolved = NO;

	// propagate up parent chain
	[parent willChange];
}

// Propagation methods
- (void)doAction:(unichar)action
{
	int i = [_children count];
	while (i--) {
		ReconItem *child = [_children objectAtIndex:i];
		[child doAction:action]; 
	}
}

- (void)doIgnore:(unichar)action
{
	// handle Path ignores at this level, name and extension at the child nodes
	if (action == 'I') {
		[super doIgnore:'I'];
	} else {
		int i = [_children count];
		while (i--) {
			ReconItem *child = [_children objectAtIndex:i];
			[child doIgnore:action]; 
		}
	}
}

// Rollup methods
- (int)fileCount
{
	if (fileCount == 0) {
		int i = [_children count];
		while (i--) {
			ReconItem *child = [_children objectAtIndex:i];
			fileCount += [child fileCount]; 
		}
	}
	return fileCount;
}

- (int)computeFileSize
{
	int size = 0;
	int i = [_children count];
	while (i--) {
		ReconItem *child = [_children objectAtIndex:i];
		size += [child fileSize]; 
	}
	return size;
}

- (int)bytesTransferred
{
	if (bytesTransferred == -1) {
		bytesTransferred = 0;
		int i = [_children count];
		while (i--) {
			ReconItem *child = [_children objectAtIndex:i];
			bytesTransferred += [child bytesTransferred]; 
		}
	}
	return bytesTransferred;
}



- (NSString *)dirString
{
	NSString *rollup = nil;
	int i = [_children count];
	while (i--) {
		ReconItem *child = [_children objectAtIndex:i];
		NSString *dirString = [child dirString];
		if (!rollup || [dirString isEqual:rollup]) {
			rollup = dirString;
		} else {
			// conflict
			if ([dirString isEqual:@"---->"] || [dirString isEqual:@"<----"] || [dirString isEqual:@"<--->"]) {
				if ([rollup isEqual:@"---->"] || [rollup isEqual:@"<----"] || [rollup isEqual:@"<--->"]) {
					rollup = @"<--->";
				}
			} else {
				rollup = @"<-?->";
			}
		}
	}
	// NSLog(@"dirString for %@: %@", path, rollup);
	return rollup;
}

- (BOOL)hasConflictedChildren
{
	NSString *dirString = [self dirString];
	BOOL result = [dirString isEqual:@"<--->"] || [dirString isEqual:@"<-?->"];
	// NSLog(@"hasConflictedChildren (%@): %@: %i", [self path], dirString, result);
	return result;
}

static NSMutableDictionary *_parentImages = nil;
static NSColor *_veryLightGreyColor = nil;
- (NSImage *)direction
{
	if (!_parentImages) {
		_parentImages = [[NSMutableDictionary alloc] init];
		_veryLightGreyColor = [[NSColor colorWithCalibratedRed:0.9 green:0.9 blue:0.9 alpha:1.0] retain];
		// [NSColor lightGrayColor]
	}
	NSImage *baseImage = [super direction];
	NSImage *parentImage = [_parentImages objectForKey:baseImage];
	if (!parentImage) {
		// make parent images a grey version of the leaf images
		parentImage = [baseImage tintedImageWithColor:_veryLightGreyColor operation:NSCompositeSourceIn];
		[_parentImages setObject:parentImage forKey:baseImage];
	}
	return parentImage;
}

@end

