#import "ReconItem.h"
#include <caml/callback.h>
#include <caml/memory.h>

extern value Callback_checkexn(value,value);
extern value Callback2_checkexn(value,value,value);

@implementation ReconItem

-(void)dealloc
{
    ri = Val_unit;
    caml_remove_global_root(&ri);
    [super dealloc];
}

- (void)setRi:(value)v
{
    caml_register_global_root(&ri); // needed in case of ocaml garbage collection
    ri = v;
    resolved = NO;
}

- (void)setIndex:(int)i
{
    index = i;
}

- init
{
    if ((self = [super init]))
        resolved = NO;

    return self;
}

+ (id)initWithRiAndIndex:(value)v index:(int)i
{
    ReconItem *r = [[ReconItem alloc] init];
    [r setRi:v];
    [r setIndex:i];
    return r;
}

- (NSString *)path
{
    if (path) return path;
    
    value *f = caml_named_value("unisonRiToPath");
    [path release];
    path = [NSString stringWithCString:String_val(Callback_checkexn(*f, ri))];
    [path retain];
    return path;
}

- (NSString *)left
{
    if (left) return left;
    
    value *f = caml_named_value("unisonRiToLeft");
    [left release];
    left = [NSString stringWithCString:String_val(Callback_checkexn(*f, ri))];
    [left retain];
    return left;
}

- (NSString *)right
{
    if (right) return right;
    
    value *f = caml_named_value("unisonRiToRight");
    [right release];
    right = [NSString stringWithCString:String_val(Callback_checkexn(*f, ri))];
    [right retain];
    return right;
}

- (NSImage *)direction
{
    if (direction) return direction;
    
    value *f = caml_named_value("unisonRiToDirection");
    value v = Callback_checkexn(*f, ri);
    char *s = String_val(v);
    [direction release];
    NSString * dirString = [NSString stringWithCString:s];

    BOOL changedFromDefault = [self changedFromDefault];
    
    if ([dirString isEqual:@"<-?->"]) {
        if (changedFromDefault | resolved)
            direction = [NSImage imageNamed: @"table-skip.tif"];
        else
            direction = [NSImage imageNamed: @"table-conflict.tif"];
    }
    
    else if ([dirString isEqual:@"---->"]) {
        if (changedFromDefault)
            direction = [NSImage imageNamed: @"table-right-blue.tif"];
        else
            direction = [NSImage imageNamed: @"table-right-green.tif"];
    }
    
    else if ([dirString isEqual:@"<----"]) {
        if (changedFromDefault)
            direction = [NSImage imageNamed: @"table-left-blue.tif"];
        else
            direction = [NSImage imageNamed: @"table-left-green.tif"];
    }

    else if ([dirString isEqual:@"<-M->"])
        direction = [NSImage imageNamed: @"table-merge.tif"];
    else
        direction = [NSImage imageNamed: @"table-error.tif"];
    
    [direction retain];
    return direction;
}

- (void)setDirection:(char *)d
{
    [direction release];
    direction = nil;
    value *f = caml_named_value(d);
    Callback_checkexn(*f, ri);
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
    value *f;
    switch (action) {
    case 'I':
        f = caml_named_value("unisonIgnorePath");
        Callback_checkexn(*f, ri);
        break;
    case 'E':
        f = caml_named_value("unisonIgnoreExt");
        Callback_checkexn(*f, ri);
        break;
    case 'N':
        f = caml_named_value("unisonIgnoreName");
        Callback_checkexn(*f, ri);
        break;
    default:
        NSLog(@"ReconItem.doIgnore : unknown ignore");
        break;
    }
}

- (NSString *)progress
{
    if (progress) return progress;
    
    value *f = caml_named_value("unisonRiToProgress");
    progress = [NSString stringWithCString:String_val(Callback_checkexn(*f, ri))];
    [progress retain];
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
    
    value *f = caml_named_value("unisonRiToDetails");
    details = [NSString stringWithCString:String_val(Callback_checkexn(*f, ri))];
    [details retain];
    return details;
}

- (BOOL)isConflict
{
    value *f = caml_named_value("unisonRiIsConflict");
    if (Callback_checkexn(*f, ri) == Val_true) return YES;
    else return NO;
}

- (BOOL)changedFromDefault
{
    value *f = caml_named_value("changedFromDefault");
    if (Callback_checkexn(*f, ri) == Val_true) return YES;
    else return NO;
}

- (void)revertDirection
{
    value *f = caml_named_value("unisonRiRevert");
    Callback_checkexn(*f, ri);
    [direction release];
    direction = nil;
    resolved = NO;
}

- (BOOL)canDiff
{
    value *f = caml_named_value("canDiff");
    if (Callback_checkexn(*f, ri) == Val_true) return YES;
    else return NO;
}

- (void)showDiffs
{
    value *f = caml_named_value("runShowDiffs");
    Callback2_checkexn(*f, ri, Val_int(index));
}

@end
