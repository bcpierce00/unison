#import "ReconItem.h"
#include <caml/callback.h>
#include <caml/memory.h>

extern value Callback_checkexn(value,value);

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
}

+ (id)initWithRi:(value)v
{
    ReconItem *r = [[ReconItem alloc] init];
    [r setRi:v];
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

- (NSString *)direction
{
    if (direction) return direction;
    
    value *f = caml_named_value("unisonRiToDirection");
    value v = Callback_checkexn(*f, ri);
    char *s = String_val(v);
    [direction release];
    direction = [NSString stringWithCString:s];
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

- (void)revertDirection
{
    value *f = caml_named_value("unisonRiRevert");
    Callback_checkexn(*f, ri);
    [direction release];
    direction = nil;
}

@end
