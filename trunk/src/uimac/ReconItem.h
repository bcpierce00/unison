/* ReconItem */

#import <Cocoa/Cocoa.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

@interface ReconItem : NSObject
{
    NSString *path;
    NSString *left;
    NSString *right;
    NSImage *direction;
    NSString *progress;
    NSString *details;
    value ri; // an ocaml Common.reconItem
    value index; // ocaml value indicating index in Ri list
    BOOL resolved;
}
+ (id)initWithRiAndIndex:(value)v index:(int)i;
- (NSString *) path;
- (NSString *) left;
- (NSString *) right;
- (NSImage *) direction;
- (void) doAction:(unichar)action;
- (void) doIgnore:(unichar)action;
- (NSString *) progress;
- (void)resetProgress;
- (NSString *) details;
- (BOOL)isConflict;
- (BOOL)changedFromDefault;
- (void)revertDirection;
- (BOOL)canDiff;
- (void)showDiffs;

@end
