/* ReconItem */

#import <Cocoa/Cocoa.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

@interface ReconItem : NSObject
{
    NSString *path;
    NSString *left;
    NSString *right;
    NSString *direction;
    NSString *progress;
    NSString *details;
    value ri; // an ocaml Common.reconItem
}
+ initWithRi:(value)ri;
- (NSString *) path;
- (NSString *) left;
- (NSString *) right;
- (NSString *) direction;
- (void) doAction:(unichar)action;
- (void) doIgnore:(unichar)action;
- (NSString *) progress;
- (void)resetProgress;
- (NSString *) details;
- (BOOL)isConflict;
- (void)revertDirection;

@end
