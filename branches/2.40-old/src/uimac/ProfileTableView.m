#import "ProfileTableView.h"

@implementation ProfileTableView

- (void)keyDown:(NSEvent *)event
{
    unichar c = [[event characters] characterAtIndex:0];
    switch (c) {
    case '\r':
        [myController openButton:self];
        break;
    default:
        [super keyDown:event];
        break;
    }
}

@end
