#import "MyController.h"
#import "ProfileTableView.h"

@implementation ProfileTableView

- (void)keyDown:(NSEvent *)event
{
    /* some keys return zero-length strings */
    if ([[event characters] length] == 0) {
        [super keyDown:event];
        return;
    }

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

/* Override default highlight colour to match ReconTableView */
- (id)_highlightColorForCell:(NSCell *)cell
{   
    if(([[self window] firstResponder] == self) &&
        [[self window] isMainWindow] &&
        [[self window] isKeyWindow])

        return [NSColor colorWithCalibratedRed:0.7 green:0.75 blue:0.8 alpha:1.0];
    else return [NSColor colorWithCalibratedRed:0.8 green:0.8 blue:0.8 alpha:1.0];
}

@end
