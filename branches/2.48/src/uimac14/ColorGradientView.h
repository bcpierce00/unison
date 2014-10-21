//
//  ColorGradientView.h
//  uimacnew
//
//  From http://www.katoemba.net/makesnosenseatall/2008/01/09/nsview-with-gradient-background/
//

#import <Cocoa/Cocoa.h>

@interface ColorGradientView : NSView
{
    NSColor *startingColor;
    NSColor *endingColor;
    int angle;
}

// Define the variables as properties
@property(nonatomic, retain) NSColor *startingColor;
@property(nonatomic, retain) NSColor *endingColor;
@property(assign) int angle;

@end
