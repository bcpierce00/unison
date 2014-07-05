//
//  ColorGradientView.m
//  uimacnew
//
//  From http://www.katoemba.net/makesnosenseatall/2008/01/09/nsview-with-gradient-background/
//
//

#import "ColorGradientView.h"

@implementation ColorGradientView

// Automatically create accessor methods		
@synthesize startingColor;
@synthesize endingColor;
@synthesize angle;

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
        [self setStartingColor:[NSColor gridColor]];
        [self setEndingColor:[NSColor controlShadowColor]];
        [self setAngle:270];
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    if (endingColor == nil || [startingColor isEqual:endingColor]) {
        // Fill view with a standard background color
        [startingColor set];
        NSRectFill(rect);
    }
    else {
        // Fill view with a top-down gradient
        // from startingColor to endingColor
        NSGradient* aGradient = [[NSGradient alloc]
                                 initWithStartingColor:startingColor
                                 endingColor:endingColor];
        [aGradient drawInRect:[self bounds] angle:angle];
	[aGradient release];
    }
}

@end
