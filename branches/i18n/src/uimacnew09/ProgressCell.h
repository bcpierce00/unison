#import <Cocoa/Cocoa.h>

@interface ProgressCell : NSCell
{
	float _minVal, _maxVal;  // defaults to 0.0, 100.0
	BOOL _isActive;
	BOOL _useFullView;       // default: NO
	BOOL _isError;           // default: NO
	NSImage *_icon;
	NSString *_statusString;
}
- (void)setStatusString:(NSString *)string;
- (void)setIcon:(NSImage *)image;
- (void)setIsActive:(BOOL)yn;
@end
