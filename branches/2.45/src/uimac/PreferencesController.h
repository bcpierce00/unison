/* PreferencesController */

#import <Cocoa/Cocoa.h>

@interface PreferencesController : NSObject
{
    IBOutlet NSButton *cancelButton;
    IBOutlet NSTextField *firstRootText;
    IBOutlet NSButtonCell *localButtonCell;
    IBOutlet NSTextField *profileNameText;
    IBOutlet NSButtonCell *remoteButtonCell;
    IBOutlet NSButton *saveButton;
    IBOutlet NSTextField *secondRootHost;
    IBOutlet NSTextField *secondRootText;
    IBOutlet NSTextField *secondRootUser;
}
- (IBAction)anyEnter:(id)sender;
- (IBAction)localClick:(id)sender;
- (IBAction)remoteClick:(id)sender;
- (BOOL)validatePrefs;
- (void)reset;
@end
