#import "PreferencesController.h"
#import "Bridge.h"

@implementation PreferencesController

- (void)reset
{
    [profileNameText setStringValue:@""];
    [firstRootText setStringValue:@""];
    [secondRootUser setStringValue:@""];
    [secondRootHost setStringValue:@""];
    [secondRootText setStringValue:@""];
    [remoteButtonCell setState:NSOnState];
    [localButtonCell setState:NSOffState];
    [secondRootUser setSelectable:YES];
    [secondRootUser setEditable:YES];
    [secondRootHost setSelectable:YES];
    [secondRootHost setEditable:YES];
}

- (BOOL)validatePrefs
{
    NSString *profileName = [profileNameText stringValue];
    if (profileName == nil | [profileName isEqualTo:@""]) {
        // FIX: should check for already existing names too
        NSRunAlertPanel(@"Error",@"You must enter a profile name",@"OK",nil,nil);
        return NO;
    }
    NSString *firstRoot = [firstRootText stringValue];
    if (firstRoot == nil | [firstRoot isEqualTo:@""]) {
        NSRunAlertPanel(@"Error",@"You must enter a first root",@"OK",nil,nil);
        return NO;
    }
    NSString *secondRoot;
    if ([remoteButtonCell state] == NSOnState) {
        NSString *user = [secondRootUser stringValue];
        if (user == nil | [user isEqualTo:@""]) {
            NSRunAlertPanel(@"Error",@"You must enter a user",@"OK",nil,nil);
            return NO;
        }
        NSString *host = [secondRootHost stringValue];
        if (host == nil | [host isEqualTo:@""]) {
            NSRunAlertPanel(@"Error",@"You must enter a host",@"OK",nil,nil);
            return NO;
        }
        NSString *file = [secondRootText stringValue];
        // OK for empty file, e.g., ssh://foo@bar/
        secondRoot = [NSString stringWithFormat:@"ssh://%@@%@/%@",user,host,file];
    }
    else {
        secondRoot = [secondRootText stringValue];
        if (secondRoot == nil | [secondRoot isEqualTo:@""]) {
            NSRunAlertPanel(@"Error",@"You must enter a second root file",@"OK",nil,nil);
            return NO;
        }
    }
	ocamlCall("xSSS", "unisonProfileInit", profileName, firstRoot, secondRoot);
    return YES;
}

/* The target when enter is pressed in any of the text fields */
// FIX: this is broken, it takes tab, mouse clicks, etc.
- (IBAction)anyEnter:(id)sender
{
    NSLog(@"enter");
    [self validatePrefs];
}

- (IBAction)localClick:(id)sender
{
    NSLog(@"local");
    [secondRootUser setStringValue:@""];
    [secondRootHost setStringValue:@""];
    [secondRootUser setSelectable:NO];
    [secondRootUser setEditable:NO];
    [secondRootHost setSelectable:NO];
    [secondRootHost setEditable:NO];
}

- (IBAction)remoteClick:(id)sender
{
    NSLog(@"remote");
    [secondRootUser setSelectable:YES];
    [secondRootUser setEditable:YES];
    [secondRootHost setSelectable:YES];
    [secondRootHost setEditable:YES];
}

@end
