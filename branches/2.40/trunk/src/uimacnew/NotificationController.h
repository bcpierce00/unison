//
//  NotificationController.h
//  uimac
//
//  Created by Alan Schmitt on 02/02/06.
//  Copyright 2006, see file COPYING for details. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Growl/Growl.h>

@interface NotificationController : NSObject <GrowlApplicationBridgeDelegate>
{
}

- (void)updateFinishedFor: (NSString *)profile;
- (void)syncFinishedFor: (NSString *)profile;

/* Implement the GrowlApplicationBridgeDelegate protocol */
- (NSDictionary *)registrationDictionaryForGrowl;
- (NSString *)applicationNameForGrowl;

@end
