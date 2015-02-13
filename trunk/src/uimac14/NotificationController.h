//
//  NotificationController.h
//  uimac
//
//  Created by Alan Schmitt on 02/02/06.
//  Copyright 2006, see file COPYING for details. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface NotificationController : NSObject <NSApplicationDelegate, NSUserNotificationCenterDelegate>
{
}

- (void)updateFinishedFor: (NSString *)profile;
- (void)syncFinishedFor: (NSString *)profile;

@end
