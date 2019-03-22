//
//  NotificationController.m
//  uimac
//
//  Created by Alan Schmitt on 02/02/06.
//  Copyright 2006, see file COPYING for details. All rights reserved.
//

#import "NotificationController.h"

#define NOTIFY_UPDATE   @"Scan finished"
#define NOTIFY_SYNC     @"Synchronization finished"

/* Show a simple notification */
static void simpleNotify(NSString *name, NSString *descFmt, NSString *profile);

@implementation NotificationController

- (void)awakeFromNib
{
    [[NSUserNotificationCenter defaultUserNotificationCenter] setDelegate:self];
}

- (void)updateFinishedFor: (NSString *)profile
{
    simpleNotify(NOTIFY_UPDATE,
                 @"Profile '%@' is finished scanning for updates",
                 profile);
}

- (void)syncFinishedFor: (NSString *)profile {
    simpleNotify(NOTIFY_SYNC,
                 @"Profile '%@' is finished synchronizing",
                 profile);
}

- (BOOL)userNotificationCenter:(NSUserNotificationCenter *)center shouldPresentNotification:(NSUserNotification *)notification{
    return YES;
}

@end

static void simpleNotify(NSString *name, NSString *descFmt, NSString *profile)
{
    NSUserNotification *notification = [[NSUserNotification alloc] init];
    notification.title = name;
    notification.informativeText = [NSString stringWithFormat:descFmt, profile];
    notification.soundName = NSUserNotificationDefaultSoundName;
    [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:notification];
}