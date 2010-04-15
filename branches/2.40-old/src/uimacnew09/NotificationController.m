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
    [GrowlApplicationBridge setGrowlDelegate:self];
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

- (NSDictionary *)registrationDictionaryForGrowl
{
    NSArray* notifications = [NSArray arrayWithObjects:
        NOTIFY_UPDATE,
        NOTIFY_SYNC,
        nil];
    return [NSDictionary dictionaryWithObjectsAndKeys:
        notifications,      GROWL_NOTIFICATIONS_ALL,
        notifications,      GROWL_NOTIFICATIONS_DEFAULT,
        nil];
}

- (NSString *)applicationNameForGrowl
{
    return @"Unison";
}

@end

static void simpleNotify(NSString *name, NSString *descFmt, NSString *profile)
{
    [GrowlApplicationBridge notifyWithTitle:name
                                description:[NSString stringWithFormat:descFmt, profile]
                           notificationName:name
                                   iconData:nil
                                   priority:0
                                   isSticky:false
                               clickContext:nil];
}