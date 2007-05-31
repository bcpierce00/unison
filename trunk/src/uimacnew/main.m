//
//  main.m
//  uimac
//
//  Created by Trevor Jim on Sun Aug 17 2003.
//  Copyright (c) 2003, see file COPYING for details.
//

#import <Cocoa/Cocoa.h>
#import "Bridge.h"

int main(int argc, const char *argv[])
{
    int i;
    
    /* When you click-start or use the open command, the program is invoked with
       a command-line arg of the form -psn_XXXXXXXXX.  The XXXXXXXX is a "process
       serial number" and it seems to be important for Carbon programs.  We need
       to get rid of it if it's there so the ocaml code won't exit.  Note, the
       extra arg is not added if the binary is invoked directly from the command
       line without using the open command. */
    if (argc == 2 && strncmp(argv[1],"-psn_",5) == 0) {
        argc--;
        argv[1] = NULL;
    }
	
	[Bridge startup:argv];
	
    /* Check for invocations that don't start up the gui */
    for (i=1; i<argc; i++) {
        if (!strcmp(argv[i],"-doc") ||
            !strcmp(argv[i],"-help") ||
            !strcmp(argv[i],"-version") ||
            !strcmp(argv[i],"-server") ||
            !strcmp(argv[i],"-socket") ||
            !strcmp(argv[i],"-ui")) {
            /* We install an autorelease pool here because there might be callbacks
               from ocaml to objc code */
			NSLog(@"Calling nonGuiStartup");
            NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
			@try {
				ocamlCall("x", "unisonNonGuiStartup");
			} @catch (NSException *ex) {
				NSLog(@"Uncaught exception: %@", [ex reason]);
				exit(1);
			}
            [pool release];
            /* If we get here without exiting first, the non GUI startup detected a
               -ui graphic or command-line profile, and we should in fact start the GUI. */
        }
    }
	
	/* go! */
    return NSApplicationMain(argc, argv);
}
