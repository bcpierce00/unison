//
//  main.m
//  uimac
//
//  Created by Trevor Jim on Sun Aug 17 2003.
//  Copyright (c) 2003, see file COPYING for details.
//

#import <Cocoa/Cocoa.h>

#define CAML_NAME_SPACE
#include <caml/callback.h>

void reportExn(value e) {
    value *f = caml_named_value("unisonExnInfo");
    char *m = String_val(caml_callback(*f,Extract_exception(e)));
    NSString *s = [NSString stringWithFormat:@"Uncaught exception: %s", m];
    NSLog(@"%@",s);
    NSRunAlertPanel(@"Fatal error",s,@"Exit",nil,nil);
}

value Callback_checkexn(value c,value v) {
    value e = caml_callback_exn(c,v);
    if (!Is_exception_result(e)) return e;
    reportExn(e);
    exit(1);
}

value Callback2_checkexn(value c,value v1,value v2) {
    value e = caml_callback2_exn(c,v1,v2);
    if (!Is_exception_result(e)) return e;
    reportExn(e);
    exit(1);
}

value Callback3_checkexn(value c,value v1,value v2,value v3) {
    value e = caml_callback3_exn(c,v1,v2,v3);
    if (!Is_exception_result(e)) return e;
    reportExn(e);
    exit(1);
}

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
    
    /* Initialize ocaml gc, etc. */
    caml_startup((char **)argv); // cast to avoid warning, caml_startup assumes non-const,
                                 // NSApplicationMain assumes const

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
            NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
            value *f = caml_named_value("unisonNonGuiStartup");
            value e = caml_callback_exn(*f,Val_unit);
            if (Is_exception_result(e)) {
                value *f = caml_named_value("unisonExnInfo");
                char *m = String_val(caml_callback(*f,Extract_exception(e)));
                NSLog(@"Uncaught exception: %s", m);
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
