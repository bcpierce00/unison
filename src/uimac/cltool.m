/* cltool.m

   This is a command-line tool for Mac OS X that looks up the unison
   application, where ever it has been installed, and runs it.  This
   is intended to be installed in a standard place (e.g.,
   /usr/bin/unison) to make it easy to invoke unison as a server, or
   to use unison from the command line when it has been installed with
   a GUI.

 */

#import <Foundation/NSString.h>
#import <Foundation/NSAutoreleasePool.h>
#import <AppKit/NSWorkspace.h>

#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  /* Look up the application by its bundle identifier, which is given
     in the Info.plist file.  This will continue to work even if the
     user changes the name of the application, unlike
     fullPathForApplication. */

  NSString *app_path =
    [[NSWorkspace sharedWorkspace] 
      absolutePathForAppBundleWithIdentifier:@"edu.upenn.cis.Unison"];

  if (app_path == nil) {
    fprintf(stderr,"Error: can't find Unison.app.\n");
    fprintf(stderr,"To fix this, launch the Unison application from\n");
    fprintf(stderr,"the Finder, and try again.\n");
    exit(1);
  }

  NSString *executable_path =
    [app_path stringByAppendingString:@"/Contents/MacOS/Unison"];

  /* It's important to pass the absolute path on to the GUI,
     that's how it knows where to find the bundle, e.g., the
     Info.plist file. */
  argv[0] = (char *)[executable_path cString];

  /*
  NSLog(@"The Unison app is at %@\n",app_path);
  NSLog(@"The Unison executable is at %@\n",executable_path);
  printf("The Unison executable is at %s\n",argv[0]);
  printf("Running...\n");
  */

  execv(argv[0],argv);

  /* If we get here the execv has failed,
     print an error message to stderr */
  perror("unison");
  [pool release];
  exit(1);
}
