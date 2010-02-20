/* cltool.c

   This is a command-line tool for Mac OS X that looks up the unison
   application, where ever it has been installed, and runs it.  This
   is intended to be installed in a standard place (e.g.,
   /usr/bin/unison) to make it easy to invoke unison as a server, or
   to use unison from the command line when it has been installed with
   a GUI.

 */

#import <CoreServices/CoreServices.h>
#import <ApplicationServices/ApplicationServices.h>
#include <stdio.h>

#define BUFSIZE 1024
#define EXECPATH "/Contents/MacOS/Unison"

int main(int argc, char **argv) {

  /* Look up the application by its bundle identifier, which is given
     in the Info.plist file.  This will continue to work even if the
     user changes the name of the application, unlike
     fullPathForApplication. */

  FSRef fsref;
  OSStatus status;
  int len;
  char buf[BUFSIZE];

  status = LSFindApplicationForInfo(kLSUnknownCreator,CFSTR("edu.upenn.cis.Unison"),NULL,&fsref,NULL);
  if (status) {
    if (status == kLSApplicationNotFoundErr) {
      fprintf(stderr,"Error: can't find the Unison application using the Launch Services database.\n");
      fprintf(stderr,"Try launching Unison from the Finder, and then try this again.\n",status);
    }
    else fprintf(stderr,"Error: can't find Unison application (%d).\n",status);
    exit(1);
  }

  status = FSRefMakePath(&fsref,(UInt8 *)buf,BUFSIZE);
  if (status) {
    fprintf(stderr,"Error: problem building path to Unison application (%d).\n",status);
    exit(1);
  }

  len = strlen(buf);
  if (len + strlen(EXECPATH) + 1 > BUFSIZE) {
    fprintf(stderr,"Error: path to Unison application exceeds internal buffer size (%d).\n",BUFSIZE);
    exit(1);
  }
  strcat(buf,EXECPATH);

  /* It's important to pass the absolute path on to the GUI,
     that's how it knows where to find the bundle, e.g., the
     Info.plist file. */
  argv[0] = buf;

  // printf("The Unison executable is at %s\n",argv[0]);
  // printf("Running...\n");

  execv(argv[0],argv);

  /* If we get here the execv has failed; print an error message to stderr */
  perror("unison");
  exit(1);
}
