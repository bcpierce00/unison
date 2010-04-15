#import <Foundation/Foundation.h>
#include <CoreServices/CoreServices.h>

void myCallbackFunction(
    ConstFSEventStreamRef streamRef,
    void *clientCallBackInfo,
    size_t numEvents,
    void *eventPaths,
    const FSEventStreamEventFlags eventFlags[],
    const FSEventStreamEventId eventIds[])
{
    int i;
    char **paths = eventPaths;
 
    // printf("Callback called\n");
    for (i=0; i<numEvents; i++) {
        /* flags are unsigned long, IDs are uint64_t
        printf("Change %llu in %s, flags %lu\n", eventIds[i], paths[i], eventFlags[i]); */
		if (eventFlags[i] == kFSEventStreamEventFlagMustScanSubDirs) {
			printf("*%s\n",paths[i]);
		} else {
			printf(".%s\n",paths[i]);
		}
   }
}


int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	int i;
	CFMutableArrayRef pathsToWatch;
	
	if (argc == 1) {
		exit(0);
	}
	
	/* Define variables and create a CFArray object containing
       CFString objects containing paths to watch.
	   
	   There may be a simpler or nicer way to do this than using a mutable array, but I don't know it.
     */
	
	pathsToWatch = CFArrayCreateMutable(NULL, argc - 1, &kCFTypeArrayCallBacks);
	
	for (i=1; i<argc; i++) {
		CFArrayAppendValue(pathsToWatch, CFStringCreateWithCString(NULL, argv[i], kCFStringEncodingUTF8));
	}
	
    void *callbackInfo = NULL; // could put stream-specific data here.
    FSEventStreamRef stream;
    CFAbsoluteTime latency = 1.0; /* Latency in seconds */
 
    /* Create the stream, passing in a callback */
    stream = FSEventStreamCreate(NULL,
        &myCallbackFunction,
        callbackInfo,
        pathsToWatch,
        kFSEventStreamEventIdSinceNow, /* Or a previous event ID */
        latency,
        kFSEventStreamCreateFlagNone /* Flags explained in reference */
    );

    /* Create the stream before calling this. */
    FSEventStreamScheduleWithRunLoop(stream, CFRunLoopGetCurrent(),         kCFRunLoopDefaultMode);
	
	FSEventStreamStart(stream);
		
	CFRunLoopRun();
		
    [pool drain];
    return 0;
}
