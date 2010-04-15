//
//  Bridge.m
//  uimac
//
//  Created by Craig Federighi on 4/25/07.
//  Copyright 1999-2008 (see COPYING for details)
//

#import "Bridge.h"
#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#import <ExceptionHandling/NSExceptionHandler.h>

#include <pthread.h>
#include <stdarg.h>

/*
 CMF, April 2007:  Alternate strategy for solving UI crashes based on 
 http://alan.petitepomme.net/cwn/2005.03.08.html#9:
 1) Run OCaml in a separate thread from the Cocoa main run loop.
 2) Handle all calls to OCaml as callbacks -- have an OCaml thread
    hang in C-land and use mutexes and conditions to pass control from the
    C calling thread to the OCaml callback thread.
	
 Value Conversion Done in Bridge Thread:
 Value creation/conversion (like calls to caml_named_value or caml_copy_string) 
 or access calls (like Field) need to occur in the OCaml thread.  We do this by
 passing C args for conversion to the bridgeThreadWait() thread.
 
 Example of vulnerability:
 Field(caml_reconItems,j) could dereference caml_reconItems
 when the GC (running independently in an OCaml thread) could be moving it.
*/

pthread_mutex_t init_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t init_cond = PTHREAD_COND_INITIALIZER;
static BOOL doneInit = NO;

pthread_mutex_t global_call_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t global_call_cond = PTHREAD_COND_INITIALIZER;
pthread_mutex_t global_res_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t global_res_cond = PTHREAD_COND_INITIALIZER;

@implementation Bridge
static Bridge *_instance = NULL;

const char **the_argv;

- (void)_ocamlStartup:(id)ignore
{

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   	pthread_mutex_lock(&init_lock);

    /* Initialize ocaml gc, etc. */
    caml_startup((char **)the_argv); // cast to avoid warning, caml_startup assumes non-const,
                                 // NSApplicationMain assumes const
	
	// Register these with the collector
	// NSLog(@"*** _ocamlStartup - back from startup; signalling! (%d)", pthread_self());
    doneInit = TRUE;
	pthread_cond_signal(&init_cond);
   	pthread_mutex_unlock(&init_lock);
			 
	// now start the callback thread
	// NSLog(@"*** _ocamlStartup - calling callbackThreadCreate (%d)", pthread_self());
	value *f = caml_named_value("callbackThreadCreate");
	(void)caml_callback_exn(*f,Val_unit);
    [pool release];
}

+ (void)startup:(const char **)argv
{
	if (_instance) return;
	
	_instance = [[Bridge alloc] init];

	[[NSExceptionHandler defaultExceptionHandler] setDelegate:_instance];
	[[NSExceptionHandler defaultExceptionHandler] setExceptionHandlingMask:
		(NSLogUncaughtExceptionMask  | NSLogTopLevelExceptionMask)];

	// Init OCaml in another thread and wait for it to be ready
   	pthread_mutex_lock(&init_lock);
	the_argv = argv;
	[NSThread detachNewThreadSelector:@selector(_ocamlStartup:)
		toTarget:_instance withObject:nil];

	// NSLog(@"*** waiting for completion of caml_init");
	while (!doneInit) pthread_cond_wait(&init_cond, &init_lock);
   	pthread_mutex_unlock(&init_lock);
	// NSLog(@"*** caml_init complete!");
}

#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_5
typedef unsigned int NSUInteger;
#endif
- (BOOL)exceptionHandler:(NSExceptionHandler *)sender shouldLogException:(NSException *)exception mask:(NSUInteger)aMask
{
	// if (![[exception name] isEqual:@"OCamlException"]) return YES;
	
    NSString *msg = [NSString stringWithFormat:@"Uncaught exception: %@", [exception reason]];
    msg = [[msg componentsSeparatedByString:@"\n"] componentsJoinedByString:@" "];
    NSLog(@"%@", msg);
    NSRunAlertPanel(@"Fatal error", msg, @"Exit", nil, nil);
	exit(1);
	return FALSE;
}

@end


// CallState struct is allocated on the C thread stack and then handed
// to the OCaml callback thread to perform value conversion and issue the call
typedef struct  {
	enum { SafeCall, OldCall, FieldAccess } opCode;
	
	// New style calls
	const char *argTypes;
	va_list args;

	// Field access
	value *valueP;
	long fieldIndex;
	char fieldType;
	
	// Return values
	char *exception;
	void *retV;
	BOOL _autorelease;
	
	// for old style (unsafe) calls
	value call, a1, a2, a3, ret;
	int argCount;
} CallState;

static CallState *_CallState = NULL;
static CallState *_RetState = NULL;

// Our OCaml callback server thread -- waits for call then makes them
// Called from thread spawned from OCaml
CAMLprim value bridgeThreadWait(value ignore)
{
        CAMLparam0();
        CAMLlocal1 (args);
        args = caml_alloc_tuple(3);
	
	// NSLog(@"*** bridgeThreadWait init!  (%d) Taking lock...", pthread_self());
	while (TRUE) {
		// unblock ocaml while we wait for work
		caml_enter_blocking_section();
		
		pthread_mutex_lock(&global_call_lock);
		while (!_CallState) pthread_cond_wait(&global_call_cond, &global_call_lock);

		// pick up our work and free up the call lock for other threads
		CallState *cs = _CallState;
		_CallState = NULL;		
		pthread_mutex_unlock(&global_call_lock);
		
     	// NSLog(@"*** bridgeThreadWait: have call -- leaving caml_blocking_section");
		
		// we have a call to do -- get the ocaml lock
		caml_leave_blocking_section();
		
    	// NSLog(@"*** bridgeThreadWait: doing call");

        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

		char retType = 'v';
		value e = Val_unit;
		if (cs->opCode == SafeCall) {
                        int i;
			char *fname = va_arg(cs->args, char *);
			value *f = caml_named_value(fname);
			// varargs with C-based args -- convert them to OCaml values based on type code string
			const char *p = cs->argTypes;
			retType = *p++;
			int argCount = 0;
			for(; *p != '\0'; p++) {
				const char *str;
				switch (*p) {
					case 's':
						str = va_arg(cs->args, const char *);
						Store_field (args, argCount, caml_copy_string(str));
						break;
					case 'S':
						str = [va_arg(cs->args, NSString *) UTF8String];
						Store_field (args, argCount, caml_copy_string(str));
						break;
					case 'i':
                                                Store_field (args, argCount, Val_long(va_arg(cs->args, long)));
						break;
					case '@':
                                                Store_field (args, argCount, [va_arg(cs->args, OCamlValue *) value]);
						break;
                                        default:
                                                NSCAssert1(0, @"Unknown input type '%c'", *p);
						break;
				}
				argCount++;
                                NSCAssert(argCount <= 3, @"More than 3 arguments");
			}
			// Call OCaml -- TODO: add support for > 3 args
			if (argCount == 3) e = caml_callback3_exn(*f,Field(args,0),Field(args,1),Field(args,2));
			else if (argCount == 2) e = caml_callback2_exn(*f,Field(args,0),Field(args,1));
			else if (argCount == 1) e = caml_callback_exn(*f,Field(args,0));
			else e = caml_callback_exn(*f,Val_unit);			
                        for (i = 0; i < argCount; i++) Store_field (args, i, Val_unit);
		} else if (cs->opCode == OldCall) {
			// old style (unsafe) version where OCaml values were passed directly from C thread
			if (cs->argCount == 3) e = caml_callback3_exn(cs->call,cs->a1,cs->a2,cs->a3);
			else if (cs->argCount == 2) e = caml_callback2_exn(cs->call,cs->a1,cs->a2);
			else e = caml_callback_exn(cs->call,cs->a1);
			retType = 'v';
		} else if (cs->opCode == FieldAccess) {
			long index = cs->fieldIndex;
			e = (index == -1) ? Val_long(Wosize_val(*cs->valueP)) : Field(*cs->valueP, index);
			retType = cs->fieldType;
		}
		
		// Process return value
		cs->_autorelease = FALSE;
		cs->ret = e; // OCaml return type -- unsafe...
		if (!Is_exception_result(e)) {
			switch (retType) {
				case 'S':
					*((NSString **)&cs->retV) = (e == Val_unit) ? NULL : [[NSString alloc] initWithUTF8String:String_val(e)];
					cs->_autorelease = TRUE;
					break;
				case 'N':
                                        if (Is_long (e)) {
					        *((NSNumber **)&cs->retV) = [[NSNumber alloc] initWithLong:Long_val(e)];
                                        } else {
					        *((NSNumber **)&cs->retV) = [[NSNumber alloc] initWithDouble:Double_val(e)];
                                        }
					cs->_autorelease = TRUE;
					break;
				case '@':
					*((NSObject **)&cs->retV) = (e == Val_unit) ? NULL : [[OCamlValue alloc] initWithValue:e];
					cs->_autorelease = TRUE;
					break;
				case 'i':
					*((long *)&cs->retV) = Long_val(e);
					break;
				case 'x':
                                        break;
                                default:
                                        NSCAssert1(0, @"Unknown return type '%c'", retType);
                                        break;
			}
		}

		if (Is_exception_result(e)) {
			// get exception string -- it will get thrown back in the calling thread
		    value *f = caml_named_value("unisonExnInfo");
                    // We leak memory here...
                    cs->exception = strdup(String_val(caml_callback(*f,Extract_exception(e))));
		}
		
 	    [pool release];

    	// NSLog(@"*** bridgeThreadWait: returning");

		// we're done, signal back
		pthread_mutex_lock(&global_res_lock);
		_RetState = cs;
		pthread_cond_signal(&global_res_cond);
		pthread_mutex_unlock(&global_res_lock);
	}
	// Never get here...
        CAMLreturn (Val_unit);
}

void *_passCall(CallState *cs)
{
    pthread_mutex_lock(&global_call_lock);
	_CallState = cs;

	// signal so call can happen on other thread
	pthread_mutex_lock(&global_res_lock);
	pthread_cond_signal(&global_call_cond);
	pthread_mutex_unlock(&global_call_lock);

	// NSLog(@"*** _passCall (%d) -- performing signal and waiting", pthread_self());

	// wait until done -- make sure the result is for our call
	while (_RetState != cs) pthread_cond_wait(&global_res_cond, &global_res_lock);
	_RetState = NULL;
	pthread_mutex_unlock(&global_res_lock);

	// NSLog(@"*** doCallback -- back with result");
	if (cs->exception) {
		@throw [NSException exceptionWithName:@"OCamlException"
				reason:[NSString stringWithUTF8String:cs->exception]
				userInfo:nil];
	}
	if (cs->_autorelease) [((id)cs->retV) autorelease];
    return cs->retV;
}

void *ocamlCall(const char *argTypes, ...)
{
	CallState cs;
	cs.opCode = SafeCall;
	cs.exception = NULL;
	cs.argTypes = argTypes;
	va_start(cs.args, argTypes);
	void * res = _passCall(&cs);
	
	va_end(cs.args);
	return res;
}

void *getField(value *vP, long index, char type)
{
	CallState cs;
	cs.opCode = FieldAccess;
	cs.valueP = vP;
	cs.fieldIndex = index;
	cs.fieldType = type;
	cs.exception = NULL;
	return _passCall(&cs);
}

@implementation OCamlValue

- initWithValue:(long)v
{
	[super init];
	_v = v;
	caml_register_global_root((value *)&_v);
	return self;
}

- (long)count
{
        return (long)getField((value *) &_v, -1, 'i');
}

- (void *)getField:(long)i withType:(char)t
{
        return getField((value *)&_v, i, t);
}

- (long)value
{
	// Unsafe to use!
	return _v;
}

- (void)dealloc
{
	_v = Val_unit;
    caml_remove_global_root((value *)&_v);
	[super dealloc];
}
@end


// Legacy OCaml call API -- no longer needed
#if 0

extern value doCallback (value c, int argcount, value v1, value v2, value v3, BOOL exitOnException);
extern value Callback_checkexn(value c,value v);
extern value Callback2_checkexn(value c,value v1,value v2);
extern value Callback3_checkexn(value c,value v1,value v2,value v3);

void reportExn(const char *msg) {
    NSString *s = [NSString stringWithFormat:@"Uncaught exception: %s", msg];
    s = [[s componentsSeparatedByString:@"\n"] componentsJoinedByString:@" "];
    NSLog(@"%@",s);
    NSRunAlertPanel(@"Fatal error",s,@"Exit",nil,nil);
}

// FIXME!  Claim is that value conversion must also happen in the OCaml thread...
value doCallback (value c, int argcount, value v1, value v2, value v3, BOOL exitOnException) {
	// NSLog(@"*** doCallback: (%d) -- trying to acquire global lock", pthread_self());
	CallState cs;
	cs.opCode = OldCall;
	cs.exception = NULL;
	cs.call = c;
	cs.a1 = v1;
	cs.a2 = v2;
	cs.a3 = v3;
	cs.argCount = argcount;
	@try {
		return _passCall(&cs);
	} @catch (NSException *ex) {
		if (exitOnException) {
			reportExn(cs.exception);
			exit(1);
		}
		@throw ex;
	}
}

value Callback_checkexn(value c,value v) {
    return doCallback(c, 1, v, 0, 0, TRUE);
}

value Callback2_checkexn(value c,value v1,value v2) {
    return doCallback(c, 2, v1, v2, 0, TRUE);
}

value Callback3_checkexn(value c,value v1,value v2,value v3) {
    return doCallback(c, 3, v1, v2, v3, TRUE);
}
#endif
