//
//  Bridge.h
//  uimac
//
//  Created by Craig Federighi on 4/25/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//
#import <Cocoa/Cocoa.h>

/*
	Bridge supports safe calling from C back to OCaml by using daemon threads
	spawned from OCaml to make the actual calls and converting all argument / return values
	in the OCaml thread (when in possession of the OCaml lock)
*/
@interface Bridge : NSObject {
}
+ (void)startup:(const char **)argv;	
@end

/*
	ocamlCall(sig, funcName, [args...]);
	
	Call ocaml function (via safe thread handoff mechanism).
	Args/return values are converted to/from C/OCaml according to the
	supplied type signture string.  Type codes are:
		x	- void (for return type)
		i	- int
		s	- char *
		S	- NSString *
		@	- OCamlValue (see below)
		v	- unwrapped OCaml value (deprecated -- unsafe!)
		
	Examples:
		int count = (int)ocamlCall("iS", "lengthOfString", @"Some String");
		
		(void)ocamlCall("x", "someVoidOCamlFunction");
		
		OCamlValue *v = (id)ocamlCall("@Si", "makeArray", @"Some String", 10);
		NSString s = [v getField:0 withType:'S'];
*/
extern void *ocamlCall(const char *argTypes, ...);

// Wrapper/proxy for unconverted OCaml values
@interface OCamlValue : NSObject {
	int _v;
}
- initWithValue:(int)v;

- (void *)getField:(int)i withType:(char)t;
	// get value by position.  See ocamlCall for list of type conversion codes
	
- (int)count;
	// count of items in array
	
- (int)value;
	// returns Ocaml value directly -- not safe to use except in direct callback from OCaml
	// (i.e. in the OCaml thread)
@end
