//
//  Base64.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/14/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

// Contains an implementation of Base64 encoding, and various other HTTP-related helpers.

#import <Foundation/Foundation.h>

@interface Base64 : NSObject

+ (NSString *)encode:(NSData *)plainText;
+ (void) addBasicAuthHeader:(NSMutableURLRequest*)request forUser:(NSString*)user withPass:(NSString*)pass;
+ (void) setContentTypeXML:(NSMutableURLRequest*)postRequest withContent:(NSString*)content;

@end
