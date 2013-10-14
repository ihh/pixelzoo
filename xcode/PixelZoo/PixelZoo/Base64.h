//
//  Base64.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/14/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Base64 : NSObject

+ (NSString *)encode:(NSData *)plainText;
+ (void) addBasicAuthHeader:(NSMutableURLRequest*)request forUser:(NSString*)user withPass:(NSString*)pass;

@end
