//
//  PZLockDescriptor.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "GDataXMLNode.h"
#import "PZWorldDescriptor.h"

@interface PZLockDescriptor : NSObject <NSURLConnectionDelegate> {
    GDataXMLDocument *lockDoc;
    NSURLConnection *deleteConnection;
}

@property (strong, atomic) PZWorldDescriptor *worldDescriptor;

-(void)initFromLockData:(NSData*)lockData forWorld:(PZWorldDescriptor*)world;
-(GDataXMLElement*)gameXMLElement;

- (NSString*)identifier;

-(void)deleteLock;

@end
