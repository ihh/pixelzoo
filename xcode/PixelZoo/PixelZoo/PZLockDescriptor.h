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

@interface NSObject(LockDeletionDelegate)
- (void)lockWasDeleted;
@end

@interface PZLockDescriptor : NSObject <NSURLConnectionDelegate> {
    GDataXMLDocument *lockDoc;
    NSURLConnection *deleteConnection;
}

@property (strong, atomic) PZWorldDescriptor *worldDescriptor;
@property (strong, atomic) id lockDeletionDelegate;

-(void)initFromLockData:(NSData*)lockData forWorld:(PZWorldDescriptor*)world;
-(GDataXMLElement*)gameXMLElement;

- (NSString*)identifier;
- (NSInteger)lockExpiryWait;

-(void)deleteLock;

@end
