//
//  PZLockDescriptor.m
//  pixelzoo
//
//  Created by Ian Holmes on 10/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZLockDescriptor.h"
#import "PZDefs.h"
#import "PZAppDelegate.h"

@implementation PZLockDescriptor

@synthesize worldDescriptor;

-(void)initFromLockData:(NSData*)lockData forWorld:(PZWorldDescriptor*)world {
    NSError * error = nil;
    worldDescriptor = world;
    lockDoc = [[GDataXMLDocument alloc] initWithData:lockData
                                             options:0 error:&error];
}

-(GDataXMLElement*)gameXMLElement {
    NSArray *gamesArray = [lockDoc nodesForXPath:@"//lock/world/game" error:nil];
    GDataXMLElement *gameElement = [gamesArray objectAtIndex:0];
    return gameElement;
}

- (NSString*)identifier {
    NSArray *ids = [lockDoc nodesForXPath:@"//lock/id" error:nil];
    GDataXMLElement *idElement = [ids objectAtIndex:0];
    return [idElement stringValue];
}

-(void)deleteLock {
    NSMutableURLRequest *request = [[NSMutableURLRequest alloc] init];
    request.URL = [NSURL URLWithString:[NSString stringWithFormat:@"%@/world/%@/lock/%@",@SERVER_URL_PREFIX,[worldDescriptor identifier],[self identifier]]];
    request.HTTPMethod = @"DELETE";
    PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
    [appDelegate addStoredBasicAuthHeader:request];
    deleteConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}


#pragma mark NSURLConnectionDelegate methods


- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    NSHTTPURLResponse *httpLockResponse = (NSHTTPURLResponse*)response;
    if ([httpLockResponse statusCode] == 204) {  // 204 NO CONTENT (success)
        lockDoc = nil;
    }
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // nothing to see here
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // nothing to see here
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}


@end
