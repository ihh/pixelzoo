//
//  PZWorldDescriptor.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZDefs.h"
#import "PZWorldDescriptor.h"
#import "PZAppDelegate.h"
#import "Base64.h"
#import "GDataXMLNode.h"

@implementation PZWorldDescriptor

@synthesize worldNode;
@synthesize statusNode;

- (id)initWithNode:(GDataXMLNode *)node {
    self = [super init];
    worldNode = node;
    return self;
}

- (NSString*)name {
    NSArray *names = [worldNode nodesForXPath:@"name" error:nil];
    GDataXMLElement *nameElement = [names objectAtIndex:0];
    return [nameElement stringValue];
}

- (NSString*)identifier {
    NSArray *ids = [worldNode nodesForXPath:@"id" error:nil];
    GDataXMLElement *idElement = [ids objectAtIndex:0];
    return [idElement stringValue];
}

- (NSMutableURLRequest*)getRequest:(NSString *)controllerSuffix {
    NSMutableURLRequest *request = [[NSMutableURLRequest alloc] init];
    request.URL = [NSURL URLWithString:[NSString stringWithFormat:@"%@/world/%@/%@",@SERVER_URL_PREFIX,[self identifier],controllerSuffix]];

    // add authentication header
    PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
    [appDelegate addStoredBasicAuthHeader:request];
    
    return request;
}

- (NSMutableURLRequest*)postRequest:(NSString*)controllerSuffix withContent:(NSString*)content {
    // start with a GET request URL
    NSMutableURLRequest *request = [self getRequest:controllerSuffix];

    // Specify instead that it will be a POST request
    request.HTTPMethod = @"POST";
    
    // set Content-Type to XML, and set content itself
    [Base64 setContentTypeXML:request withContent:content];
    
    return request;
}

- (NSArray*)toolboxXPath:(NSString*)suffix {
    NSMutableString* str = [[NSMutableString alloc] initWithString:@"toolbox/"];
    [str appendString:@"owner"];  // hardcode this for now; in future, will check ownership & use 'owner' or 'guest'
    [str appendString:suffix];
    return [statusNode nodesForXPath:str error:nil];
}

- (NSString*)toolboxName {
    if (!statusNode) return 0;
    return [[[self toolboxXPath:@"/name"] objectAtIndex:0] stringValue];
}

- (int)numberOfTools {
    if (!statusNode) return 0;
    return [[self toolboxXPath:@"/tool"] count];
}

- (int)maxSelectableTools {
    if (!statusNode) return 0;
    return [[[[self toolboxXPath:@"/max"] objectAtIndex:0] stringValue] integerValue];
}


- (NSString*)getToolID:(int)num {
    if (!statusNode) return @"none";
    return [[[self toolboxXPath:@"/tool/id"] objectAtIndex:num] stringValue];
}

- (NSString*)getToolName:(int)num {
    if (!statusNode) return @"";
    return [[[self toolboxXPath:@"/tool/name"] objectAtIndex:num] stringValue];
}

- (NSMutableArray*)defaultToolIDs {
    NSArray* checked = [self toolboxXPath:@"/tool/checked"];
    NSMutableArray* ids = [[NSMutableArray alloc] init];
    for (int n = 0; n < [self numberOfTools]; ++n)
        if ([[[checked objectAtIndex:n] stringValue] integerValue])
            [ids addObject:[self getToolID:n]];
    return ids;
}

@end
