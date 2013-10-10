//
//  PZWorldDescriptor.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZDefs.h"
#import "PZWorldDescriptor.h"
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

- (NSMutableURLRequest*)getController:(NSString *)suffix {
    NSMutableURLRequest *request = [[NSMutableURLRequest alloc] init];
    request.URL = [NSURL URLWithString:[NSString stringWithFormat:@"%@/world/%@/%@",@SERVER_URL_PREFIX,[self identifier],suffix]];
    return request;
}

- (NSString*)toolBoxXpath:(NSString*)suffix {
    NSMutableString* str = [[NSMutableString alloc] initWithString:@"toolbox/"];
    [str appendString:@"owner"];  // hardcode this for now; in future, will check ownership & use 'owner' or 'guest'
    [str appendString:suffix];
    return str;
}

- (NSString*)toolboxName {
    if (!statusNode) return 0;
    return [[[statusNode nodesForXPath:[self toolBoxXpath:@"/name"] error:nil] objectAtIndex:0] stringValue];
}

- (int)numberOfTools {
    if (!statusNode) return 0;
    return [[statusNode nodesForXPath:[self toolBoxXpath:@"/tool"] error:nil] count];
}

- (NSString*)getToolID:(int)num {
    if (!statusNode) return @"none";
    return [[[statusNode nodesForXPath:[self toolBoxXpath:@"/tool/id"] error:nil] objectAtIndex:num] stringValue];
}

- (NSString*)getToolName:(int)num {
    if (!statusNode) return @"";
    return [[[statusNode nodesForXPath:[self toolBoxXpath:@"/tool/name"] error:nil] objectAtIndex:num] stringValue];
}

@end
