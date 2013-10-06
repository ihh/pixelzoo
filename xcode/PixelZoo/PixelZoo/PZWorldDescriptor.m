//
//  PZWorldDescriptor.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZWorldDescriptor.h"
#import "GDataXMLNode.h"

@implementation PZWorldDescriptor

@synthesize worldNode;

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

@end
