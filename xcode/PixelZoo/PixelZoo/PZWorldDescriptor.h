//
//  PZWorldDescriptor.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "GDataXMLNode.h"

@interface PZWorldDescriptor : NSObject

@property (nonatomic, strong) GDataXMLNode *worldNode;
@property (nonatomic, strong) GDataXMLNode *statusNode;

- (id)initWithNode:(GDataXMLNode *)node;

- (NSString*)name;
- (NSString*)identifier;

- (NSMutableURLRequest*)getController:(NSString*)suffix;

- (NSString*)toolBoxXpath:(NSString*)suffix;

- (NSString*)toolboxName;
- (int)numberOfTools;
- (NSString*)getToolID:(int)num;
- (NSString*)getToolName:(int)num;


@end
