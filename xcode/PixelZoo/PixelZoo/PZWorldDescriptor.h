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

- (bool)isLocked;
- (NSString*)lockOwner;
- (bool)userOwnsLock;
- (NSInteger)lockExpiryTime;
- (bool)lockedOut;
- (NSInteger)nextLockTime;

- (NSMutableURLRequest*)getRequest:(NSString*)controllerSuffix;
- (NSMutableURLRequest*)postRequest:(NSString*)controllerSuffix withContent:(NSString*)content;

- (NSArray*)toolboxXPath:(NSString*)suffix;

- (NSString*)toolboxName;
- (int)numberOfTools;
- (int)maxSelectableTools;
- (NSString*)getToolID:(int)num;
- (NSString*)getToolName:(int)num;
- (NSMutableArray*)defaultToolIDs;


@end
