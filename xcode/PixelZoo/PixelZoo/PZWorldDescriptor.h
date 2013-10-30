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

// the following methods require statusNode to be loaded, i.e. they can only be used from the status screen or beyond
- (NSString*)owner;

- (bool)userIsOwner;
- (bool)isLocked;
- (NSString*)lockOwner;
- (bool)userOwnsLock;
- (NSInteger)lockExpiryWait;
- (bool)lockedOut;
- (NSInteger)lockDeleteWait;

- (NSMutableURLRequest*)getRequest:(NSString*)controllerSuffix;
- (NSMutableURLRequest*)postRequest:(NSString*)controllerSuffix withContent:(NSString*)content;

- (NSArray*)toolboxXPath:(NSString*)suffix;

- (NSString*)toolboxName;
- (int)numberOfTools;
- (int)maxSelectableTools;
- (NSString*)getToolID:(int)num;
- (NSString*)getToolName:(int)num;
- (NSMutableArray*)defaultToolIDs;

- (void) setWorldLabel:(UILabel*)worldLabel;

@end
