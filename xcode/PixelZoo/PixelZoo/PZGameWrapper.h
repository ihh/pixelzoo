//
//  PZGameWrapper.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "pixelzoo.h"
#import "GDataXMLNode.h"

@interface PZGameWrapper : NSObject

@property (nonatomic) pzGame *game;

-(void)initGameFromXMLElement:(GDataXMLElement*)element;
-(bool)isInitialized;

@end
