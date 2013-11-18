//
//  PZCellNode.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/17/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZCellNode : SKSpriteNode

@property unsigned long long lastModified;
@property unsigned long long lastUpdated;

+(PZCellNode*)newForGame:(PZGameWrapper*)game x:(int)x y:(int)y z:(int)z;

@end
