//
//  PZBoardIterator.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/17/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol PZBoardIterator <NSObject>

-(void) visitCellAtX:(int)x y:(int)y z:(int)z;

@end
