//
//  PZToolbarNode.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZToolbarNode.h"
#import "PZToolNode.h"
#import "PZDefs.h"

@implementation PZToolbarNode

@synthesize game;
@synthesize tools;

+(PZToolbarNode*)newToolbarNodeForGame:(PZGameWrapper*)game withWidth:(CGFloat)width {
    PZToolbarNode* node = [PZToolbarNode node];
    node.game = game;
    node.userInteractionEnabled = YES;
    NSMutableArray* tools = [[NSMutableArray alloc] init];
    for (int n = -1; n < [game numberOfTools]; ++n) {
        PZToolNode* toolNode = [PZToolNode spriteNodeForToolNumber:n forGame:game forToolbar:node];
        toolNode.position = CGPointMake(TOOL_ICON_WIDTH * (n+1),0);
        [node addChild:toolNode];
        [tools addObject:toolNode];
    }
    node.tools = tools;
    [node refresh];
    [node setScale:MIN(1,width/(TOOL_ICON_WIDTH * (1 + [game numberOfTools])))];
    return node;
}

-(CGFloat)height {
    return (TOOL_RESERVE_HEIGHT + TOOL_ICON_HEIGHT) * self.yScale;
}

-(void)refresh {
    for (id toolNode in tools)
        [(PZToolNode*)toolNode refresh];
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    [self refresh];
}

@end
