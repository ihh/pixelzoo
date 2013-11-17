//
//  PZToolNode.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZToolNode.h"
#import "PZDefs.h"

#define MoveIcon "hand.png"
#define DefaultIcon @"tool-spray.png"
#define FrameIcon @"ToolFrame.png"

@implementation PZToolNode

@synthesize toolnum;
@synthesize game;
@synthesize frame;
@synthesize reserve;

+(PZToolNode*)spriteNodeForToolNumber:(int)n forGame:(PZGameWrapper*)game forToolbar:(UIResponder*)toolbar {
    const char* icon = n < 0 ? MoveIcon : [game toolIconByNumber:n];
    NSString* iconImage = icon ? [NSString stringWithCString:icon encoding:NSUTF8StringEncoding] : DefaultIcon;
    PZToolNode* node = [[PZToolNode alloc] initWithImageNamed:iconImage];
    node.anchorPoint = CGPointMake(0,1);
    node.userInteractionEnabled = YES;
    node.game = game;
    node.toolnum = n;
    node->toolbar = toolbar;
    
    node.frame = [[SKSpriteNode alloc] initWithImageNamed:FrameIcon];
    node.frame.anchorPoint = CGPointMake(0,1);
    node.frame.position = CGPointMake(0, 0);
    
    if (n >= 0) {
        node.reserve = [[SKSpriteNode alloc] initWithColor:[SKColor greenColor] size:CGSizeMake(TOOL_ICON_WIDTH-2,TOOL_RESERVE_HEIGHT)];
        node.reserve.anchorPoint = CGPointMake(0,1);
        node.reserve.position = CGPointMake(1,-TOOL_ICON_HEIGHT);
        [node addChild:node.reserve];
    }

    return node;
}

-(void)refresh {
    if (reserve)
        reserve.xScale = [game toolReserveByNumber:toolnum];
    bool selected = [game selectedToolNumber] == toolnum;
    if (frame.parent && !selected) {
        [frame removeFromParent];
    } else if (selected && !frame.parent) {
        [self addChild:frame];
        frame.zPosition = self.zPosition + 1;
    }
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    if (toolnum >= 0)
        [game selectTool:toolnum];
    else
        [game unselectTool];
    [super touchesBegan:touches withEvent:event];
}

- (UIResponder*) nextResponder {
    return toolbar;
}

@end
