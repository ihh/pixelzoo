//
//  pixelzooView.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/29/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "pixelzooView.h"


@implementation pixelzooView


- (id)initWithFrame:(CGRect)frame {
    if ((self = [super initWithFrame:frame])) {
        // Initialization code
		NSLog(@"initWithFrame");
    }
    return self;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
*/
- (void)drawRect:(CGRect)rect
{   
	NSLog(@"drawRect");
}



- (void)dealloc {
    [super dealloc];
}


@end
