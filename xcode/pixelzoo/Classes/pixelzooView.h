//
//  pixelzooView.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/29/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "pixelzooViewController.h"

// PixelZoo includes
#include "game.h"

@interface pixelzooView : UIView {
	pixelzooViewController *controller;
	int redraws;
}

@property(readwrite,assign) pixelzooViewController *controller;

@end
