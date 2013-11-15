//
//  PZIsometricMapViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <SpriteKit/SpriteKit.h>
#import "PZWorldDescriptor.h"
#import "PZGameWrapper.h"
#import "PZIsoMapScene.h"

@interface PZIsometricViewController : UIViewController {
    // timers
	NSTimer *evolveTimer;

    // scenes
    PZIsoMapScene* map;
}

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) PZLockDescriptor *lockDescriptor;
@property (nonatomic, strong) PZGameWrapper *gameWrapper;
@property (weak, nonatomic) IBOutlet SKView *skview;


@end
