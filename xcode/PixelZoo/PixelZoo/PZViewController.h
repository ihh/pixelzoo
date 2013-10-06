//
//  PZViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "PZWorldDescriptor.h"
#import "PZView.h"

@interface PZViewController : UIViewController

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) IBOutlet UILabel *worldLabel;
@property (nonatomic, strong) IBOutlet PZView *worldView;

@end
