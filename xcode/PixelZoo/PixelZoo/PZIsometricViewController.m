//
//  PZIsometricMapViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZIsometricViewController.h"
#import "PZIsoMapScene.h"

@interface PZIsometricViewController ()

@end

@implementation PZIsometricViewController

@synthesize gameWrapper;
@synthesize worldDescriptor;
@synthesize lockDescriptor;

@synthesize skview;


- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view.
    skview.showsDrawCount = YES;
    skview.showsNodeCount = YES;
    skview.showsFPS = YES;
}

- (void)viewWillAppear:(BOOL)animated
{
    PZIsoMapScene* map = [[PZIsoMapScene alloc] initWithSize:CGSizeMake(skview.bounds.size.width,skview.bounds.size.height)];
    [skview presentScene: map];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
