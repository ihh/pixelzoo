//
//  PZIsometricViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZIsometricViewController.h"
#import "PZIsometricScene.h"
#import "PZDefs.h"

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
    [[self navigationController] interactivePopGestureRecognizer].enabled = NO;
    
    skview.showsDrawCount = YES;
    skview.showsNodeCount = YES;
    skview.showsFPS = YES;

}

- (void)viewWillAppear:(BOOL)animated
{
    // present a dummy scene so that the screen looks black when the view appears.
    // if we present the actual scene here, the SKView's coordinate system gets screwed up.
    SKScene *dummyScene = [[SKScene alloc] initWithSize:skview.frame.size];
    [skview presentScene: dummyScene];
    [super viewWillAppear:animated];
}

- (void) viewDidAppear:(BOOL)animated
{
    map = [[PZIsometricScene alloc] initWithSize:skview.frame.size forGame:gameWrapper withWorld:worldDescriptor withLock:lockDescriptor forController:self];
    [skview presentScene: map];
    [map startTimers];
    [super viewDidAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated {
	[map stopTimers];
    [super viewWillDisappear:animated];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/* Gesture recognizers */
- (IBAction)handlePan:(UIPanGestureRecognizer *)recognizer {
    [map handlePan:recognizer];
}

- (IBAction)handlePinch:(UIPinchGestureRecognizer *)recognizer {
    [map handlePinch:recognizer];
}


@end
