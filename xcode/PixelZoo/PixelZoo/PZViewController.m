//
//  PZViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZViewController.h"

@interface PZViewController ()

@end

@implementation PZViewController

@synthesize worldDescriptor;
@synthesize worldLabel;
@synthesize worldView;

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
    worldLabel.text = [worldDescriptor name];

    // ... post lock, pass world to worldView ....
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
