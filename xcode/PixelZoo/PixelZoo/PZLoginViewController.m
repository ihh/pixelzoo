//
//  PZLoginViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/12/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZLoginViewController.h"
#import "PZAppDelegate.h"

@interface PZLoginViewController ()

@end

@implementation PZLoginViewController

@synthesize username;
@synthesize password;
@synthesize loginButton;
@synthesize accountExists;

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
    [self.loginButton setTitle:(accountExists ? @"Log in" : @"Create account") forState:UIControlStateNormal];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)backToMenu {
    NSArray* viewControllers = [self.navigationController viewControllers];
    [[(UITableViewController*)[viewControllers objectAtIndex:([viewControllers count] - 2)] tableView] reloadData];
    [self.navigationController popViewControllerAnimated:YES];
}

- (IBAction)didLogin:(UIButton *)sender {
    PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
    if (accountExists) {
        // log into existing account
        bool loginWorked = [appDelegate loginUser:username.text withPass:password.text];
        if (loginWorked)
            [self backToMenu];
        else {
            password.text = @"";
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Login failed"
                                                            message:@"Oops, that didn't work."
                                                           delegate:nil
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
            [alert show];
        }
    } else {
        // create account
        NSInteger statusCode = [appDelegate createUser:username.text withPass:password.text];
        if (statusCode == 201)
            [self backToMenu];
        else {
            bool userExists = statusCode == 409;  // 409 CONFLICT
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Couldn't create account"
                                                            message:(userExists ? @"Sorry, that username is taken." : @"Oops, that didn't work.")
                                                           delegate:nil
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
            [alert show];
        }
    }
}

@end
