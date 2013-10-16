//
//  PZLoginViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/12/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface PZLoginViewController : UIViewController

@property (weak, nonatomic) IBOutlet UITextField *username;
@property (weak, nonatomic) IBOutlet UITextField *password;
@property (weak, nonatomic) IBOutlet UIButton *loginButton;

@property (nonatomic) bool accountExists;

- (IBAction)didLogin:(UIButton *)sender;

- (void)backToMenu;

@end
