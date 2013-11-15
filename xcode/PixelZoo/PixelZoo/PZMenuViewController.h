//
//  PZMenuViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/12/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface PZMenuViewController : UITableViewController <UITableViewDelegate,UIAlertViewDelegate>

- (bool) loginValidated;

@property (weak, nonatomic) IBOutlet UITableViewCell *createAccountCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *loginCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *playCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *logoutCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *aboutCell;
@property (weak, nonatomic) IBOutlet UITableViewCell *tutorialCell;

@end
