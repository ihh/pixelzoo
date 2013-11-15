//
//  PZMenuViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/12/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZAppDelegate.h"
#import "PZMenuViewController.h"
#import "PZLoginViewController.h"
#import "PZIsometricViewController.h"

@interface PZMenuViewController ()

@end

@implementation PZMenuViewController

@synthesize createAccountCell;
@synthesize loginCell;
@synthesize logoutCell;
@synthesize playCell;
@synthesize aboutCell;
@synthesize tutorialCell;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];

    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
 
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (bool) loginValidated {
    PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
    return [appDelegate loginValidated];
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell *theCellClicked = [tableView cellForRowAtIndexPath:indexPath];
    if (theCellClicked == logoutCell) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Confirm logout"
                                                        message:@"Really, log out?"
                                                       delegate:self
                                              cancelButtonTitle:@"Yes"
                                              otherButtonTitles:@"No",nil];
        [alert show];
    }
}

#pragma mark - UIAlertView delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    if (buttonIndex == 0)
    {
        PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
        [appDelegate clearDefaultUser];
        [self.tableView reloadData];
    }
}


#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    // Return the number of rows in the section.
    return 4;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    int row = indexPath.row;
    switch (row) {
        case 0:
            return [self loginValidated] ? playCell : createAccountCell;
        case 1:
            return [self loginValidated] ? logoutCell : loginCell;
        case 2:
            return tutorialCell;
        case 3:
            return aboutCell;
        default:
            break;
    }
    return NULL;
}


/*
// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Return NO if you do not want the specified item to be editable.
    return YES;
}
*/

/*
// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        // Delete the row from the data source
        [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
    }   
    else if (editingStyle == UITableViewCellEditingStyleInsert) {
        // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
    }   
}
*/

/*
// Override to support rearranging the table view.
- (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
{
}
*/

/*
// Override to support conditional rearranging of the table view.
- (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Return NO if you do not want the item to be re-orderable.
    return YES;
}
*/

#pragma mark - Navigation

// In a story board-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"loginSegue"]) {
        PZLoginViewController *destViewController = segue.destinationViewController;
        destViewController.accountExists = true;
    } else if ([segue.identifier isEqualToString:@"createAccountSegue"]) {
        PZLoginViewController *destViewController = segue.destinationViewController;
        destViewController.accountExists = false;
    } else if ([segue.identifier isEqualToString:@"tutorialSegue"]) {
        PZIsometricViewController *destViewController = segue.destinationViewController;
        PZGameWrapper *game = [PZGameWrapper alloc];
        NSString *tutorialPath = [[[NSBundle mainBundle] resourcePath]
                                      stringByAppendingPathComponent:@"tutorial.xml"];
        [game initGameFromXMLString:[NSString stringWithContentsOfFile:tutorialPath encoding:NSUTF8StringEncoding error:nil] ];
        destViewController.gameWrapper = game;
    }
}


@end
