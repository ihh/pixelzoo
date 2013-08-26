//
//  pixelzooWorldTableViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 8/22/13.
//  Copyright 2013 __MyCompanyName__. All rights reserved.
//

#import "pixelzooWorldTableViewController.h"
#import "pixelzooViewController.h"
#import "pixelzooDefs.h"
#import "GDataXMLNode.h"

@implementation pixelzooWorldTableViewController

@synthesize worldArray;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

- (void)viewDidLoad
{
    [super viewDidLoad];

    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
 
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [super viewWillDisappear:animated];
}

- (void)viewDidDisappear:(BOOL)animated
{
    [super viewDidDisappear:animated];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
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
    NSInteger rows = [self.worldArray count];
    NSLog(@"Rows: %d",rows);
    return rows;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Cell";
    
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier] autorelease];
    }
    
    // Configure the cell...
    
    int row = indexPath.row;
    GDataXMLNode *worldNode = [self.worldArray objectAtIndex:row];
    NSArray *names = [worldNode nodesForXPath:@"name" error:nil];
    GDataXMLElement *nameElement = [names objectAtIndex:0];

    cell.textLabel.text = [nameElement stringValue];
    cell.selectionStyle = UITableViewCellSelectionStyleBlue;
    
    return cell;
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
        [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] withRowAnimation:UITableViewRowAnimationFade];
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

#pragma mark - Table view delegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Create and push another view controller.
    pixelzooViewController* viewController = [[pixelzooViewController alloc] init];
    [self.navigationController pushViewController:viewController animated:YES];
    
    // extract WorldID from GDataXMLNode (using xpath)
    int row = indexPath.row;
    GDataXMLNode *worldNode = [self.worldArray objectAtIndex:row];

    NSArray *ids = [worldNode nodesForXPath:@"id" error:nil];
    GDataXMLElement *idElement = [ids objectAtIndex:0];
    NSString *idText = [idElement stringValue];
    
    // POST a lock to http://localhost:3000/world/WorldID/lock
    // again see GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
    // Create the request.
    NSMutableURLRequest *request = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/world/%@/lock",@SERVER_URL_PREFIX,idText]]];
    
    // Specify that it will be a POST request
    request.HTTPMethod = @"POST";
    
    // set header fields
    [request setValue:@"application/xml; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
    
    // Convert data and set request's HTTPBody property
    NSArray *toolsArray = [worldNode nodesForXPath:@"tools" error:nil];
    GDataXMLElement *toolsElement = [toolsArray objectAtIndex:0];
    NSString *toolsString = [toolsElement stringValue];

    NSData *requestBodyData = [toolsString dataUsingEncoding:NSUTF8StringEncoding];
    request.HTTPBody = requestBodyData;
    
    // Create url connection and fire request
    NSURLResponse * response = nil;
    NSError * error = nil;
    NSData *lockData = [NSURLConnection sendSynchronousRequest:request
                                             returningResponse:&response
                                                      error:&error];
    NSHTTPURLResponse* httpResponse = (NSHTTPURLResponse*)response;

    // if lock successfully POSTed, parse return body using GDataXMLDocument; use xpath to get <game>...</game>, also lock expiration time
    if (error == nil && [httpResponse statusCode] == 201)  // 201 CREATED
    {
        // parse data using GDataXMLDocument, use xpath to extract <game>...</game> element
        GDataXMLDocument *lockDoc = [[GDataXMLDocument alloc] initWithData:lockData 
                                                               options:0 error:&error];
        
        NSArray *gamesArray = [lockDoc nodesForXPath:@"//lock/game" error:nil];
        GDataXMLElement *gameElement = [gamesArray objectAtIndex:0];
        NSString *gameString = [gameElement stringValue];
        
        // initialize viewController from <game> element, start game

        // TODO (updates to pixelzooViewController):
        // implement pixelzooViewController.loadFromString method

        // add another NSTimer for lock expiration, change "restart" to "quit"
        
        // the following end-of-turn logic needs to go in a common method called by "quit" & timeout:
        // call pzSaveBoardAsXmlString and POST to http://localhost:3000/world/WorldID/turn
        // again see GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
        
    }

    [viewController release];

}

@end
