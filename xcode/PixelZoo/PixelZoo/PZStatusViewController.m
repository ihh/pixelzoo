//
//  PZStatusViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZStatusViewController.h"
#import "PZToolsViewController.h"
#import "PZLockViewController.h"

@interface PZStatusViewController ()

@end

@implementation PZStatusViewController

@synthesize worldStatusConnection;
@synthesize doc;

@synthesize worldDescriptor;
@synthesize selectedToolIDs;

@synthesize worldLabel;
@synthesize toolboxLabel;
@synthesize selectToolsButton;
@synthesize startTurnButton;

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

    // get world status (list of tools, detailed lock info, etc)
    // Send an asynchronous request
    NSMutableURLRequest *request = [worldDescriptor getRequest:@"status"];
    worldStatusConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    if ([segue.identifier isEqualToString:@"selectTools"]) {
        PZToolsViewController *destViewController = segue.destinationViewController;
        destViewController.worldDescriptor = self.worldDescriptor;
        destViewController.selectedToolIDs = self.selectedToolIDs;
    } else if ([segue.identifier isEqualToString:@"lockWorld"]) {
        PZLockViewController *destViewController = segue.destinationViewController;
        destViewController.worldDescriptor = self.worldDescriptor;
        destViewController.selectedToolIDs = self.selectedToolIDs;
    }
}

#pragma mark NSURLConnection Delegate Methods

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    // A response has been received, this is where we initialize the instance var
    // so that we can append data to it in the didReceiveData method
    // Furthermore, this method is called each time there is a redirect so reinitializing it
    // also serves to clear it
    worldStatusResponseData = [[NSMutableData alloc] init];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // Append the new data to the instance variable
    [worldStatusResponseData appendData:data];
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // The request is complete and data has been received
    // We can parse the stuff in the instance variable now
    
    // parse data using GDataXMLDocument, use xpath to extract list of <world>...</world> elements as NSArray
    NSError * error = nil;
    doc = [[GDataXMLDocument alloc] initWithData:worldStatusResponseData
                                         options:0 error:&error];
    
    worldDescriptor.statusNode = [doc rootElement];
    selectedToolIDs = [worldDescriptor defaultToolIDs];
    toolboxLabel.text = [worldDescriptor toolboxName];
    [self.view setNeedsDisplay];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}

@end
