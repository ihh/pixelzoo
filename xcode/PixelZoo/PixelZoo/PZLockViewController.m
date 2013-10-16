//
//  PZLockViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZLockViewController.h"
#import "PZGameViewController.h"
#import "PZAppDelegate.h"
#import "Base64.h"

@interface PZLockViewController ()

@end

@implementation PZLockViewController

@synthesize lockConnection;

@synthesize worldDescriptor;
@synthesize selectedToolIDs;
@synthesize gameWrapper;

@synthesize playButton;
@synthesize lockLabel;

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
    // POST a lock to SERVER_URL_PREFIX/world/WorldID/lock
    // http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
    
    // Create tools description
    NSMutableString *toolsString = [[NSMutableString alloc] initWithString:@"<lock><tools>"];
    for (NSString* toolID in selectedToolIDs) {
        [toolsString appendString:[NSString stringWithFormat:@"<id>%@</id>",toolID]];
    }
    [toolsString appendString:@"</tools></lock>"];

    // Create the request.
    NSMutableURLRequest *request = [worldDescriptor authenticatedPostRequest:@"lock" withContent:toolsString];
    
    // Create url connection and fire request
    lockConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    if ([segue.identifier isEqualToString:@"playWorld"]) {
        PZGameViewController *destViewController = segue.destinationViewController;
        destViewController.worldDescriptor = self.worldDescriptor;
        destViewController.gameWrapper = self.gameWrapper;
    }
}

- (void)viewWillDisappear:(BOOL)animated {
    [self.lockConnection cancel];
    [super viewWillDisappear:animated];
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    didAppear = YES;
    if (lockFailed)
        [self.navigationController popViewControllerAnimated:YES];
}


#pragma mark NSURLConnection Delegate Methods

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    // A response has been received, this is where we initialize the instance var
    // so that we can append data to it in the didReceiveData method
    // Furthermore, this method is called each time there is a redirect so reinitializing it
    // also serves to clear it
    lockData = [[NSMutableData alloc] init];
    httpLockResponse = (NSHTTPURLResponse*)response;
    if ([httpLockResponse statusCode] != 201) {  // 201 CREATED
        [self.lockConnection cancel];
        lockFailed = YES;
        if (didAppear)
            [self.navigationController popViewControllerAnimated:YES];
    }
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // Append the new data to the instance variable
    [lockData appendData:data];
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // The request is complete and data has been received
    // We can parse the stuff in the instance variable now
    
    // check that lock was successfully POSTed
    if ([httpLockResponse statusCode] == 201) {  // 201 CREATED
        // parse return body using GDataXMLDocument; use xpath to get <game>...</game>
        NSError * error = nil;
        lockDoc = [[GDataXMLDocument alloc] initWithData:lockData
                                                 options:0 error:&error];
        
        NSArray *gamesArray = [lockDoc nodesForXPath:@"//lock/world/game" error:nil];
        GDataXMLElement *gameElement = [gamesArray objectAtIndex:0];
        
        gameWrapper = [PZGameWrapper alloc];
        [gameWrapper initGameFromXMLElement:gameElement forWorld:worldDescriptor];
        
        lockLabel.text = @"Locked";
        
        playButton.enabled = YES;
        playButton.alpha = 1.0;
        
        // TODO:
        // get lock expiration time; add an NSTimer for lock expiration
        
        // the following end-of-turn logic needs to go in a common method called by "quit" & timeout:
        // call pzSaveBoardAsXmlString and POST to http://localhost:3000/world/WorldID/turn
        // again see GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
        
    }
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var

    // ...for now, just pop
    lockFailed = YES;
    if (didAppear)
        [self.navigationController popViewControllerAnimated:YES];
}

@end
