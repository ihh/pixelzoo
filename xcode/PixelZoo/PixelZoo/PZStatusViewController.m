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
@synthesize ownerLabel;
@synthesize toolboxLabel;
@synthesize selectToolsButton;
@synthesize startTurnButton;

@synthesize currentLock;
@synthesize nextLock;

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
    [worldDescriptor setWorldLabel:worldLabel];
    ownerLabel.text = [NSString stringWithFormat:@"Ruler: %@",[worldDescriptor owner]];

    [[startTurnButton layer] setBorderWidth:1.0];
    [[startTurnButton layer] setCornerRadius:3.0];

    [[selectToolsButton layer] setBorderWidth:1.0];
    [[selectToolsButton layer] setCornerRadius:3.0];
}

-(void)viewWillAppear:(BOOL)animated {
    [self initStatusConnection];
}

- (void) initStatusConnection {
    if (worldStatusConnection == nil) {
        // get world status (list of tools, detailed lock info, etc)
        // Send an asynchronous request
        NSMutableURLRequest *request = [worldDescriptor getRequest:@"status"];
        worldStatusConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    }
}

- (void) viewWillDisappear:(BOOL)animated {
    [lockUpdateTimer invalidate];
    lockUpdateTimer = nil;
}

-(void)updateLockLabels {
    if (worldDescriptor) {
        if ([worldDescriptor isLocked]) {
            NSInteger expiryTime = [worldDescriptor lockExpiryWait];
            currentLock.text = [NSString stringWithFormat:@"Locked by %@ for %d:%02d",[worldDescriptor lockOwner],(int)(expiryTime/60),(int)(expiryTime%60)];
            bool myLock = [worldDescriptor userOwnsLock];
            [startTurnButton setTitle:(myLock ? @"Continue turn" : @"Start turn") forState:UIControlStateNormal];
            startTurnButton.enabled = myLock;
            selectToolsButton.enabled = !myLock;
            startTurnButton.alpha = myLock ? 1.0 : 0.5;
            selectToolsButton.alpha = myLock ? 0.5 : 1.0;
        } else {
            bool lockedOut = [worldDescriptor lockedOut];
            currentLock.text = lockedOut ? @"Unlocked (but you have to wait)" : @"Unlocked";
            [startTurnButton setTitle:@"Start turn" forState:UIControlStateNormal];
            startTurnButton.enabled = !lockedOut;
            selectToolsButton.enabled = YES;
            startTurnButton.alpha = lockedOut ? 0.5 : 1.0;
            selectToolsButton.alpha = 1.0;
        }
        [startTurnButton sizeToFit];
        [selectToolsButton sizeToFit];

        if ([worldDescriptor lockedOut]) {
            NSInteger deleteTime = [worldDescriptor lockDeleteWait];
            nextLock.text = [NSString stringWithFormat:@"Next turn in %d:%02d",(int)(deleteTime/60),(int)(deleteTime%60)];
        } else {
            nextLock.text = @"";
        }
        [self.view setNeedsDisplay];
    }
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
    toolboxLabel.text = [NSString stringWithFormat:@"Terraforming: %@",[worldDescriptor toolboxName]];

    worldStatusConnection = nil;

    [lockUpdateTimer invalidate];
    lockUpdateTimer = [NSTimer scheduledTimerWithTimeInterval:.1 target:self selector:@selector(updateLockLabels) userInfo:nil repeats:YES];

    [self updateLockLabels];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}


@end
