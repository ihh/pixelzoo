//
//  pixelzooAppDelegate.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//

#import "pixelzooAppDelegate.h"
#import "pixelzooViewController.h"
#import "pixelzooWorldTableViewController.h"
#import "GDataXMLNode.h"

@implementation pixelzooAppDelegate

@synthesize window;
@synthesize viewController;
@synthesize worldTableViewController;


- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
    
    // Override point for customization after app launch    
    
    // Basic idea:
    // Use SERVER_URL_PREFIX instead of localhost:3000/world...
    // get world list from http://localhost:3000/world
    // e.g. GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/

    // Send a synchronous request
    NSURLRequest * urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/%@",@SERVER_URL_PREFIX,@"world/list"]]];
    NSURLResponse * response = nil;
    NSError * error = nil;
    NSData * data = [NSURLConnection sendSynchronousRequest:urlRequest
                                          returningResponse:&response
                                                      error:&error];
    
    if (error == nil)
    {
        // parse data using GDataXMLDocument, use xpath to extract list of <world>...</world> elements as NSArray
        // e.g. http://www.raywenderlich.com/725/xml-tutorial-for-ios-how-to-read-and-write-xml-documents-with-gdataxml

        GDataXMLDocument *doc = [[GDataXMLDocument alloc] initWithData:data 
                                                               options:0 error:&error];

        NSArray *worlds = [doc nodesForXPath:@"//world-list/world" error:nil];

        // little debug logging
        for (GDataXMLNode* node in worlds)
            NSLog(@"%@", node);

        // put worlds NSArray in pixelzooWorldTableViewController
        // as per http://blog.teamtreehouse.com/introduction-to-the-ios-uitableviewcontroller
        self.worldTableViewController.worldArray = worlds;
        
        // add the TableViewController to the view
        [window addSubview:worldTableViewController.tableView];

    // override pixelzooWorldTableViewController.didSelectRowAtIndexPath
    // https://developer.apple.com/library/ios/documentation/uikit/reference/UITableViewDelegate_Protocol/Reference/Reference.html

    // the following all occurs within pixelzooWorldTableViewController
    // extract WorldID from GDataXMLNode (using xpath?)

    // POST a lock to http://localhost:3000/world/WorldID/lock
    // again see GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
    // if successful, parse return body using GDataXMLDocument; use xpath to get <game>...</game>, also lock expiration time
    // create pixelzooViewController, initialize from <game> element, add to superview
    //     [superview addSubview:viewController.view];
    // update pixelzooViewController: add another NSTimer for lock expiration, change "restart" to "quit"
    // when done, call pzSaveBoardAsXmlString and POST to http://localhost:3000/world/WorldID/turn
    //     [viewController removeFromSuperview];
    //     [viewController release];

    }
    
//    [window addSubview:viewController.view];

    [window makeKeyAndVisible];
	
	return YES;
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
