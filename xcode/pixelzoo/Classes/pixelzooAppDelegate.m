//
//  pixelzooAppDelegate.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//

#import "pixelzooAppDelegate.h"
#import "pixelzooViewController.h"
#import "pixelzooWorldTableViewController.h"
#import "pixelzooDefs.h"
#import "GDataXMLNode.h"

@implementation pixelzooAppDelegate

@synthesize window;
@synthesize viewController;
@synthesize worldTableViewController;


- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
    
    // Override point for customization after app launch    
    
    UIWindow* w = [[UIWindow alloc] init];
    self.window = w;
    [w release];
    
    pixelzooWorldTableViewController* wtvc = [[pixelzooWorldTableViewController alloc] init];
    self.worldTableViewController = wtvc;
    [wtvc release];
    
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

        // do a little debug logging
        for (GDataXMLNode* node in worlds)
            NSLog(@"%@", node);

        // put worlds NSArray in pixelzooWorldTableViewController
        // as per http://blog.teamtreehouse.com/introduction-to-the-ios-uitableviewcontroller
        worldTableViewController.worldArray = worlds;
        [worlds release];
        
        // add the TableViewController to the view
        [window addSubview:worldTableViewController.tableView];
    }
    
    [window makeKeyAndVisible];
	
	return YES;
}


- (void)dealloc {
    [worldTableViewController release];
    [window release];
    [super dealloc];
}


@end
