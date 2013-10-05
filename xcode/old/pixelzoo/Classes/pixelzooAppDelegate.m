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
    
    // Basic idea:
    // Use SERVER_URL_PREFIX instead of localhost:3000/world...
    // get world list from http://localhost:3000/world
    // e.g. GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/

    // Send a synchronous request
    NSURLRequest * urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/world/list",@SERVER_URL_PREFIX]]];
    NSURLResponse * response = nil;
    NSError * error = nil;
    NSData * data = [NSURLConnection sendSynchronousRequest:urlRequest
                                          returningResponse:&response
                                                      error:&error];
    
    if (error == nil)
    {
        // create main menu (UITableViewController)
        pixelzooWorldTableViewController* wtvc = [[pixelzooWorldTableViewController alloc] init];
        [wtvc tableView].allowsSelection = YES;
        
        // parse data using GDataXMLDocument, use xpath to extract list of <world>...</world> elements as NSArray
        // e.g. http://www.raywenderlich.com/725/xml-tutorial-for-ios-how-to-read-and-write-xml-documents-with-gdataxml

        GDataXMLDocument *doc = [[GDataXMLDocument alloc] initWithData:data 
                                                               options:0 error:&error];

        NSArray *worlds = [doc nodesForXPath:@"//world-list/world" error:nil];

        // put worlds NSArray in pixelzooWorldTableViewController
        // as per http://blog.teamtreehouse.com/introduction-to-the-ios-uitableviewcontroller
        wtvc.worldArray = worlds;
        [worlds release];
        
        // add the TableView to the main window
        [window addSubview:wtvc.tableView];
        
        // assign & release
        self.worldTableViewController = wtvc;
        [wtvc release];
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
