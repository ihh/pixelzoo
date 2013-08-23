//
//  pixelzooAppDelegate.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//

#import "pixelzooAppDelegate.h"
#import "pixelzooViewController.h"

@implementation pixelzooAppDelegate

@synthesize window;
@synthesize viewController;


- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
    
    // Override point for customization after app launch    
    
    // Basic idea:
    //     pixelzooWorldTableViewController *worldTableViewController;
    //     #define UrlPrefix http://localhost:3000/
    // get world list from http://localhost:3000/world
    // e.g. GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
    // parse using GDataXMLDocument, use xpath to extract list of <world>...</world> elements as NSArray
    // e.g. http://www.raywenderlich.com/725/xml-tutorial-for-ios-how-to-read-and-write-xml-documents-with-gdataxml
    // add this to pixelzooWorldTableViewController as per http://blog.teamtreehouse.com/introduction-to-the-ios-uitableviewcontroller
    //     [window addSubview:worldTableViewController.view]

    // override pixelzooWorldTableViewController.didSelectRowAtIndexPath
    // https://developer.apple.com/library/ios/documentation/uikit/reference/UITableViewDelegate_Protocol/Reference/Reference.html

    // the following all occurs within pixelzooWorldTableViewController
    // extract WorldID from GDataXMLNode (using xpath?)

    // POST a lock to http://localhost:3000/world/WorldID/lock
    // if successful, parse return body using GDataXMLDocument; use xpath to get <game>...</game>, also lock expiration time
    // create pixelzooViewController, initialize from <game> element, add to superview
    //     [superview addSubview:viewController.view];
    // refactor pixelzooViewController to #include pixelzoo.h instead of xmlgame.h
    // update pixelzooViewController: add another NSTimer for lock expiration, change "restart" to "quit"
    // when done, call pzSaveBoardAsXmlString and POST to http://localhost:3000/world/WorldID/turn
    //     [viewController removeFromSuperview];
    //     [viewController release];

    
    [window addSubview:viewController.view];
    [window makeKeyAndVisible];
	
	return YES;
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
