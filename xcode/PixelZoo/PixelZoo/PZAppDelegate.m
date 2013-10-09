//
//  PZAppDelegate.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/4/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZAppDelegate.h"
#import "PZDefs.h"
#import "GDataXMLNode.h"
#import "PZWorldDescriptor.h"

@implementation PZAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.

    // get Worlds controller
    UINavigationController *navigationController = (UINavigationController*) self.window.rootViewController;
    worldsViewController = (PZWorldsViewController*) [[navigationController viewControllers] objectAtIndex:0];
    
    // get world list, to populate Worlds controller
    
    // Send an asynchronous request
    NSURLRequest * urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/world/list",@SERVER_URL_PREFIX]]];
    worldListConnection = [[NSURLConnection alloc] initWithRequest:urlRequest delegate:self];

    return YES;
}
							
- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

#pragma mark NSURLConnection Delegate Methods

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    // A response has been received, this is where we initialize the instance var
    // so that we can append data to it in the didReceiveData method
    // Furthermore, this method is called each time there is a redirect so reinitializing it
    // also serves to clear it
    worldListResponseData = [[NSMutableData alloc] init];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // Append the new data to the instance variable
    [worldListResponseData appendData:data];
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
    GDataXMLDocument *doc = [[GDataXMLDocument alloc] initWithData:worldListResponseData
                                                           options:0 error:&error];
    
    worldsViewController.doc = doc;
    
    NSArray *worldNodes = [doc nodesForXPath:@"//world-list/world" error:nil];
    NSMutableArray *worldDescriptors = [[NSMutableArray alloc] init];
    for (int i = 0; i < [worldNodes count]; ++i)
        [worldDescriptors addObject:[[PZWorldDescriptor alloc] initWithNode:[worldNodes objectAtIndex:i]]];
    
    // put worlds NSArray in PZWorldTableViewController
    worldsViewController.worlds = worldDescriptors;
    [worldsViewController.tableView reloadData];
    
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}

@end
