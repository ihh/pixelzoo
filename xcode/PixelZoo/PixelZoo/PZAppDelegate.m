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
#import "Base64.h"

#define GotDefaultUserKey "PZGotDefaultUser"
#define DefaultUserKey "PZDefaultUser"
#define DefaultPassKey "PZDefaultPass"

@implementation PZAppDelegate

@synthesize window;
@synthesize loginValidated;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.

    if ([self gotDefaultUser])
        [self loginUser:[self getDefaultUser] withPass:[self getDefaultPass]];
    
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

- (bool) gotDefaultUser {
    return [[NSUserDefaults standardUserDefaults] boolForKey:@GotDefaultUserKey];
}

- (NSString*) getDefaultUser {
    return [[NSUserDefaults standardUserDefaults] stringForKey:@DefaultUserKey];
}

- (NSString*) getDefaultPass {
    return [[NSUserDefaults standardUserDefaults] stringForKey:@DefaultPassKey];
}

- (void) setDefaultUser:(NSString*)user withPass:(NSString*)pass {
    [[NSUserDefaults standardUserDefaults] setObject:user forKey:@DefaultUserKey];
    [[NSUserDefaults standardUserDefaults] setObject:pass forKey:@DefaultPassKey];
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:@GotDefaultUserKey];
}

- (void) clearDefaultUser {
    [[NSUserDefaults standardUserDefaults] setBool:NO forKey:@GotDefaultUserKey];
    self.loginValidated = NO;
}

- (bool) loginUser:(NSString*)user withPass:(NSString*)pass {
    NSMutableURLRequest * urlRequest = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/user/login",@SERVER_URL_PREFIX]]
                                                        cachePolicy: NSURLRequestReloadIgnoringCacheData
                                                        timeoutInterval: 3];

    // add Base64-encoded authentication token
    [Base64 addBasicAuthHeader:urlRequest forUser:user withPass:pass];

    // fire it off, synchronously...
    NSURLResponse * response = nil;
    NSError * error = nil;
    [NSURLConnection sendSynchronousRequest:urlRequest returningResponse:&response error:&error];
    
    if (error == nil)
    {
        [self setDefaultUser:user withPass:pass];
        self.loginValidated = YES;
        return YES;
    }
    
    [self clearDefaultUser];
    return NO;
}

- (void) addStoredBasicAuthHeader:(NSMutableURLRequest*)request {
    [Base64 addBasicAuthHeader:request forUser:[self getDefaultUser] withPass:[self getDefaultPass]];
}


@end
