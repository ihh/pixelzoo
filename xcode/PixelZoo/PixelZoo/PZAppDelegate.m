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

// chibi global path variables
char* sexp_default_module_path = "";  // empty for now
char* sexp_pixelzoo_module_path = "";  // empty for now

@implementation PZAppDelegate

@synthesize window;
@synthesize loginValidated;

// for playing with ARM ASM...
/*
void sum7(int a,int b,int c,int d,int e,int f,int g,int* ret) {
    *ret = a+b+c+d+e+f+g;
}
*/
 
- (BOOL )application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.

    // set up chibi global variables to point to Scheme module resource bundles
    NSString *chibiLibDirectoryPath = [[[NSBundle mainBundle] resourcePath]
					stringByAppendingPathComponent:@"lib"];
    char *default_module_path = (char*) [chibiLibDirectoryPath UTF8String];
    sexp_default_module_path = malloc (strlen (default_module_path) + 1);
    strcpy(sexp_default_module_path,default_module_path);

    NSString *pzSchemeFilePath = [[[NSBundle mainBundle] resourcePath]
				   stringByAppendingPathComponent:@"scheme/zoo.scm"];
    char* pixelzoo_module_path = (char*) [pzSchemeFilePath UTF8String];
    sexp_pixelzoo_module_path = malloc (strlen (pixelzoo_module_path) + 1);
    strcpy(sexp_pixelzoo_module_path,pixelzoo_module_path);

    // check stored authentication info
    if ([self gotDefaultUser])
        [self loginUser:[self getDefaultUser] withPass:[self getDefaultPass]];

    // for playing with ARM ASM...
    /*
    int x;
    sum7(1,2,3,4,5,6,7,&x);
    NSLog(@"%d",x);
    */
     
    // return
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
    // create request
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

- (NSInteger) createUser:(NSString*)user withPass:(NSString*)pass {
    // create request
    NSMutableURLRequest * urlRequest = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@/user/status",@SERVER_URL_PREFIX]]
                                                               cachePolicy: NSURLRequestReloadIgnoringCacheData
                                                           timeoutInterval: 3];
    
    // Specify that it will be a POST request
    urlRequest.HTTPMethod = @"POST";
    
    // add authentication header
    PZAppDelegate *appDelegate = (PZAppDelegate *)[[UIApplication sharedApplication] delegate];
    [appDelegate addStoredBasicAuthHeader:urlRequest];
    
    // Convert data and set request's HTTPBody property
    NSString* statusString = [NSString stringWithFormat:@"<status><user>%@</user><pass>%@</pass></status>",user,pass];
    [Base64 setContentTypeXML:urlRequest withContent:statusString];
    
    // fire it off, synchronously...
    NSURLResponse * response = nil;
    NSError * error = nil;
    [NSURLConnection sendSynchronousRequest:urlRequest returningResponse:&response error:&error];
    NSHTTPURLResponse* httpResponse = (NSHTTPURLResponse*) response;
    
    if (error == nil && [httpResponse statusCode] == 201) {  // 201 CREATED
        [self setDefaultUser:user withPass:pass];
        loginValidated = YES;
    } else
        [self clearDefaultUser];

    return (error == nil && httpResponse) ? [httpResponse statusCode] : 500;  // 500 INTERNAL SERVER ERROR (faked)
}

- (void) addStoredBasicAuthHeader:(NSMutableURLRequest*)request {
    [Base64 addBasicAuthHeader:request forUser:[self getDefaultUser] withPass:[self getDefaultPass]];
}


@end
