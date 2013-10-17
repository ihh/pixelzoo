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

char* sexp_default_module_path = "";  // empty for now
// To actually do anything with chibi, you'll need the following lines...
// #include "chibi/eval.h"
// #include "chibi/sexp.h"

@implementation PZAppDelegate

@synthesize window;
@synthesize loginValidated;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.

    NSString *chibiLibDirectory = [[[NSBundle mainBundle] resourcePath]
                                   stringByAppendingPathComponent:@"lib"];
    sexp_default_module_path = (char*) [chibiLibDirectory UTF8String];

// the following example code tests the embedded Scheme interpreter & standard environment...
// first, uncomment the two chibi #include's, above
/*
    sexp ctx;
    char* ret;
    ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
    
    sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);  // required for definition of (let ...)
    sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 0);  // unnecessary, in this program
    
    sexp_eval_string(ctx,"(define (dbl_square x) (let ((k 2)) (* x x k)))",-1,NULL);
    ret = sexp_string_data (sexp_write_to_string (ctx, sexp_eval_string(ctx, "(dbl_square 3)", -1, NULL)));
    
    printf ("Return value is %s\n", ret);  // should be 18
    // don't free(ret) ... it's part of a Scheme string & will be garbage-collected
    
    sexp_destroy_context(ctx);
*/
    
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
