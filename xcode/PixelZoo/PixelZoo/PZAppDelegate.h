//
//  PZAppDelegate.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/4/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface PZAppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (nonatomic) bool loginValidated;

- (bool) gotDefaultUser;
- (NSString*) getDefaultUser;
- (NSString*) getDefaultPass;
- (void) setDefaultUser:(NSString*)user withPass:(NSString*)pass;
- (void) clearDefaultUser;

- (bool) loginUser:(NSString*)user withPass:(NSString*)pass;
- (void) addStoredBasicAuthHeader:(NSMutableURLRequest*)request;

@end
