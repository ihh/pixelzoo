//
//  pixelzooAppDelegate.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//

#import <UIKit/UIKit.h>

@class pixelzooViewController;
@class pixelzooWorldTableViewController;

@interface pixelzooAppDelegate : NSObject <UIApplicationDelegate>

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) pixelzooWorldTableViewController *worldTableViewController;

@property (nonatomic, retain) pixelzooViewController *viewController;


@end

