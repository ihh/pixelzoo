//
//  PZLockViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "PZWorldDescriptor.h"
#import "PZGameWrapper.h"

@interface PZLockViewController : UIViewController <NSURLConnectionDelegate> {
    NSMutableData *lockData;
    NSHTTPURLResponse* httpLockResponse;
    GDataXMLDocument *lockDoc;
    bool didAppear, lockFailed;
}

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) NSMutableArray *selectedToolIDs;
@property (nonatomic, strong) PZGameWrapper *gameWrapper;

@property (nonatomic, retain) NSURLConnection *lockConnection;

@property (nonatomic, strong) IBOutlet UIButton *playButton;
@property (nonatomic, strong) IBOutlet UILabel *lockLabel;

@end
