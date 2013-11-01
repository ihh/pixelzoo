//
//  PZLockViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "PZWorldDescriptor.h"
#import "PZLockDescriptor.h"
#import "PZGameWrapper.h"

@interface PZLockViewController : UIViewController <NSURLConnectionDelegate> {
    NSMutableData *lockData;
    NSHTTPURLResponse* httpLockResponse;
    bool didAppear, popQueued, pushQueued;
    NSTimer* lockUpdateTimer;
}

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) PZLockDescriptor *lockDescriptor;
@property (nonatomic, strong) NSMutableArray *selectedToolIDs;
@property (nonatomic, strong) PZGameWrapper *gameWrapper;

@property (nonatomic, retain) NSURLConnection *lockConnection;

@property (nonatomic, strong) IBOutlet UIButton *playButton;
@property (weak, nonatomic) IBOutlet UIButton *endTurnButton;
@property (weak, nonatomic) IBOutlet UILabel *worldLabel;
@property (nonatomic, strong) IBOutlet UILabel *lockLabel;
@property (weak, nonatomic) IBOutlet UILabel *incumbentCountLabel;
@property (weak, nonatomic) IBOutlet UILabel *challengerCountLabel;

- (IBAction)endTurn:(id)sender;
- (void)lockWasDeleted;

-(void)updateLockLabels;


@end
