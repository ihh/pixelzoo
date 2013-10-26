//
//  PZStatusViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "PZWorldDescriptor.h"

@interface PZStatusViewController : UIViewController <NSURLConnectionDelegate>
{
    NSMutableData *worldStatusResponseData;
}

@property (nonatomic, retain) NSURLConnection *worldStatusConnection;
@property (nonatomic, retain) GDataXMLDocument *doc;

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) NSMutableArray *selectedToolIDs;

@property (nonatomic, strong) IBOutlet UILabel *worldLabel;
@property (weak, nonatomic) IBOutlet UILabel *ownerLabel;
@property (nonatomic, strong) IBOutlet UILabel *toolboxLabel;
@property (nonatomic, strong) IBOutlet UIButton *selectToolsButton;
@property (nonatomic, strong) IBOutlet UIButton *startTurnButton;
@property (weak, nonatomic) IBOutlet UILabel *currentLock;
@property (weak, nonatomic) IBOutlet UILabel *nextLock;

@property (nonatomic,retain) NSTimer* lockUpdateTimer;

-(void)initStatusConnection;
-(void)updateLockLabels;

@end
