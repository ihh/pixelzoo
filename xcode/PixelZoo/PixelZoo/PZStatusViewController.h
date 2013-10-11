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
@property (nonatomic, strong) IBOutlet UILabel *toolboxLabel;
@property (nonatomic, strong) IBOutlet UIButton *selectToolsButton;
@property (nonatomic, strong) IBOutlet UIButton *startTurnButton;

@end
