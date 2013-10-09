//
//  PZWorldsViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/4/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "GDataXMLNode.h"

@interface PZWorldsViewController : UITableViewController <NSURLConnectionDelegate>
{
    NSMutableData *worldListResponseData;
}

@property (nonatomic, retain) NSURLConnection *worldListConnection;
@property (nonatomic, retain) GDataXMLDocument *doc;
@property (nonatomic, retain) NSMutableArray *worlds;

@end
