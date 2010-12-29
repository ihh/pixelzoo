//
//  pixelzooViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface pixelzooViewController : UIViewController {
	UIImageView *drawImage;

	NSInteger appState;

	CGPoint lastPoint;
	BOOL mouseSwiped;	
	int mouseMoved;
}

@property(nonatomic) NSInteger appState;

@end

