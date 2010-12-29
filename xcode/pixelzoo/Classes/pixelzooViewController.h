//
//  pixelzooViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface pixelzooViewController : UIViewController {
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;
	
	UIImageView *drawImage;

	CGPoint lastPoint;
	BOOL mouseSwiped;	
	int mouseMoved;
}

@end

