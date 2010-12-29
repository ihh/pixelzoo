//
//  pixelzooViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import "pixelzooViewController.h"

@implementation pixelzooViewController

- (void)viewDidLoad {
    [super viewDidLoad];
	drawImage = [[UIImageView alloc] initWithImage:nil];
	drawImage.frame = self.view.frame;
	[self.view addSubview:drawImage];
	self.view.backgroundColor = [UIColor blackColor];
	mouseMoved = 0;
}


- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    
    mouseSwiped = NO;
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
        drawImage.image = nil;
        return;
    }
	
    lastPoint = [touch locationInView:self.view];
    lastPoint.y -= 20;
	
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    mouseSwiped = YES;
    
    UITouch *touch = [touches anyObject];   
    CGPoint currentPoint = [touch locationInView:self.view];
    currentPoint.y -= 20;
    
    
    UIGraphicsBeginImageContext(self.view.frame.size);
    [drawImage.image drawInRect:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
    CGContextSetLineCap(UIGraphicsGetCurrentContext(), kCGLineCapRound);
    CGContextSetLineWidth(UIGraphicsGetCurrentContext(), 5.0);
    CGContextSetRGBStrokeColor(UIGraphicsGetCurrentContext(), 1.0, 0.0, 0.0, 1.0);
    CGContextBeginPath(UIGraphicsGetCurrentContext());
    CGContextMoveToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
    CGContextAddLineToPoint(UIGraphicsGetCurrentContext(), currentPoint.x, currentPoint.y);
    CGContextStrokePath(UIGraphicsGetCurrentContext());
    drawImage.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    
    lastPoint = currentPoint;
	
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
        drawImage.image = nil;
        return;
    }
    
    
    if(!mouseSwiped) {
        UIGraphicsBeginImageContext(self.view.frame.size);
        [drawImage.image drawInRect:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
        CGContextSetLineCap(UIGraphicsGetCurrentContext(), kCGLineCapRound);
        CGContextSetLineWidth(UIGraphicsGetCurrentContext(), 5.0);
        CGContextSetRGBStrokeColor(UIGraphicsGetCurrentContext(), 1.0, 0.0, 0.0, 1.0);
        CGContextMoveToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
        CGContextAddLineToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
        CGContextStrokePath(UIGraphicsGetCurrentContext());
        CGContextFlush(UIGraphicsGetCurrentContext());
        drawImage.image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
    }
}



/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    if ((self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
}
*/


/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
}
*/


/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


- (void)dealloc {
    [super dealloc];
}

@end
