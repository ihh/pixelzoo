//
// Scene demo
// a cocos2d example
// http://www.cocos2d-iphone.org
//

#import "SceneTest.h"
#import "HelloWorldScene.h"

#pragma mark -
#pragma mark MainMenuLayer

@implementation MainMenuLayer
-(id) init
{
	if( (self=[super initWithColor: ccc4(0,255,0,255)]) ) {
	
		
		CCMenuItemFont *item1 = [CCMenuItemFont itemFromString: @"Quick Play" target:self selector:@selector(onQuickPlay:)];
		CCMenuItemFont *item2 = [CCMenuItemFont itemFromString: @"Levels" target:self selector:@selector(onLevels:)];
		CCMenuItemFont *item3 = [CCMenuItemFont itemFromString: @"Multiplayer" target:self selector:@selector(onMultiplayer:)];
		CCMenuItemFont *item4 = [CCMenuItemFont itemFromString: @"Options" target:self selector:@selector(onOptions:)];
		
		CCMenu *menu = [CCMenu menuWithItems: item1, item2, item3, item4, nil];
		[menu alignItemsVertically];
		
		[self addChild: menu];

		CGSize s = [CCDirector sharedDirector].winSize;
		CCSprite *sprite = [CCSprite spriteWithFile:@"grossini.png"];
		[self addChild:sprite];
		sprite.position = ccp(s.width-40, s.height/2);
		id rotate = [CCRotateBy actionWithDuration:2 angle:360];
		id repeat = [CCRepeatForever actionWithAction:rotate];
		[sprite runAction:repeat];
		
		
		[self schedule:@selector(testDealloc:)];
	}

	return self;
}

-(void) onEnter
{
	NSLog(@"Layer1#onEnter");
	[super onEnter];
}

-(void) onEnterTransitionDidFinish
{
	NSLog(@"Layer1#onEnterTransitionDidFinish");
	[super onEnterTransitionDidFinish];
}

-(void) cleanup
{
	NSLog(@"Layer1#cleanup");
	[super cleanup];
}

-(void) testDealloc:(ccTime) dt
{
	NSLog(@"Layer1:testDealloc");
}

-(void) dealloc
{
	NSLog(@"Layer1 - dealloc");
	[super dealloc];
}

-(void) onQuickPlay: (id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [HelloWorld node] z:0];
	[[CCDirector sharedDirector] replaceScene:scene];
}

-(void) onLevels: (id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [LevelMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene:scene];
}

-(void) onMultiplayer: (id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [MultiplayerMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene:scene];
}

-(void) onOptions: (id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [OptionsMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene:scene];
}

-(void) onVoid: (id) sender
{
}
@end

#pragma mark -
#pragma mark LevelMenuLayer

@implementation LevelMenuLayer
-(id) init
{
	if( (self=[super initWithColor: ccc4(255,0,0,255)]) ) {
	
		CCMenuItemFont *item1 = [CCMenuItemFont itemFromString: @"Go Back" target:self selector:@selector(onGoBack:)];
		
		CCMenu *menu = [CCMenu menuWithItems: item1, nil];
		[menu alignItemsVertically];
		
		[self addChild: menu];
		
		[self schedule:@selector(testDealloc:)];
		
		CGSize s = [CCDirector sharedDirector].winSize;
		CCSprite *sprite = [CCSprite spriteWithFile:@"grossini.png"];
		[self addChild:sprite];
		sprite.position = ccp(40, s.height/2);
		id rotate = [CCRotateBy actionWithDuration:2 angle:360];
		id repeat = [CCRepeatForever actionWithAction:rotate];
		[sprite runAction:repeat];		
	}

	return self;
}

-(void) dealloc
{
	NSLog(@"LevelMenuLayer - dealloc");
	[super dealloc];
}

-(void) testDealloc:(ccTime) dt
{
	NSLog(@"LevelMenuLayer:testDealloc");
}

-(void) onGoBack:(id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [MainMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene: [CCTransitionFlipX transitionWithDuration:.5 scene:scene]];
}
@end

#pragma mark -
#pragma mark MultiplayerMenuLayer

@implementation MultiplayerMenuLayer
-(id) init
{
	if( (self=[super initWithColor: ccc4(255,0,0,255)]) ) {
		
		CCMenuItemFont *item1 = [CCMenuItemFont itemFromString: @"Go Back" target:self selector:@selector(onGoBack:)];
		
		CCMenu *menu = [CCMenu menuWithItems: item1, nil];
		[menu alignItemsVertically];
		
		[self addChild: menu];
		
		[self schedule:@selector(testDealloc:)];
		
		CGSize s = [CCDirector sharedDirector].winSize;
		CCSprite *sprite = [CCSprite spriteWithFile:@"grossini.png"];
		[self addChild:sprite];
		sprite.position = ccp(40, s.height/2);
		id rotate = [CCRotateBy actionWithDuration:2 angle:360];
		id repeat = [CCRepeatForever actionWithAction:rotate];
		[sprite runAction:repeat];		
	}
	
	return self;
}

-(void) dealloc
{
	NSLog(@"MultiplayerMenuLayer - dealloc");
	[super dealloc];
}

-(void) testDealloc:(ccTime) dt
{
	NSLog(@"MultiplayerMenuLayer:testDealloc");
}

-(void) onGoBack:(id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [MainMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene: [CCTransitionFlipX transitionWithDuration:.5 scene:scene]];
}
@end

#pragma mark -
#pragma mark OptionsMenuLayer

@implementation OptionsMenuLayer
-(id) init
{
	if( (self=[super initWithColor: ccc4(255,0,0,255)]) ) {
		
		CCMenuItemFont *item1 = [CCMenuItemFont itemFromString: @"Go Back" target:self selector:@selector(onGoBack:)];
		
		CCMenu *menu = [CCMenu menuWithItems: item1, nil];
		[menu alignItemsVertically];
		
		[self addChild: menu];
		
		[self schedule:@selector(testDealloc:)];
		
		CGSize s = [CCDirector sharedDirector].winSize;
		CCSprite *sprite = [CCSprite spriteWithFile:@"grossini.png"];
		[self addChild:sprite];
		sprite.position = ccp(40, s.height/2);
		id rotate = [CCRotateBy actionWithDuration:2 angle:360];
		id repeat = [CCRepeatForever actionWithAction:rotate];
		[sprite runAction:repeat];		
	}
	
	return self;
}

-(void) dealloc
{
	NSLog(@"OptionsMenuLayer - dealloc");
	[super dealloc];
}

-(void) testDealloc:(ccTime) dt
{
	NSLog(@"OptionsMenuLayer:testDealloc");
}

-(void) onGoBack:(id) sender
{
	CCScene * scene = [CCScene node];
	[scene addChild: [MainMenuLayer node] z:0];
	[[CCDirector sharedDirector] replaceScene: [CCTransitionFlipX transitionWithDuration:.5 scene:scene]];
}
@end
