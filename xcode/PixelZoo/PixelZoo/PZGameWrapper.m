//
//  PZGameWrapper.m
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZGameWrapper.h"

@implementation PZGameWrapper

@synthesize game;

-(void)initGameFromXMLElement:(GDataXMLElement*)element {
    game = pzNewGameFromXmlString([[element XMLString] UTF8String], false);
	pzStartGame(game);
}

-(bool)isInitialized {
    return game != NULL;
}

-(void)dealloc {
    if (game != NULL)
        pzDeleteGame(game);
}

@end
