#import "cocos2d.h"

//CLASS INTERFACE

@interface MainMenuLayer : CCLayerColor
{
}
-(void) onQuickPlay: (id) sender;
-(void) onLevels: (id) sender;
-(void) onMultiplayer: (id) sender;
-(void) onOptions: (id) sender;
@end

@interface LevelMenuLayer : CCLayerColor
{
}
-(void) onGoBack: (id) sender;
@end

@interface MultiplayerMenuLayer : CCLayerColor
{
}
-(void) onGoBack: (id) sender;
@end

@interface OptionsMenuLayer : CCLayerColor
{
}
-(void) onGoBack: (id) sender;
@end


