#ifndef _AndroidGame_H
#define _AndroidGame_H

#include <stdio.h>
#include <stdlib.h>
#include <jni.h>
#include <SDL.h>

#include <time.h>

#include "xmlgame.h"
#include "xmlmove.h"
#include "xmlutil.h"
#include "optlist.h"

/*#include <android/log.h>
#define LOGV(...) __android_log_print(ANDROID_LOG_VERBOSE, "libnav", __VA_ARGS__)
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG  , "libnav", __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO   , "libnav", __VA_ARGS__)
#define LOGW(...) __android_log_print(ANDROID_LOG_WARN   , "libnav", __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR  , "libnav", __VA_ARGS__)*/

typedef struct AndroidGame {
  Game *game;
  Uint32 sdlColor[PaletteSize];

  // asw12: too hackish?
  // JNIEnv* env;
  jobject thiz;
} AndroidGame;

#include <androidhook.h>

#ifdef __cplusplus
extern "C" {
#endif

int launch(int argc, char *argv[], jobject thiz);
AndroidGame* newAndroidGame(char *filename, jobject thiz);
void deleteAndroidGame(AndroidGame* androidGame);
void render(AndroidGame* androidGame);
void renderAndDelay(AndroidGame* androidGame);
void renderPixel(AndroidGame* androidGame, int x, int y, Uint32 color);

int evolveThreadFunc ( void *voidGame );
int renderThreadFunc( void *voidAndroidGame );

#ifdef __cplusplus
}
#endif

#endif
