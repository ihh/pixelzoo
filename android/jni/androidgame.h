#ifndef _AndroidGame_H
#define _AndroidGame_H

#include <stdio.h>
#include <stdlib.h>
#include <jni.h>
#include <SDL.h>

#include <time.h>

#include "pixelzoo.h"
#include "pzutil.h"
#include "xmlutil.h"

// remove?
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

#include "optlist.h"

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

typedef struct AndroidGame {
  pzGame *game;
  Uint32 *sdlColor;
  char *moveLogFilename;
  char *boardFilename;
  Uint64 totalMicroticks;

  jobject thiz; // TODO: refactor this to Activity
} AndroidGame;

#include <androidhook.h>

#ifdef __cplusplus
extern "C" {
#endif

AndroidGame* createAndroidGame(int argc, char *argv[], jobject thiz);
AndroidGame* newAndroidGame(char *filename, jobject thiz, char *boardFilename, char *moveLogFilename, Uint64 totalMicroticks);
int startAndroidGame(AndroidGame *androidGame);
void deleteAndroidGame(AndroidGame* androidGame);
void render(AndroidGame* androidGame);
void renderAndDelay(AndroidGame* androidGame);
void renderPixel(AndroidGame* androidGame, int x, int y, Uint32 color);

#ifdef __cplusplus
}
#endif

#endif
