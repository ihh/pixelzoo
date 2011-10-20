#ifndef _AndroidGame_H
#define _AndroidGame_H

#include <stdio.h>
#include <stdlib.h>
#include <jni.h>
#include <SDL.h>

#include <time.h>

#include "xmlgame.h" // change to pixelzoo.h
#include "xmlmove.h"
#include "xmlutil.h"
#include "pixelzoo.h"
#include "pzutil.h"

// remove?
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

#include "optlist.h"

typedef struct AndroidGame {
  Game *game;
  Uint32 sdlColor[PaletteSize];

  jobject thiz; // TODO: refactor this to Activity
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

#ifdef __cplusplus
}
#endif

#endif
