#ifndef _AndroidHook_H
#define _AndroidHook_H

#include <string.h>
#include <jni.h>
#include "androidgame.h"

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

#include <android/log.h>
#define LOGV(...) __android_log_print(ANDROID_LOG_VERBOSE, "libnav", __VA_ARGS__)
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG  , "libnav", __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO   , "libnav", __VA_ARGS__)
#define LOGW(...) __android_log_print(ANDROID_LOG_WARN   , "libnav", __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR  , "libnav", __VA_ARGS__)

JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_runAndroidGame( JNIEnv* env, jobject thiz );

// Asks androidgame to redraw the entire board
JNIEXPORT jboolean Java_com_pixelzoo_PixelzooActivity_requestRedrawBoard(JNIEnv* env, jobject thiz);

void drawParticle(AndroidGame *game, int x, int y, Uint32 color);

#endif
