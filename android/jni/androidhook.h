#ifndef _AndroidHook_H
#define _AndroidHook_H

#include <string.h>
#include <jni.h>
#include "androidgame.h"

#include <android/log.h>
#define LOGV(...) __android_log_print(ANDROID_LOG_VERBOSE, "libnav", __VA_ARGS__)
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG  , "libnav", __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO   , "libnav", __VA_ARGS__)
#define LOGW(...) __android_log_print(ANDROID_LOG_WARN   , "libnav", __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR  , "libnav", __VA_ARGS__)

// Asks androidgame to redraw the entire board
JNIEXPORT jboolean Java_com_pixelzoo_PixelzooActivity_requestRedrawBoard(JNIEnv* env, jobject thiz);

JNIEXPORT jlong Java_com_pixelzoo_PixelzooActivity_createAndroidGame( JNIEnv* env, jobject thiz );
JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_runAndroidGame( JNIEnv* env, jobject thiz, jlong androidGamePtr );

JNIEXPORT jint Java_com_pixelzoo_PixelzooActivity_getNumberOfTools( JNIEnv* env, jobject thiz, jlong androidGamePtr );
JNIEXPORT jstring Java_com_pixelzoo_PixelzooActivity_getToolName( JNIEnv* env, jobject thiz, jlong androidGamePtr, jint index );
JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_selectTool( JNIEnv* env, jobject thiz, jlong androidGamePtr, jint index );
JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_unselectTool( JNIEnv* env, jobject thiz, jlong androidGamePtr );

JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_touchCell( JNIEnv* env, jobject thiz, jlong androidGamePtr, jint x, jint y );
JNIEXPORT void Java_com_pixelzoo_PixelzooActivity_untouchCell( JNIEnv* env, jobject thiz, jlong androidGamePtr );

void drawBoard(AndroidGame *game);
void drawParticle(AndroidGame *game, int x, int y, Uint32 color);

#endif
