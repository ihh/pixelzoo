#include <androidhook.h>

 JavaVM *cached_jvm;
 // jclass Class_C;
 // jmethodID MID_C_g;
 JNIEXPORT jint JNICALL
 JNI_OnLoad(JavaVM *jvm, void *reserved)
 {
     JNIEnv *env;
     // jclass cls;
     cached_jvm = jvm;  /* cache the JavaVM pointer */

     if ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_2)) {
         return JNI_ERR; /* JNI version not supported */
     }
     /*cls = (*env)->FindClass(env, "C");
     if (cls == NULL) {
         return JNI_ERR;
     }
     // Use weak global ref to allow C class to be unloaded
     Class_C = (*env)->NewWeakGlobalRef(env, cls);
     if (Class_C == NULL) {
         return JNI_ERR;
     }
     // Compute and cache the method ID
     MID_C_g = (*env)->GetMethodID(env, cls, "g", "()V");
     if (MID_C_g == NULL) {
         return JNI_ERR;
     }*/
     return JNI_VERSION_1_2;
 }

JNIEXPORT void
Java_com_pixelzoo_PixelzooActivity_runAndroidGame( JNIEnv* env, jobject thiz )
{
	int argc = 5;
	char *argv[5];
	argv[0] = "sdlgame";
	argv[1] = "-g";
	argv[2] = "/sdcard/testgame.xml";
	argv[3] = "-b";
	argv[4] = "/sdcard/board.xml";

	LOGV("STARTING GAME");
	launch(argc, argv, thiz);
	LOGV("GAME ENDED");

	// jmethodID mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "javaCall", "()V");
	// (*env)->CallVoidMethod(env, thiz, mid);

    // return (*env)->NewStringUTF(env, "Success from SDL game!");
}

// Asks androidgame to redraw the entire board
JNIEXPORT jboolean
Java_com_pixelzoo_PixelzooActivity_requestRedrawBoard( JNIEnv* env, jobject thiz ) {
	// test
	AndroidGame *androidGame = SafeMalloc(sizeof(AndroidGame));
	androidGame->thiz = thiz;
	drawParticle(androidGame, 10, 10, 0xFF00FF00);
	SafeFree(androidGame);

	return 0;
}

void startParticle(AndroidGame* androidGame) {
	static jmethodID mid = 0;
	JNIEnv* env; // = AndroidGame->env;
	(*cached_jvm)->GetEnv(cached_jvm,
							   (void **)&env,
							   JNI_VERSION_1_2);
	jobject thiz = androidGame->thiz;

	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "startDraw", "()V");
	}

	(*env)->CallVoidMethod(env, thiz, mid);
}
/*void endDraw(AndroidGame* androidGame) {
	static jmethodID mid = 0;
	JNIEnv* env; // = AndroidGame->env;
	(*cached_jvm)->GetEnv(cached_jvm,
							   (void **)&env,
							   JNI_VERSION_1_2);
	jobject thiz = androidGame->thiz;

	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "endDraw", "()V");
	}

	(*env)->CallVoidMethod(env, thiz, mid);
}*/

void drawParticle(AndroidGame* androidGame, int x, int y, Uint32 color) {
	static jmethodID mid = 0;
	JNIEnv* env; // = AndroidGame->env;
	(*cached_jvm)->GetEnv(cached_jvm,
	                           (void **)&env,
	                           JNI_VERSION_1_2);
	jobject thiz = androidGame->thiz;

	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "drawParticle", "(III)V");
	}

	// asw12: not sure if cast from int to jint is always safe + portable
	(*env)->CallVoidMethod(env, thiz, mid, (jint)x, (jint)y, (jint)color);
}

void drawBoardTwoDTest(AndroidGame* androidGame) {
	static jmethodID mid = 0;
	JNIEnv* env; // = AndroidGame->env;
	(*cached_jvm)->GetEnv(cached_jvm,
	                           (void **)&env,
	                           JNI_VERSION_1_2);
	jobject thiz = androidGame->thiz;

	// 2D
	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "drawBoardTwoDTest", "([[I)V");
	}

	// Initialize a int[][] in java (multi-dimensional arrays are object arrays)
	// TODO: need to check if 1D array is appreciably faster than a 2D array
	jintArray row = (jintArray)(*env)->NewIntArray(env, androidGame->game->board->size);
	jobjectArray board = (jobjectArray)(*env)->NewObjectArray(env, androidGame->game->board->size, (*env)->GetObjectClass(env, row), NULL);

	int tmp[128];
	for(int j=0; j<androidGame->game->board->size; ++j) {
		tmp[j] = 0xFF00FF00;
	}

	for(int i=0; i<androidGame->game->board->size; ++i) {
	    row = (jintArray)(*env)->NewIntArray(env, androidGame->game->board->size);
	    (*env)->SetIntArrayRegion(env, (jintArray)row,(jsize)0, androidGame->game->board->size, tmp);
	    (*env)->SetObjectArrayElement(env, board, i, row);
	}

	(*env)->CallVoidMethod(env, thiz, mid, board);
}

void drawBoard(AndroidGame* androidGame) {
	static jmethodID mid = 0;
	JNIEnv* env; // = AndroidGame->env;
	(*cached_jvm)->GetEnv(cached_jvm,
	                           (void **)&env,
	                           JNI_VERSION_1_2);
	jobject thiz = androidGame->thiz;

	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "drawBoard", "([I)V");
	}

	// Initialize a int[][] in java (multi-dimensional arrays are object arrays)
	// TODO: need to check if 1D array is appreciably faster than a 2D array
	jintArray board = (jintArray)(*env)->NewIntArray(env, androidGame->game->board->size * androidGame->game->board->size);

	int tmp[128];
	for(int j=0; j<androidGame->game->board->size; ++j) {
		tmp[j] = 0xFF00FF00;
	}

	for(int i=0; i<androidGame->game->board->size; ++i) {
		(*env)->SetIntArrayRegion(env, (jintArray)board,(jsize)(i * 128), androidGame->game->board->size, tmp);
	}

	(*env)->CallVoidMethod(env, thiz, mid, board);
}
