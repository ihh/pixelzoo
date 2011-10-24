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
    return JNI_VERSION_1_2;
}

// Asks androidgame to redraw the entire board
JNIEXPORT jboolean
Java_com_pixelzoo_PixelzooActivity_requestRedrawBoard( JNIEnv* env, jobject thiz ) {
    // test
    AndroidGame *androidGame = malloc(sizeof(AndroidGame));
    androidGame->thiz = thiz;
    drawParticle(androidGame, 10, 10, 0xFF00FF00);
    free(androidGame);

    return 0;
}

JNIEXPORT jlong
Java_com_pixelzoo_PixelzooActivity_createAndroidGame( JNIEnv* env, jobject thiz )
{
	int argc = 5;
	char *argv[5];
	argv[0] = "sdlgame";
	argv[1] = "-g";
	argv[2] = "/sdcard/testgame.xml";
	argv[3] = "-b";
	argv[4] = "/sdcard/board.xml";

	AndroidGame *androidGame = createAndroidGame(argc, argv, thiz);
	return (jlong)((long)androidGame);
}

JNIEXPORT void
Java_com_pixelzoo_PixelzooActivity_runAndroidGame( JNIEnv* env, jobject thiz, jlong androidGamePtr ) {
	startAndroidGame((AndroidGame*)((long)androidGamePtr));
}

JNIEXPORT jint
Java_com_pixelzoo_PixelzooActivity_getNumberOfTools( JNIEnv* env, jobject thiz, jlong androidGamePtr ) {
    if(androidGamePtr)
        return (jint)(pzGetNumberOfTools((pzGame)((AndroidGame*)((long)androidGamePtr))->game));

    return 0;
}

JNIEXPORT jstring
Java_com_pixelzoo_PixelzooActivity_getToolName( JNIEnv* env, jobject thiz, jlong androidGamePtr, jint index ) {
    pzGame game = (pzGame)((AndroidGame*)((long)androidGamePtr))->game;
    pzTool tool = pzGetToolByNumber(game, (int)index);
    const char *toolName = pzGetToolName(tool);

    return (*env)->NewStringUTF(env, toolName);
}

JNIEXPORT void
Java_com_pixelzoo_PixelzooActivity_touchCell( JNIEnv* env, jobject thiz, jlong androidGamePtr, jint x, jint y ) {
    pzGame game = (pzGame)((AndroidGame*)((long)androidGamePtr))->game;

    // TODO: make this schedule a draw instead of doing it immediately
    LOGV("Touched %i %i", (int)x, (int)y);
    pzTouchCell(game, (int)x, (int)y);
}

JNIEXPORT void
Java_com_pixelzoo_PixelzooActivity_untouchCell( JNIEnv* env, jobject thiz, jlong androidGamePtr ) {
    pzGame game = (pzGame)((AndroidGame*)((long)androidGamePtr))->game;

    LOGV("Stopped touch");
    pzUntouchCell(game);
}

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
	int size = pzGetBoardSize(androidGame->game);

	jintArray row = (jintArray)(*env)->NewIntArray(env, size);
	jobjectArray board = (jobjectArray)(*env)->NewObjectArray(env, size, (*env)->GetObjectClass(env, row), NULL);

	int tmp[128];
	for(int j=0; j < size; ++j) {
		tmp[j] = 0xFF00FF00;
	}

	for(int i=0; i < size; ++i) {
	    row = (jintArray)(*env)->NewIntArray(env, size);
	    (*env)->SetIntArrayRegion(env, (jintArray)row,(jsize)0, size, tmp);
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

	int size = pzGetBoardSize(androidGame->game);

	// Initialize a int[][] in java (multi-dimensional arrays are object arrays)
	// TODO: need to check if 1D array is appreciably faster than a 2D array
	jintArray board = (jintArray)(*env)->NewIntArray(env, size * size);
	int **rgbArray = pzNewCellRgbArray(androidGame->game);

	// This method doesn't return a valid format. 24bit should be converted to ARGB_8888 for android.
	// pzReadCellRgbArray((pzGame)androidGame->game, rgbArray);
	int x, y, pal;
	for (x = 0; x < size; ++x)
	  for (y = 0; y < size; ++y)
	    rgbArray[x][y] = androidGame->sdlColor[pzGetCellPaletteIndex(androidGame->game, x, y)];

	for(int i=0; i < size; ++i) {
		(*env)->SetIntArrayRegion(env, (jintArray)board, (jsize)(i * size), size, rgbArray[i]);
	}
	pzDeleteCellRgbArray((pzGame)androidGame->game, rgbArray);

	(*env)->CallVoidMethod(env, thiz, mid, board);

	(*env)->DeleteLocalRef(env, board);
}
