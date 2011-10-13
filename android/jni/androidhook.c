#include <androidhook.h>

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
	launch(argc, argv, env, thiz);
	LOGV("GAME ENDED");

	// jmethodID mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "javaCall", "()V");
	// (*env)->CallVoidMethod(env, thiz, mid);

    // return (*env)->NewStringUTF(env, "Success from SDL game!");
}

// Asks androidgame to redraw the entire board
JNIEXPORT jboolean
Java_com_pixelzoo_PixelzooActivity_requestRedrawBoard( JNIEnv* env, jobject thiz ) {
	// test
	AndroidGame *AndroidGame = SafeMalloc(sizeof(AndroidGame));
	AndroidGame->env = env;
	AndroidGame->thiz = thiz;
	drawParticle(AndroidGame, 10, 10, 0xFF0000FF);
	SafeFree(AndroidGame);

	return 0;
}

void drawParticle(AndroidGame* AndroidGame, int x, int y, Uint32 color) {
	static jmethodID mid = 0;
	JNIEnv* env = AndroidGame->env;
	jobject thiz = AndroidGame->thiz;

	if(!mid) {
		mid = (*env)->GetMethodID(env, (*env)->GetObjectClass(env, thiz), "drawParticle", "(III)V");
	}

	// asw12: not sure if cast from int to jint is always safe + portable
	(*env)->CallVoidMethod(env, thiz, mid, (jint)x, (jint)y, (jint)color);
}
