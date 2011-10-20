package com.pixelzoo;

import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.app.Activity;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.widget.TextView;

public class PixelzooActivity extends Activity {
	// C libraries that must be loaded for JNI
    static {
    	System.loadLibrary("hello-jni");

    	// Note that ndk does not automatically load dependencies!
    	System.loadLibrary("xml2");
    	System.loadLibrary("SDL");
        System.loadLibrary("pixelzoo");
        System.loadLibrary("androidhook");
    }
    
    PixelzooView pv;
	
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);        
        
        // setContentView(R.layout.main);

        //TextView  tv = new TextView(this);
        //tv.setText( runSDLGame() );
        //setContentView(tv);
        
        // this should suffice for now, but ideally, only the viewport needs a fixed orientation
        this.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        
        SurfaceHolder.Callback callback = new SurfaceHolder.Callback() {
				public void surfaceChanged(SurfaceHolder arg0, int arg1, int arg2, int arg3) {}
	
				public void surfaceCreated(SurfaceHolder arg0) {
					// start JNI thread here
					Log.d("SurfaceHolder.Callback", "should start drawing now");
					startGameThread();
					// runAndroidGame();
					// requestRedrawBoard();
				}
	
				public void surfaceDestroyed(SurfaceHolder arg0) {}
        	
        	};
        this.pv = new PixelzooView(this, callback);
        setContentView(pv);
        pv.bringToFront();
        // pv.invalidate();
        
        Log.d("PixelzooActivity", "Set PixelzooView");
        
        
        // runAndroidGame();
        // requestRedrawBoard();
    }
    
    Thread jniThread;
    
    private void startGameThread() {
        // INFO/dalvikvm(289): Landroid/view/ViewRoot$CalledFromWrongThreadException;: Only the original thread that created a view hierarchy can touch its views.
        Thread jniThread = new Thread(new Runnable() {
				public void run() {
					runAndroidGame();					
				}
        	});
        jniThread.setDaemon(true);
        jniThread.start();
    }
    
    @Override
    public void onResume() {
    	Log.d("PixelzooActivity", "Resumed");
    	super.onResume();
    }
    
    @Override
    public void onPause() {
    	super.onPause();
    	
    	// TODO: stop the game loop or something
    }
    
    public native String stringFromJNI();
    public native String runXmlTest();
    
    public native String runAndroidGame();
    public native void requestRedrawBoard();
    
    public void javaCall() {
    	Log.d("PixelZooActivity", "hello world");
    }
    
    @Deprecated
    public void endDraw() {
    	pv.endDraw();
    }
    
    public void drawBoardTwoDTest(int[][] board) {
    	pv.drawBoardTwoDTest(board);
        pv.endDraw();
    }
    
    public void drawBoard(int[] board) {
    	pv.drawBoard(board);
        pv.endDraw();
    }

    public void drawParticle(int x, int y, int color) {
    	pv.drawParticle(x, y, color);
    }
    
    // TODO: implement
	public void drawTools(int x, int y, int color) {
		
	}
}