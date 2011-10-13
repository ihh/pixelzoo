package com.pixelzoo;

import android.util.Log;
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
        
        this.pv = new PixelzooView(this);
        setContentView(pv);
        pv.bringToFront();
        pv.invalidate();
        
        Log.d("PixelzooActivity", "Set PixelzooView");
        
        drawParticle(0, 0, 0xFF0000FF);
        drawParticle(1, 1, 0xFFFF0000);
        drawParticle(2, 2, 0xFF00FF00);
        runAndroidGame();
        // requestRedrawBoard();
    }
    
    @Override
    public void onResume() {
    	Log.d("PixelzooActivity", "Resumed");
    	super.onResume();
    	
    	// pause
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
    
    public void drawParticle(int x, int y, int color) {
    	pv.drawParticle(x, y, color);
    }
    
    // TODO: implement
	public void drawTools(int x, int y, int color) {
		
	}
    // public native String drawBoard();
}