package com.pixelzoo;

import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
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
        System.loadLibrary("androidhook");
        
        // System.loadLibrary("pixelzoo"); // couldn't get this to compile separately with androidhook
    }
    
    PixelzooView pv;
    long androidGamePtr = 0;
	
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
        
        SurfaceHolder.Callback surfaceCallback = new SurfaceHolder.Callback() {
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
        
        View.OnTouchListener touchCallback = new View.OnTouchListener() {
                public boolean onTouch(View v, MotionEvent event) {
                    switch(event.getActionMasked()) {
                        case MotionEvent.ACTION_DOWN:
                        case MotionEvent.ACTION_MOVE:
                            touchCell(androidGamePtr, (int)(event.getY()/4), (int)(event.getX()/4));
                            break;
                        case MotionEvent.ACTION_UP:
                            untouchCell(androidGamePtr);
                            break;
                        default:
                            break;
                    }
                    
                    return true;
                }
            };
        
        this.pv = new PixelzooView(this, surfaceCallback, touchCallback);
        setContentView(pv);
        pv.bringToFront();
        // pv.invalidate();
        
        Log.d("PixelzooActivity", "Set PixelzooView");
        
        
        androidGamePtr = createAndroidGame();
    }
    
    Thread jniThread;
    
    private void startGameThread() {
        // INFO/dalvikvm(289): Landroid/view/ViewRoot$CalledFromWrongThreadException;: Only the original thread that created a view hierarchy can touch its views.
        Thread jniThread = new Thread(new Runnable() {
				public void run() {
					runAndroidGame(androidGamePtr);					
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
    
    // Native methods
    // JNI test methods
    public native String stringFromJNI();
    public native String runXmlTest();
    public native void requestRedrawBoard();
    
    // Initialization methods
    public native long createAndroidGame();
    public native void runAndroidGame(long androidGamePtr);
    
    // Tool methods
    public native int getNumberOfTools(long ptr);
    public native String getToolName(long ptr, int index);
    public native void selectTool(long ptr, int index);
    public native void unselectTool(long ptr);
    
    public native void touchCell(long ptr, int x, int y);
    public native void untouchCell(long ptr);
    
    public void javaCall() {
    	Log.d("PixelZooActivity", "hello world");
    }
        
    public void drawBoard(int[] board) {
    	pv.drawBoard(board);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        Log.d("PixelzooActivity", "Creating menu");
        
        int numTools = getNumberOfTools(androidGamePtr);
        Log.v("PixelzooActivity", "creating tools menu with " + numTools);
        
        for(int i = 0; i < numTools; ++i) {
            MenuItem item = menu.add(Menu.NONE, Menu.NONE, Menu.NONE, getToolName(androidGamePtr, i));
            
            final int temp = i;
            item.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
                    public boolean onMenuItemClick(MenuItem item) {
                        Log.v("PixelzooActivity", "Set tool to " + temp);
                        selectTool(androidGamePtr, temp);
                        return true;
                    }
                });
        }
        return true;
    }
}