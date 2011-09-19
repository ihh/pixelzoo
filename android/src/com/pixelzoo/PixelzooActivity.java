package com.pixelzoo;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class PixelzooActivity extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // setContentView(R.layout.main);
        
        /* Create a TextView and set its content.
         * the text is retrieved by calling a native
         * function.
         */
        TextView  tv = new TextView(this);
        tv.setText( runXmlTest() );
        // tv.setText( stringFromJNI() );
        setContentView(tv);
    }
    
    public native String stringFromJNI();
    public native String runXmlTest();
    
    static {
    	System.loadLibrary("hello-jni");
    	// Note that ndk does not automatically load dependencies!
    	System.loadLibrary("xml2");
    	System.loadLibrary("SDL");
        System.loadLibrary("xmltest");
    }
}