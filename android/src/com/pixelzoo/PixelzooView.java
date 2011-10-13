package com.pixelzoo;

import java.nio.Buffer;

import android.util.Log;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Picture;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RectShape;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;

public class PixelzooView extends SurfaceView {
	// TODO: 9patch drawable instead?
	private BitmapDrawable mBitmapDrawable;
	private final int particleSize = 4;
	
	private int maxXParticles = 0;
	private int maxYParticles = 0;

    public PixelzooView(Context context, SurfaceHolder.Callback callback) { 
        super(context);

        this.setWillNotDraw(false); // necessary for View to call onDraw

        mBitmapDrawable = new BitmapDrawable(Bitmap.createBitmap(600, 800, Bitmap.Config.ARGB_8888));
        mBitmapDrawable.setBounds(0, 0, 600, 800);
        
        if(callback!= null) {
        	getHolder().addCallback(callback);
        }
        setFocusable(true);

        Log.d("PixelzooView", "Created");
    }
    
    @Override
    public void onSizeChanged(int w, int h, int oldw, int oldh) {
    	super.onSizeChanged(w, h, oldw, oldh);
    	
    	// TODO: copy in old bitmap.
    	BitmapDrawable newBitmapDrawable = new BitmapDrawable(Bitmap.createBitmap(getWidth(), getHeight(), Bitmap.Config.ARGB_8888));
    	newBitmapDrawable.setBounds(0, 0, getWidth(), getHeight());
    	// mBitmapDrawable = newBitmapDrawable;

    	maxXParticles = w / particleSize;
    	maxYParticles = h / particleSize;
    	
    	Log.d("PixelzooView", "resize");
    }

    @Override
	protected void onDraw(Canvas canvas) {
    	super.onDraw(canvas);
    	
        myDraw(canvas);
    }
    
    private void myDraw(Canvas canvas) {
    	mBitmapDrawable.draw(canvas);
    }
    
    public void clearScreen() {
    	
    }
	
	public void drawParticle(int x, int y, int color) {
		ShapeDrawable particleDrawable = new ShapeDrawable(new RectShape());
		particleDrawable.getPaint().setColor(color);
        int surfaceX = x * particleSize;
        int surfaceY = y * particleSize;
                
        Canvas canvas = new Canvas();
        canvas.setBitmap(mBitmapDrawable.getBitmap());
        Paint paint = new Paint();
        paint.setColor(color);
        
        // TODO: check bounds. It appears as if nothing will be drawn if the rect is out of bounds at all, rather than it being clipped to show the parts that are in bounds.
        canvas.drawRect(surfaceX, surfaceY, surfaceX + particleSize, surfaceY + particleSize, paint);
                
        // invalidate();
        // 
	}
	
	public void endDraw() {
		repaint();
	}
	
	private void repaint() {
		Canvas c = null;
		try {
			c = getHolder().lockCanvas();
			if(c != null) {
				myDraw(c);
			}
		} finally {
			if (c != null) {
				getHolder().unlockCanvasAndPost(c);
			}
		}
	}
}
