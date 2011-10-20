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
	private Canvas mBitmapCanvas; // a canvas that uses the BitmapDrawable's bitmap
	private final int particleSize = 4;
	
	private int maxXParticles = 0;
	private int maxYParticles = 0;

    public PixelzooView(Context context, SurfaceHolder.Callback callback) { 
        super(context);

        this.setWillNotDraw(false); // necessary for View to call onDraw

        initBitmap(600, 800);
        
        if(callback!= null) {
        	getHolder().addCallback(callback);
        }
        setFocusable(true);

        Log.d("PixelzooView", "Created");
    }
    
    @Override
    public void onSizeChanged(int w, int h, int oldw, int oldh) {
    	super.onSizeChanged(w, h, oldw, oldh);
    	
    	initBitmap(w, h);

    	maxXParticles = w / particleSize;
    	maxYParticles = h / particleSize;
    	
    	Log.d("PixelzooView", "resize");
    }
    
    private void initBitmap(int width, int height) {
    	// TODO: copy in old bitmap.
    	if(mBitmapDrawable != null) {
	    	BitmapDrawable newBitmapDrawable = new BitmapDrawable(Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888));
	    	newBitmapDrawable.setBounds(0, 0, width, height);
	    	mBitmapDrawable = newBitmapDrawable;
    	}
    	else {
    		mBitmapDrawable = new BitmapDrawable(Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888));
            mBitmapDrawable.setBounds(0, 0, width, height);
    	}
    	
    	mBitmapCanvas = new Canvas();
    	mBitmapCanvas.setBitmap(mBitmapDrawable.getBitmap());
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
    
    public void drawBoardTwoDTest(int[][] board) {
    	Paint paint = new Paint();
    	for(int x = 0; x < 128; ++x) {
	        for(int y = 0; y < 128; ++y) {
	            paint.setColor(board[x][y]);
	            mBitmapCanvas.drawRect((x * particleSize), (y * particleSize), (x * particleSize) + particleSize, (y * particleSize) + particleSize, paint);
	        }
    	}
	}
    
    public void drawBoard(int[] board) {
    	Paint paint = new Paint();
    	for(int x = 0; x < 128; ++x) {
	        for(int y = 0; y < 128; ++y) {
	            //int color = board[(128 * x) + y];
	            //if(color != 0)
	            //    Log.d("PixelzooView: color", color + "");

	            paint.setColor(board[(128 * x) + y]);
	            mBitmapCanvas.drawRect((x * particleSize), (y * particleSize), (x * particleSize) + particleSize, (y * particleSize) + particleSize, paint);
	        }
    	}
	}
	
    @Deprecated
	public void drawParticle(int x, int y, int color) {
        int surfaceX = x * particleSize;
        int surfaceY = y * particleSize;
        
        Paint paint = new Paint();
        paint.setColor(color);
                
        /*Canvas canvas = new Canvas();
        canvas.setBitmap(mBitmapDrawable.getBitmap());
        
        // TODO: check bounds. It appears as if nothing will be drawn if the rect is out of bounds at all, rather than it being clipped to show the parts that are in bounds.
        canvas.drawRect(surfaceX, surfaceY, surfaceX + particleSize, surfaceY + particleSize, paint);*/
        
        mBitmapCanvas.drawRect(surfaceX, surfaceY, surfaceX + particleSize, surfaceY + particleSize, paint);
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
