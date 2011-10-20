package com.pixelzoo;
import android.util.Log;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Matrix;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

public class PixelzooView extends SurfaceView {
    private final int particleSize = 4;
    private final int boardSize = 128;
	Bitmap mSmallBitmap = Bitmap.createBitmap(boardSize, boardSize, Bitmap.Config.ARGB_8888);
	Matrix matrix;
	{
	    matrix = new Matrix();
	    matrix.postScale(particleSize, particleSize);
	}
	int frameSkipTimer = 0;
    final int frameSkip = 10;
	
	private int maxXParticles = 0;
	private int maxYParticles = 0;

    public PixelzooView(Context context, SurfaceHolder.Callback callback) { 
        super(context);

        this.setWillNotDraw(false); // necessary for View to call onDraw
        
        if(callback!= null) {
        	getHolder().addCallback(callback);
        }
        setFocusable(true);

        Log.d("PixelzooView", "Created");
    }
        
    @Override
    public void onSizeChanged(int w, int h, int oldw, int oldh) {
    	super.onSizeChanged(w, h, oldw, oldh);
    	
    	// TODO: change matrix

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
        canvas.drawBitmap(mSmallBitmap, matrix, null);
    }
    
    public void clearScreen() {
    	
    }
    
    public void drawBoard(int[] board) {
        // skip frames if slow
        // TODO: get information on when to skip frames from JNI; make this more similar to ZooGas
        if(++frameSkipTimer == frameSkipTimer) {
            frameSkipTimer %= frameSkipTimer;
            mSmallBitmap.setPixels(board, 0, boardSize, 0, 0, boardSize, boardSize);
            
            repaint();
        }
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
