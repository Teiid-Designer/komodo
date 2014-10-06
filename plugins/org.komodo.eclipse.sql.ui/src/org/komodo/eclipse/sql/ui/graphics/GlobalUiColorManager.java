/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.eclipse.sql.ui.graphics;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;



/**
 * This class provides the Modeler UI plugins a place to cache their required colors, insure there aren't any duplicates
 * ( Based on RGB values), and insure that the colors are Disposed when the UI plugin is shut-down.
 * @since 8.0
 */
public class GlobalUiColorManager {

    private static Map<RGB, Color> fColorTable = Collections.synchronizedMap(new HashMap<RGB, Color>(10));
    
    public static final Color NOTE_COLOR = getSystemColor(SWT.COLOR_DARK_BLUE);
    public static final Color EMPHASIS_COLOR = getSystemColor(SWT.COLOR_DARK_BLUE);
    public static final Color EMPHASIS_COLOR_DISABLED = getSystemColor(SWT.COLOR_WIDGET_NORMAL_SHADOW);
    
    private static Color varColor;

    /**
     * @param operation The operation to be executed in the SWT thread.
     * @param asynchronous True if the operation should be run asynchronously, meaning the calling thread will not be blocked.
     * @since 4.1
     */
    public static void runInSwtThread( final Runnable operation,
                                       final boolean asynchronous ) {
        Display display = (Display.getCurrent() == null ? Display.getDefault() : Display.getCurrent());
        if (Thread.currentThread() != display.getThread()) {
            if (asynchronous) {
                display.asyncExec(operation);
            } else {
                display.syncExec(operation);
            }
        } else {
            operation.run();
        }
    }

    static Color getSystemColor(final int colorID) {
    	varColor = null;
    	runInSwtThread(new Runnable() {
			@Override
			public void run() {
				varColor = Display.getDefault().getSystemColor(colorID);
			}
		}, true);
    	
    	return varColor;
    }
    /**
     * Method disposes of the colors.
     */
    public static void dispose() {
        //int nColors = fColorTable.values().size();
        Iterator<Color> e= fColorTable.values().iterator();
        while (e.hasNext()) {
            e.next().dispose();
        }

        fColorTable.clear();
        //System.out.println(" ModelerUiColorManager.dispose() called.  " + nColors + " Colors disposed");
    }
    /**
     * A getter method that returns a color.
     * @param rgb
     * @return Color
     */
    public static Color getColor(RGB rgb) {
        Color color= fColorTable.get(rgb);
        if (color == null) {
            color= new Color(Display.getCurrent(), rgb);
            //System.out.println(" ModelerUiColorManager.getColor(" + fColorTable.keySet().size()+ ")  Adding New Color = " + rgb );
            fColorTable.put(rgb, color);
        }
        return color;
    }

}
