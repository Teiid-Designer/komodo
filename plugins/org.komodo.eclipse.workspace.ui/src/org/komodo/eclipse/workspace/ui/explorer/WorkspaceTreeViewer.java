package org.komodo.eclipse.workspace.ui.explorer;

import java.util.Collections;

import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DragDetectEvent;
import org.eclipse.swt.events.DragDetectListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Composite;

public class WorkspaceTreeViewer extends TreeViewer {

	private Font textFont;

	/**
	 * Construct an instance of DatatypeHierarchyTreeViewer.
	 * 
	 * @param parent
	 */
	public WorkspaceTreeViewer(Composite parent) {
		this(parent, SWT.MULTI);
	}

	public WorkspaceTreeViewer(Composite parent, int style) {
		super(parent, style | SWT.H_SCROLL | SWT.V_SCROLL);
		setContentProvider(new WorkspaceContentProvider());
		setLabelProvider(new WorkspaceLabelProvider());
		super.setInput(Collections.EMPTY_LIST);

		ColumnViewerToolTipSupport.enableFor(this);
        
//        textFont = new Font(null, "Droid Sans Mono", 10, 0); //SWT.BOLD); //"Monospace", 10, 0); FreeMono

//        this.getTree().setFont(textFont);
	}

	public void close() {
		if( textFont != null && !textFont.isDisposed() ) {
			textFont.dispose();
		}
	}
}