package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Parameter;

public class ColumnWidthEditingSupport extends EditingSupport implements ViewerElementChangedListener {
	
	private TextCellEditor editor;
	
	private ViewerElementChangedListener listener;
	
	/**
	 * Create a new instance of the receiver.
	 * 
	 * @param viewer the column viewer
	 */
	public ColumnWidthEditingSupport(ColumnViewer viewer) {
		super(viewer);
		this.editor = new TextCellEditor((Composite) viewer.getControl());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
	 */
	@Override
	protected boolean canEdit(Object element) {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
	 */
	@Override
	protected CellEditor getCellEditor(Object element) {
		return editor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
	 */
	@Override
	protected Object getValue(Object element) {
		if( element instanceof Column || element instanceof Parameter) {
			return 1; //Long.toString(((Column)element).getLength(null));
		} 
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object,
	 *      java.lang.Object)
	 */
	@Override
	protected void setValue(Object element, Object value) {
		if( element instanceof Column ) {
			long oldValue = 1; //((Column)element).getLength(null);
			long newValue = oldValue;
			try {
				newValue = Long.parseLong((String)value);
			} catch (NumberFormatException ex) {
				return;
			}
			if( newValue != oldValue ) {
				// TODO: ((Column)element).setLength(null, newValue);
				notifyChanged(new ElementChangedEvent(element));
			}
		} else if( element instanceof Parameter ) {
			long oldValue = 1; //((Parameter)element).getLength(null);
			long newValue = oldValue;
			try {
				newValue = Long.parseLong((String)value);
			} catch (NumberFormatException ex) {
				return;
			}
			if( newValue != oldValue ) {
				// TODO: ((Parameter)element).setLength(null, newValue);
				notifyChanged(new ElementChangedEvent(element));
			}
		}
	}
	
	@Override
	public void notifyChanged(ElementChangedEvent event) {
		if( this.listener != null ) {
			listener.notifyChanged(event);
		}
	}
	
	public void setViewerElementChangedListener(ViewerElementChangedListener listener) {
		this.listener = listener;
	}
}