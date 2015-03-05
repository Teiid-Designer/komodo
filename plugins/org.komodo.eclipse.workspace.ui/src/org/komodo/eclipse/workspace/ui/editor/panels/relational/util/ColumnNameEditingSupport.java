package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Parameter;

public class ColumnNameEditingSupport  extends EditingSupport implements ViewerElementChangedListener {
	
	private TextCellEditor editor;
	
	private ViewerElementChangedListener listener;

	/**
	 * Create a new instance of the receiver.
	 * 
	 * @param viewer the column viewer
	 */
	public ColumnNameEditingSupport(ColumnViewer viewer) {
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
		if( element instanceof Column ) {
			return "<name>"; //((Column)element).getName(null);
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
			String oldValue = "<name>"; //((Column)element).getName(null);
			String newValue = (String)value;
			if( newValue != null && newValue.length() > 0 && !newValue.equalsIgnoreCase(oldValue)) {
				// TODO: ((Column)element).setName(newValue);
				notifyChanged(new ElementChangedEvent(element));
			}
		} else if( element instanceof Parameter ) {
			String oldValue = "<name>"; //((Column)element).getName(null);
			String newValue = (String)value;
			if( newValue != null && newValue.length() > 0 && !newValue.equalsIgnoreCase(oldValue)) {
				// TODO: ((Column)element).setName(newValue);
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