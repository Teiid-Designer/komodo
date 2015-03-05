package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

public interface ViewerElementChangedListener {
	
	/**
	 * 
	 * @return element that changed
	 */
	void notifyChanged(ElementChangedEvent event);
}
