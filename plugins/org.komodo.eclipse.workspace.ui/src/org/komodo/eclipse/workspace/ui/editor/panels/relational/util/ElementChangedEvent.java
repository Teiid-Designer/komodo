package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

public class ElementChangedEvent {
	Object element;
	
	public ElementChangedEvent(Object element) {
		super();
		this.element = element;
	}

	public Object getElement() {
		return element;
	}
}
