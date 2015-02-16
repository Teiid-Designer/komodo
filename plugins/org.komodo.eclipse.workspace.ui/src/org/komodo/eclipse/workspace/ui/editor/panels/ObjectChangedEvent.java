/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.eclipse.workspace.ui.editor.panels;

import org.komodo.spi.repository.KomodoObject;

public class ObjectChangedEvent {
	private KomodoObject changedObject;
	
	public ObjectChangedEvent(KomodoObject changedNode) {
		super();
		this.changedObject = changedNode;
	}

	public KomodoObject getChangedObject() {
		return this.changedObject;
	}
}
