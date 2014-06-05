/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.eclipse.spi.state;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.komodo.spi.state.IState;

/**
 * Wrapper for the {@link Status} object
 */
public class State implements IState {

	public static final IState OK = new State(Status.OK_STATUS);

	private final IStatus status;

	/**
	 * Create new instance 
	 */
	public State(IStatus status) {
		this.status = status;
	}

	public boolean isOK() {
		return status.isOK();
	}
}
