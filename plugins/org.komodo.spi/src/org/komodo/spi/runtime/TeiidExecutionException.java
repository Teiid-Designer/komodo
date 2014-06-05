/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.runtime;

/**
 *
 */
public class TeiidExecutionException extends Exception {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	int code;

	/**
	 * 
	 */
	public TeiidExecutionException(int code) {
		this.code = code;
	}
	
	/**
	 * 
	 */
	public TeiidExecutionException() {
	}

	/**
	 * @param message
	 */
	public TeiidExecutionException(int code, String message) {
		super(message);
		this.code = code;
	}

	/**
	 * @param cause
	 */
	public TeiidExecutionException(int code, Throwable cause) {
		super(cause);
		this.code = code;
	}

	/**
	 * @param message
	 * @param cause
	 */
	public TeiidExecutionException(int code, String message, Throwable cause) {
		super(message, cause);
		this.code = code;
	}
	
	public int getCode() {
		return this.code;
	}

}
