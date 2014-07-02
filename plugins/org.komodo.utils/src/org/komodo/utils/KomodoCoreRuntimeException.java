/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 */
package org.komodo.utils;


/**
 *
 *
 */
public class KomodoCoreRuntimeException extends RuntimeException {
	
	Throwable child = null;
	
	/** An error code. */
    private String code;
    
    /**
     */
    private static final long serialVersionUID = 1L;

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * 
     */
    public KomodoCoreRuntimeException() {
        super();
    }

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * @param message
     */
    public KomodoCoreRuntimeException(String message) {
        super(message);
    }

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * @param code
     * @param message
     */
    public KomodoCoreRuntimeException(int code, String message) {
        super(message);
        setCode(Integer.toString(code));
    }

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * @param e
     */
    public KomodoCoreRuntimeException(Throwable e) {
        super(e);
    }

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * @param e
     * @param message
     */
    public KomodoCoreRuntimeException(Throwable e, String message) {
        super(message, e);
    }

    /**
     * Construct an instance of ModelerCoreRuntimeException.
     * @param e
     * @param code
     * @param message
     */
    public KomodoCoreRuntimeException(Throwable e, int code, String message) {
        this(code, message);
        child = e;
    }
    
    /**
     * 
     * @return child Throwable
     */
    public Throwable getChild() {
    	return this.child;
    }
    
    /**
     * Get the error code.
     *
     * @return The error code 
     */
    public String getCode() {
        return this.code;
    }
    
    private void setCode( String code ) {
        this.code = code;
    }

    @Override
    public String getMessage() {
        String message = super.getMessage();
        if (code == null || code.length() == 0 || message.startsWith(code)) {
            return message;
        }
        return code + " " + message; //$NON-NLS-1$
    } 

}
