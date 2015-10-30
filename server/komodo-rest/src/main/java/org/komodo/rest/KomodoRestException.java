/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

/**
 * An error originating from or caught by the Komodo REST application.
 */
public class KomodoRestException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * @param message
     *        the error message (can be empty)
     */
    public KomodoRestException( final String message ) {
        this( message, null );
    }

    /**
     * @param message
     *        the error message (can be empty)
     * @param cause
     *        the cause (can be <code>null</code>)
     */
    public KomodoRestException( final String message,
                                final Throwable cause ) {
        super( message, cause );
    }

    /**
     * @param cause
     *          the cause (can be <code>null</code>)
     */
    public KomodoRestException(final Throwable cause) {
        super(cause);
    }
}
