/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

/**
 *
 */
public class KomodoRestException extends Exception {

    /**
     * @param message
     */
    public KomodoRestException( final String message ) {
        this( message, null );
    }

    /**
     * @param message
     * @param cause
     */
    public KomodoRestException( final String message,
                                 final Throwable cause ) {
        super( message, cause );
    }

}
