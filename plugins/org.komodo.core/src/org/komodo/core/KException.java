/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.core;

import org.komodo.utils.ArgCheck;

/**
 * A Komodo error.
 */
public class KException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * @param message the error message (cannot be empty)
     */
    public KException(final String message) {
        super(message);
        ArgCheck.isNotEmpty(message);
    }

    /**
     * @param cause the initial error (cannot be <code>null</code>)
     */
    public KException(final Throwable cause) {
        super(cause);
        ArgCheck.isNotNull(cause);
    }

    /**
     * @param message the error message (cannot be empty)
     * @param cause the initial error (cannot be <code>null</code>)
     */
    public KException(final String message,
                      final Throwable cause) {
        super(message, cause);
        ArgCheck.isNotEmpty(message);
        ArgCheck.isNotNull(cause);
    }

}
