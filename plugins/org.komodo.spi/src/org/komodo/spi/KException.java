/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi;

/**
 * A Komodo error.
 */
public class KException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * @param message
     *        the error message (cannot be empty)
     */
    public KException( final String message ) {
        super(message);

        if ((message == null) || message.isEmpty()) {
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeEmpty, "message")); //$NON-NLS-1$
        }
    }

    /**
     * @param message
     *        the error message (cannot be empty)
     * @param cause
     *        the initial error (cannot be <code>null</code>)
     */
    public KException( final String message,
                       final Throwable cause ) {
        super(message, cause);

        if ((message == null) || message.isEmpty()) {
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeEmpty, "message")); //$NON-NLS-1$
        }

        if (cause == null) {
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "cause")); //$NON-NLS-1$
        }
    }

    /**
     * @param cause
     *        the initial error (cannot be <code>null</code>)
     */
    public KException( final Throwable cause ) {
        super(cause);

        if (cause == null) {
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "cause")); //$NON-NLS-1$
        }
    }

}
