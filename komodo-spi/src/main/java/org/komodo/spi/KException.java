/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
