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
package org.komodo.rest;

import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * An error originating from or caught by the Komodo REST application.
 */
public class KomodoRestException extends Exception implements StringConstants {

    private static final long serialVersionUID = 1L;

    /*
     * Possible for the toString() and getMessage() methods to
     * end up in an infinite loop. Simple flag to avoid this.
     */
    private boolean stopOverflow;

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

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer(super.toString());

        if (! stopOverflow) {
            stopOverflow = true;
            buf.append(NEW_LINE)
                    .append(StringUtils.exceptionToString(this));
            stopOverflow = false;
        }

        return buf.toString();
    }

    @Override
    public String getMessage() {
        StringBuffer buf = new StringBuffer(super.getMessage());

        if (! stopOverflow) {
            stopOverflow = true;
            buf.append(NEW_LINE)
                    .append(StringUtils.exceptionToString(this));
            stopOverflow = false;
        }

        return buf.toString();
    }
}
