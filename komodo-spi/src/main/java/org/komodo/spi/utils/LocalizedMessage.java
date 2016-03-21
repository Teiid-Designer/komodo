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
package org.komodo.spi.utils;

/**
 * Represents a translated message.
 */
public class LocalizedMessage {

    private final String id;
    private final String localeCode;
    private final String message;

    /**
     * @param msgId
     *        the message identifier (cannot be empty)
     * @param msgLocale
     *        the message locale code (cannot be empty)
     * @param msgText
     *        the translated message (cannot be empty)
     */
    public LocalizedMessage( final String msgId,
                             final String msgLocale,
                             final String msgText ) {
        this.id = msgId;
        this.localeCode = msgLocale;
        this.message = msgText;
    }

    /**
     * @return the identifier (never empty)
     */
    public String getId() {
        return this.id;
    }

    /**
     * @return the locale code (never empty)
     */
    public String getLocaleCode() {
        return this.localeCode;
    }

    /**
     * @return the translated message (never empty)
     */
    public String getMessage() {
        return this.message;
    }

}
