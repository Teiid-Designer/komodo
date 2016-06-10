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
package org.komodo.spi.repository;

import org.komodo.spi.constants.StringConstants;

public enum DocumentType {

    XML(StringConstants.XML),

    ZIP(StringConstants.ZIP),

    DDL(StringConstants.DDL),

    UNKNOWN(StringConstants.EMPTY_STRING);

    private String type;

    DocumentType(String type) {
        this.type = type;
    }

    @Override
    public String toString() {
        return this.type;
    }

    public static DocumentType documentType(String docTypeValue) {
        for (DocumentType docType : values()) {
            if (docType.toString().equalsIgnoreCase(docTypeValue))
                return docType;
        }

        return UNKNOWN;
    }
}
