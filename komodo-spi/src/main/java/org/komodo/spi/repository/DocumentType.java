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

public class DocumentType implements StringConstants {

    /**
     * VDB-XML
     */
    public static final DocumentType VDB_XML = new DocumentType(StringConstants.VDB_DEPLOYMENT_SUFFIX);

    /**
     * CONNECTION
     */
    public static final DocumentType CONNECTION = new DocumentType(StringConstants.CONNECTION_SUFFIX);

    /**
     * ZIP
     */
    public static final DocumentType ZIP = new DocumentType(StringConstants.ZIP);

    /**
     * DDL
     */
    public static final DocumentType DDL = new DocumentType(StringConstants.DDL);

    /**
     * JAR
     */
    public static final DocumentType JAR = new DocumentType(StringConstants.JAR);

    /**
     * UNKNOWN
     */
    public static final DocumentType UNKNOWN = new DocumentType(StringConstants.EMPTY_STRING);

    private String type;

    public DocumentType(String type) {
        this.type = type;
    }

    @Override
    public String toString() {
        return this.type;
    }

    /**
     * @param name
     * @return a file name from the given name and the document type
     */
    public String fileName(String name) {
        if (name.endsWith(DOT + type))
            return name; // nothing to do

        if (type.contains(DOT))
            return name + type; // eg. myVdb -vdb.xml

        return name + DOT + type;
    }

    public static DocumentType createDocumentType(String name) {
        if (name == null)
            return DocumentType.UNKNOWN;

        if (name.endsWith(VDB_DEPLOYMENT_SUFFIX))
            return DocumentType.VDB_XML;

        if (name.endsWith(CONNECTION_SUFFIX))
            return DocumentType.CONNECTION;

        int dotIndex = name.lastIndexOf(DOT);
        if (dotIndex == -1)
            return DocumentType.UNKNOWN;

        String suffix = name.substring(dotIndex + 1);
        return new DocumentType(suffix);
    }

    public static DocumentType documentType(String docTypeValue) {
        return new DocumentType(docTypeValue);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DocumentType other = (DocumentType)obj;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }
}
