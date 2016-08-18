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
package org.komodo.spi.storage;

import java.util.Properties;
import org.komodo.spi.repository.DocumentType;

public class StorageReference {

    /**
     * property key for supplied driver name
     */
    public static final String DRIVER_NAME_KEY = "driverName";  //$NON-NLS-1$
    /**
     * default name for driver
     */
    public static final String DRIVER_NAME_DEFAULT = "defaultDriver";  //$NON-NLS-1$

    private final String storageType;

    private final Properties parameters;

    private DocumentType docType;

    public StorageReference(String storageType, Properties parameters, DocumentType documentType) {
        this.storageType = storageType;
        this.parameters = parameters;
        this.docType = documentType;
    }

    /**
     * @return the type of storage in which this reference is located
     */
    public String getStorageType() {
        return storageType;
    }

    /**
     * @return the parameters used to access the storage
     *                  in which this reference is located
     */
    public Properties getParameters() {
        return parameters;
    }

    /**
     * @return the relative reference to the exact location of this reference
     *                  within the storage
     */
    public String getRelativeRef() {
        return parameters.getProperty(StorageConnector.FILE_PATH_PROPERTY);
    }

    /**
     * @return the document type of this reference
     */
    public DocumentType getDocumentType() {
        return docType == null ? DocumentType.UNKNOWN : docType;
    }
}
