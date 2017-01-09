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
import java.util.Set;
import org.komodo.spi.bundle.BundleService;
import org.komodo.spi.storage.StorageConnector.Descriptor;

public interface StorageService extends BundleService {

    /**
     * The prefix of all storage service bundles
     */
    String STORAGE_BUNDLE_PREFIX = "org.komodo.storage" + HYPHEN;

    /**
     * The storage id property designated in the manifests of the Storage bundles
     */
    String STORAGE_ID_PROPERTY = "Storage-Connector-Id";

    /**
     * The class property designated in the manifests of the storage bundles
     */
    String CLASS_PROPERTY = "Storage-Service-Class";

    /**
     * @return the storage Id for this service
     * @throws Exception 
     */
    String getStorageId() throws Exception;

    /**
     * @return the description of this service
     * @throws Exception
     */
    String getDescription() throws Exception;

    /**
     * @return the set of applicable parameters for this storage connector
     * @throws Exception
     */
    Set<Descriptor> getDescriptors() throws Exception;

    /**
     * @param parameters
     * @return an instance of the {@link StorageConnector}
     * @throws Exception
     */
    StorageConnector getConnector(Properties parameters) throws Exception;
}
