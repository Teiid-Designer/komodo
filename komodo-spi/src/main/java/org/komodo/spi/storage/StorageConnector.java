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

import java.io.InputStream;
import java.util.Properties;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A storage connector is a client to a certain type of storage.
 * Implementations will provide access to instances of their storage
 * repositories, allowing writes to the storage and retrieval of
 * inidividual artifacts
 */
public interface StorageConnector extends StringConstants {

    /**
     * The destination of a file
     */
    String FILE_DEST_PROPERTY = "file-dest-property";

    /**
     * @return the id of the connector
     */
    StorageConnectorId getId();

    /**
     * Write the {@link Exportable} to the storage according to the
     * parameters
     *
     * @param artifact
     * @param parameters
     * @throws Exception 
     */
    void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception;

    /**
     * Refreshes the connection and any cached files from the storage location
     * @return true if refresh was successful, false otherwise
     * @throws Exception 
     */
    boolean refresh() throws Exception;

    /**
     * @param location
     * @return input stream to the file located at the given location
     * @throws Exception
     */
    InputStream read(String location) throws Exception;

    /**
     * @return the walked tree-structure of the storage location.
     * Note. if the storage is not browseable then this will return <code>null</code>.
     * @throws Exception 
     */
    StorageTree<String> browse() throws Exception;

    /**
     * Dispose of this connector
     */
    void dispose();
}
