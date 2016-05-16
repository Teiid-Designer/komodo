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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.spi.repository.Exportable;

/**
 * A storage connector is a client to a certain type of storage.
 * Implementations will provide access to instances of their storage
 * repositories, allowing writes to the storage and retrieval of
 * inidividual artifacts
 */
public interface StorageConnector {

    /**
     * @return the id of the connector
     */
    public StorageConnectorId getId();

    /**
     * Write the {@link Exportable} to the storage according to the
     * parameters
     *
     * @param artifact
     * @param parameters
     */
    public void write(Exportable artifact, Properties parameters);

    /**
     * @param artifact
     * @return Return the stream of the given artifact from the storage
     *                  location. If cannot be found or does not exist then returns <code>null</code>.
     * @throws if IO error occurs
     */
    public InputStream synchronize(Exportable artifact) throws IOException;

    /**
     * Refreshes the connection and any cached files from the storage location
     */
    public void refresh();

    /**
     * @return true if the storage can is browsable. For example, a
     * git repository is browsable while a JDBC database is not.
     */
    public boolean canBrowse();

    /**
     * @return the walked tree-structure of the storage location.
     * Note. if the storage is not browseable then this will return <code>null</code>.
     */
    public TreeMap<String, String> browse();
}
