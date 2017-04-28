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
import java.util.Set;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A storage connector is a client to a certain type of storage.
 * Implementations will provide access to instances of their storage
 * repositories, allowing writes to the storage and retrieval of
 * individual artifacts
 */
public interface StorageConnector extends StringConstants {

    /**
     * Describes a parameter/property applicable to this storage connector
     */
    static class Descriptor {

        private String name;

        private boolean required;

        private String description;

        private boolean encoded = false;

        public Descriptor(String name, boolean required, boolean encoded, String description) {
            this.name = name;
            this.required = required;
            this.description = description;
            this.encoded = encoded;
        }

        public Descriptor(String name, boolean required, String description) {
            this(name, required, false, description);
        }

        public String getName() {
            return name;
        }

        public boolean isRequired() {
            return required;
        }

        public String getDescription() {
            return description;
        }

        public boolean isEncoded() {
            return encoded;
        }
    }

    /**
     * The path to the home directory of the location of files. Used by some connectors.
     */
    String FILES_HOME_PATH_PROPERTY = "files-home-path-property";

    /**
     * The path where the file should be located
     */
    String FILE_PATH_PROPERTY = "file-path-property";

    /**
     * Should a file be 'downloadable' once stored then this property is populated
     */
    String DOWNLOADABLE_PATH_PROPERTY = "downloadable-path-property";

    /**
     * Parameter to specify overwrite option for imports
     */
    String IMPORT_OVERWRITE_PROPERTY = "import-overwrite-property";

    /**
     * @return the id of the connector
     */
    StorageConnectorId getId();

    /**
     * @return the applicable parameters for this storage connector
     */
    Set<Descriptor> getDescriptors();

    /**
     * Write the {@link Exportable} to the storage according to the
     * parameters
     *
     * @param artifact
     * @param parameters
     *
     * @throws Exception if error occurs
     */
    void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception;

    /**
     * Refreshes the connection and any cached files from the storage location
     * @return true if refresh was successful, false otherwise
     * @throws Exception 
     */
    boolean refresh() throws Exception;

    /**
     * @param parameters the parameters used to find and read the document.
     *                  In most cases this should at least contain {@link #FILE_PATH_PROPERTY} which
     *                  points to  relative reference to the file
     * @return input stream to the file located at the given location
     * @throws Exception
     */
    InputStream read(Properties parameters) throws Exception;

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
