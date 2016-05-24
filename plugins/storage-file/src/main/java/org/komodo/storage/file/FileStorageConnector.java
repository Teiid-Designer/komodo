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
package org.komodo.storage.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Properties;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnectorId;
import org.komodo.spi.storage.StorageNode;
import org.komodo.spi.storage.StorageTree;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;

public class FileStorageConnector implements StorageConnector {

    /**
     * The path to the home directory of the location of the files handled by this connector
     */
    public static final String FILES_HOME_PATH_PROPERTY = "files-home-path-property";

    private final StorageConnectorId id;

    private final Properties parameters;

    public FileStorageConnector(Properties parameters) {
        ArgCheck.isNotNull(parameters);
        ArgCheck.isNotEmpty(parameters.getProperty(FILES_HOME_PATH_PROPERTY));

        this.parameters = parameters;

        this.id = new StorageConnectorId() {

            @Override
            public String type() {
                return StorageServiceImpl.STORAGE_ID;
            }

            @Override
            public String location() {
                return getPath();
            }
        };
    }

    @Override
    public StorageConnectorId getId() {
        return id;
    }

    /**
     * @return repository path
     */
    public String getPath() {
        return parameters.getProperty(FILES_HOME_PATH_PROPERTY);
    }

    /**
     * @param parameters
     * @return the file destination from the given parameters
     */
    public String getFileDestination(Properties parameters) {
        return parameters.getProperty(FILE_DEST_PROPERTY);
    }

    @Override
    public InputStream read(String location) throws Exception {
        File destFile = new File(getPath(), location);
        if (destFile.exists())
            return new FileInputStream(destFile);

        throw new FileNotFoundException();
    }

    @Override
    public void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception {
        ArgCheck.isNotNull(parameters);
        String destination = getFileDestination(parameters);
        ArgCheck.isNotEmpty(destination);

        File destFile = new File(getPath(), destination);

        //
        // Write the file contents
        //
        String contents = artifact.export(transaction, parameters);
        FileUtils.write(contents.getBytes(), destFile);
    }

    @Override
    public boolean refresh() throws Exception {
        return true; // Not applicable to static filesystem
    }

    private void walk(StorageNode<String> node, File parent) {
        File[] list = parent.listFiles();

        if (list == null)
            return;

        for (File file : list) {
            StorageNode<String> child = node.addChild(file.getName());
            if (file.isDirectory())
                walk(child, file);
        }
    }

    @Override
    public StorageTree<String> browse() throws Exception {
        File dir = new File(getPath());
        StorageTree<String> storageTree = new StorageTree<String>();

        if (! dir.isDirectory())
            return storageTree;

        StorageNode<String> node = storageTree.addChild(dir.getName());
        walk(node, dir);

        return storageTree;
    }

    @Override
    public void dispose() {
        // Nothing to do
    }

}
