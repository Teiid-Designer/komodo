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
package org.komodo.storage.git;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnectorId;

public class GitStorageConnector implements StorageConnector {

    @Override
    public StorageConnectorId getId() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void write(Exportable artifact, Properties parameters) {
        // TODO Auto-generated method stub

    }

    @Override
    public InputStream synchronize(Exportable artifact) throws IOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void refresh() {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean canBrowse() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public TreeMap<String, String> browse() {
        // TODO Auto-generated method stub
        return null;
    }

}
