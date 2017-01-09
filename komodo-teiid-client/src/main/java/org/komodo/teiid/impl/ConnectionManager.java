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
package org.komodo.teiid.impl;

import java.sql.Connection;
import java.sql.Driver;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.teiid.AbstractConnectionManager;
import org.teiid.jdbc.TeiidDataSource;
import org.teiid.jdbc.TeiidDriver;

public class ConnectionManager extends AbstractConnectionManager implements StringConstants {

    private static ConnectionManager instance;

    public static ConnectionManager getInstance() {
        if (instance == null)
            instance = new ConnectionManager();

        return instance;
    }

    @Override
    public Connection getConnection(String vdb, String host, int port,
                                                                    String user, String password,
                                                                    boolean secure) throws Exception {
        TeiidDataSource ds = new TeiidDataSource();
        ds.setDatabaseName(vdb);
        ds.setUser(user);
        ds.setPassword(password);
        ds.setServerName(host);
        ds.setPortNumber(port);
        ds.setSecure(secure);

        //
        // Ensure any runtime exceptions are always caught and thrown as KExceptions
        //
        try {
            return ds.getConnection();
        } catch (Throwable t) {
            throw new KException(t);
        }
    }

    @Override
    protected Driver getTeiidDriver() {
        return TeiidDriver.getInstance();
    }
}