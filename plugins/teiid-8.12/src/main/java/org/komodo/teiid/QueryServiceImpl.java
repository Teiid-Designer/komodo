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
package org.komodo.teiid;

import java.sql.Connection;
import org.komodo.plugin.framework.teiid.AbstractQueryService;
import org.komodo.spi.KException;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.type.DataTypeManager;
import org.teiid.jdbc.TeiidDataSource;

public class QueryServiceImpl extends AbstractQueryService {

    public QueryServiceImpl(DataTypeManager dataTypeManager, String user, String password) {
        super(dataTypeManager, user, password);
    }

    @Override
    protected Connection getConnection(String vdb, String user, String password) throws Exception {
        TeiidDataSource ds = new TeiidDataSource();
        ds.setDatabaseName(vdb);
        ds.setUser(user);
        ds.setPassword(password);
        ds.setServerName(TeiidInstance.DEFAULT_HOST);
        ds.setPortNumber(TeiidJdbcInfo.DEFAULT_PORT);

        //
        // Ensure any runtime exceptions are always caught and thrown as KExceptions
        //
        try {
            return ds.getConnection();
        } catch (Throwable t) {
            throw new KException(t);
        }
    }
}
