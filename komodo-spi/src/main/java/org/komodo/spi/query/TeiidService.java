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
package org.komodo.spi.query;

import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;

public interface TeiidService {

    /**
     * @return version of this service
     * @throws Exception 
     */
    TeiidVersion getVersion() throws Exception;

    /**
     * @return the data type manager
     * @throws Exception 
     */
    DataTypeManager getDataTypeManager() throws Exception;

    /**
     * Convert the sql into a tree beneath parent
     *
     * @param sql
     * @param parent
     *                  Expected to be of type javax.jcr.Node but set to Object to avoid interface having the dependency
     * @throws Exception 
     */
    void nodeConvert(String sql, Object parent) throws Exception;

    /**
     * Get the runtime teiid client instance
     *
     * @param teiidParent
     * @param jdbcInfo
     * @return an instance model of the teiid client
     * @throws Exception 
     */
    TeiidInstance getTeiidInstance(TeiidParent teiidParent, TeiidJdbcInfo jdbcInfo) throws Exception;

    /**
     * @param host the host
     * @param port the port
     * @param user jdbc username
     * @param passwd jdbc password
     * @param isSecure is connection secure
     * @return the query service for the version of teiid
     */
    QueryService getQueryService(String host, int port, String user, String passwd, boolean isSecure) throws Exception;

    /**
     * Dispose of this service
     */
    void dispose();
}
