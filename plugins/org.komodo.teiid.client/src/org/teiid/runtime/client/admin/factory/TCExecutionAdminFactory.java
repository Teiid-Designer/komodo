/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.teiid.runtime.client.admin.factory;

import java.sql.Driver;
import java.util.HashMap;
import java.util.Map;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.runtime.ExecutionAdmin;
import org.komodo.spi.runtime.ExecutionAdminFactory;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;
import org.teiid.core.types.DataTypeManagerService;
import org.teiid.jdbc.TeiidDriver;
import org.teiid.runtime.client.admin.TCExecutionAdmin;
import org.teiid.runtime.client.query.TCQueryService;

/**
 *
 */
public class TCExecutionAdminFactory implements ExecutionAdminFactory {

    private final Map<TeiidVersion, DataTypeManagerService> dataTypeManagerServiceCache = new HashMap<TeiidVersion, DataTypeManagerService>();
    
    private final Map<TeiidVersion, TCQueryService> queryServiceCache = new HashMap<TeiidVersion, TCQueryService>();

    @Override
    public ExecutionAdmin createExecutionAdmin(TeiidInstance teiidInstance) throws Exception {
        return new TCExecutionAdmin(teiidInstance);
    }
    
    @Override
    public IDataTypeManagerService getDataTypeManagerService(TeiidVersion teiidVersion) {
        DataTypeManagerService dataTypeManagerService = dataTypeManagerServiceCache.get(teiidVersion);
        if (dataTypeManagerService == null) {
            dataTypeManagerService = DataTypeManagerService.getInstance(teiidVersion);
            dataTypeManagerServiceCache.put(teiidVersion, dataTypeManagerService);
        }

        return dataTypeManagerService;
    }

    @Override
    public Driver getTeiidDriver(TeiidVersion teiidVersion) {
        TeiidDriver instance = TeiidDriver.getInstance();
        instance.setTeiidVersion(teiidVersion);
        return instance;
    }

    @Override
    public QueryService getQueryService(TeiidVersion teiidVersion) {
        TCQueryService queryService = queryServiceCache.get(teiidVersion);
        if (queryService == null) {
            queryService = new TCQueryService(teiidVersion);
            queryServiceCache.put(teiidVersion, queryService);
        }

        return queryService;
    }
}