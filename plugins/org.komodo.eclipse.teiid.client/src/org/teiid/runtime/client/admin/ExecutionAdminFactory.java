/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.teiid.runtime.client.admin;

import java.sql.Driver;
import java.util.HashMap;
import java.util.Map;

import org.komodo.spi.query.IQueryService;
import org.komodo.spi.runtime.IExecutionAdmin;
import org.komodo.spi.runtime.IExecutionAdminFactory;
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;
import org.teiid.core.types.DataTypeManagerService;
import org.teiid.jdbc.TeiidDriver;
import org.teiid.runtime.client.TeiidRuntimePlugin;
import org.teiid.runtime.client.query.QueryService;

/**
 *
 */
public class ExecutionAdminFactory implements IExecutionAdminFactory {

    private final Map<ITeiidVersion, DataTypeManagerService> dataTypeManagerServiceCache = new HashMap<ITeiidVersion, DataTypeManagerService>();
    
    private final Map<ITeiidVersion, QueryService> queryServiceCache = new HashMap<ITeiidVersion, QueryService>();

    @Override
    public IExecutionAdmin createExecutionAdmin(ITeiidInstance teiidInstance) throws Exception {
        return new ExecutionAdmin(teiidInstance);
    }
    
    @Override
    public IDataTypeManagerService getDataTypeManagerService(ITeiidVersion teiidVersion) {
        DataTypeManagerService dataTypeManagerService = dataTypeManagerServiceCache.get(teiidVersion);
        if (dataTypeManagerService == null) {
            dataTypeManagerService = DataTypeManagerService.getInstance(teiidVersion);
            dataTypeManagerServiceCache.put(teiidVersion, dataTypeManagerService);
        }

        return dataTypeManagerService;
    }

    @Override
    public Driver getTeiidDriver(ITeiidVersion teiidVersion) {
        TeiidDriver instance = TeiidDriver.getInstance();
        instance.setTeiidVersion(teiidVersion);
        return instance;
    }

    @Override
    public IQueryService getQueryService(ITeiidVersion teiidVersion) {
        QueryService queryService = queryServiceCache.get(teiidVersion);
        if (queryService == null) {
            queryService = new QueryService(teiidVersion);
            queryServiceCache.put(teiidVersion, queryService);
        }

        return queryService;
    }

    @Override
    public String getRuntimePluginPath() {
        return TeiidRuntimePlugin.getPluginPath();
    }
}