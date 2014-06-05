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
package org.komodo.spi.runtime;

import java.sql.Driver;

import org.komodo.spi.query.IQueryService;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;

/**
 * Factory for the creation of implementations of {@link IExecutionAdmin}
 */
public interface IExecutionAdminFactory {

    /**
     * Create an {@link IExecutionAdmin} with the given {@link ITeiidInstance}
     * 
     * @param teiidInstance
     * 
     * @return instance of {@link IExecutionAdmin}
     * 
     * @throws Exception 
     */
    IExecutionAdmin createExecutionAdmin(ITeiidInstance teiidInstance) throws Exception;

    /**
     * Get the teiid data type manager service
     * @param teiidVersion
     *
     * @return instance of {@link IDataTypeManagerService}
     */
    IDataTypeManagerService getDataTypeManagerService(ITeiidVersion teiidVersion);

    /**
     * Get the {@link Driver} for the Teiid Instance
     * @param teiidVersion
     *
     * @return the driver
     */
    Driver getTeiidDriver(ITeiidVersion teiidVersion);

    /**
     * Get the query service
     * @param teiidVersion
     * 
     * @return instance of {@link IQueryService}
     */
    IQueryService getQueryService(ITeiidVersion teiidVersion);

    /**
     * Get the location of this class' parent plugin
     *
     * @return OS specific path to the plugin location
     */
    String getRuntimePluginPath();
}
