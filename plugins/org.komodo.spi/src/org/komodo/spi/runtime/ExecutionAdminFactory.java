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
package org.komodo.spi.runtime;

import java.sql.Driver;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;

/**
 * Factory for the creation of implementations of {@link ExecutionAdmin}
 */
public interface ExecutionAdminFactory {

    /**
     * Create an {@link ExecutionAdmin} with the given {@link TeiidInstance}
     * 
     * @param teiidInstance
     * 
     * @return instance of {@link ExecutionAdmin}
     * 
     * @throws Exception 
     */
    ExecutionAdmin createExecutionAdmin(TeiidInstance teiidInstance) throws Exception;

    /**
     * Get the teiid data type manager service
     * @param teiidVersion
     *
     * @return instance of {@link IDataTypeManagerService}
     */
    IDataTypeManagerService getDataTypeManagerService(TeiidVersion teiidVersion);

    /**
     * Get the {@link Driver} for the Teiid Instance
     * @param teiidVersion
     *
     * @return the driver
     */
    Driver getTeiidDriver(TeiidVersion teiidVersion);

    /**
     * Get the query service
     * @param teiidVersion
     * 
     * @return instance of {@link QueryService}
     */
    QueryService getQueryService(TeiidVersion teiidVersion);
}
