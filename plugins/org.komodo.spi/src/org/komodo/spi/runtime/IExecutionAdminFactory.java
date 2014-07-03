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
}
