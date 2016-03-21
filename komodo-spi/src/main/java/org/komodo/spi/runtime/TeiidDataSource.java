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

import java.util.Properties;

/**
 *
 */
public interface TeiidDataSource {
	
	public interface ERROR_CODES {
		// Data Source issues
		int JDBC_DRIVER_SOURCE_NOT_FOUND = 101; // JDBC Source for Driver class {0} was not found on teiid instance {1}
		int DATA_SOURCE_TYPE_DOES_NOT_EXIST_ON_TEIID = 102; // Data Source Type {0} does not exist on teiid instance {1}
		int DATA_SOURCE_COULD_NOT_BE_CREATED = 103; // errorCreatingDataSource = Data Source {0} could not be created for type {1}
		int NO_CONNECTION_PROVIDER = 104; //
		int NO_CONNECTION_PROFILE_DEFINED_IN_MODEL = 105; // 
		int NO_TEIID_RELATED_PROPERTIES_IN_PROFILE = 106; // 
		int COULD_NOT_GET_OR_CREATE_DATASOURCE = 107; // 
	}

    /**
     * @return display name of data source
     */
    String getDisplayName();

    /**
     * @return real name of data source, maybe different from display name
     */
    String getName();

    /**
     * Returns the data source type name
     * 
     * @return the type
     */
    String getType();

    /**
     * Returns the data source jndi name
     *
     * @return the jndi name
     */
    String getJndiName();

    /**
     * Returns the data source connection url
     *
     * @return the connection url
     */
    String getConnectionUrl();

    /**
     * @return properties of data source
     */
    Properties getProperties();

    /**
     * @param name
     * 
     * @return value of named property
     */
    String getPropertyValue(String name);

}
