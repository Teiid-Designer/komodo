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

/**
 *
 */
public interface ITeiidJdbcInfo extends ITeiidConnectionInfo {

    /**
     * The default Teiid JDBC port number. Value is {@value} .
     */
    public static final int DEFAULT_PORT = 31000;
    /**
     * The default Teiid Admin secure protocol flag. Value is {@value} .
     */
    public static final boolean DEFAULT_SECURE = false;
    /**
     * The default username for the Teiid Instance
     */
    public static final String DEFAULT_JDBC_USERNAME = "user"; //$NON-NLS-1$
    /**
     * The default password for the Teiid Instance
     */
    public static final String DEFAULT_JDBC_PASSWORD = "user"; //$NON-NLS-1$

}
