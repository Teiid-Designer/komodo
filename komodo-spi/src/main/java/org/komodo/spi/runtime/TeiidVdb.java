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

import java.util.Collection;
import java.util.List;
import java.util.Properties;

/**
 *
 */
public interface TeiidVdb {

    /**
     * Extension of a vdb file
     */
    static final String VDB_EXTENSION = "vdb"; //$NON-NLS-1$
    
    /**
     * Extension of a vdb file with dot appended
     */
    static final String VDB_DOT_EXTENSION = ".vdb"; //$NON-NLS-1$

    /**
     * Suffix of a dynamic vdb
     */
    static final String DYNAMIC_VDB_SUFFIX = "-vdb.xml"; //$NON-NLS-1$

    /**
     * @return the name
     */
    String getName();

    /**
     * @return deployed name
     */
    String getDeployedName();

    /**
     * @return the version
     */
    String getVersion();

    /**
     * @return <code>true</code> if this is a preview VDB
     */
    boolean isPreviewVdb();

    /**
     * @return <code>true</code> if this VDB is active
     */
    boolean isActive();

    /**
     * @return <code>true</code> if this VDB is loading
     */
    boolean isLoading();

    /**
     * @return <code>true</code> if this VDB failed
     */
    boolean hasFailed();

    /**
     * @return <code>true</code> if this VDB is removed
     */
    boolean wasRemoved();

    /**
     * @return any validity errors
     */
    List<String> getValidityErrors();

    /**
     * Does the VDB contain any models 
     * 
     * @return <code>true</code> if the vdb has any models 
     */
    boolean hasModels();

    /**
     * Get the names of all the models in this vdb
     * 
     * @return {@link Collection} of model names
     */
    Collection<String> getModelNames();

    /**
     * @param key
     * 
     * @return value of property or null
     */
    String getPropertyValue(String key);
    
    /**
     * 
     * @return vdb properties
     */
    Properties getProperties( );
    
    /**
     * 
     * @return xml string of the vdb
     * @throws Exception 
     */
    String export( ) throws Exception;

}
