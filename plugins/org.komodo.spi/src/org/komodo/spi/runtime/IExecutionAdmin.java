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

import java.io.File;
import java.io.InputStream;
import java.sql.Driver;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.outcome.IOutcome;

/**
 *
 */
public interface IExecutionAdmin {

    /**
     * VDB name for the ping test
     */
    String PING_VDB = "ping-vdb.xml"; //$NON-NLS-1$

    /**
     * Type of connectivity
     */
    enum ConnectivityType {
        /**
         * Admin connection of the teiid instance
         */
        ADMIN, 
        
        /**
         * JDBC connection of the teiid instance
         */
        JDBC;
    }
    
    /**
     * Create a connection
     * 
     * @throws Exception
     */
    void connect() throws Exception;
    
    /**
     * Disconnects the connection
     */
    void disconnect();
    
    /**
     * @param name the name of the data source
     * @return true if data source exists with the provided name. else false.
     * @throws Exception 
     */
     boolean dataSourceExists(String name) throws Exception;

    /**
     * Removes the data source from the Teiid Instance (if exists)
     * 
     * @param dsName the data source name
     * @throws Exception if failure in deleting data source on teiid instance
     */
     void deleteDataSource(String dsName) throws Exception;

    /**
     * Returns a teiid data source object if it exists in this teiid instance
     * 
     * @param name the data source name
     * @return the teiid data source object (can be <code>null</code>)
     * @throws Exception 
     */
     ITeiidDataSource getDataSource(String name) throws Exception;

     /**
      * Returns all teiid data source object if any on this teiid instance
      * 
      * @return collection of {@link ITeiidDataSource}
      * 
      * @throws Exception
      */
     Collection<ITeiidDataSource> getDataSources() throws Exception;

     /**
      * Get the type names of the data sources
      * 
      * @return set of names
     * @throws Exception 
      */
     Set<String> getDataSourceTypeNames() throws Exception;

    /**
     * Get the specified DataSource, or create one if it does not exist.  When the datasource is create thru the admin API,
     * it is given a JNDI name which is the same as the sourceName.  For example if dsName 'mySource' is supplied, then the 
     * JNDI name is set as 'java:/mySource' (java context is added).  When the sources created by any other user are retrieved 
     * from the teiid instance, however, it is not guaranteed that the dsName and jndi name will match.
     * @param displayName the data source display name
     * @param dsName the data source name
     * @param typeName the translator type name
     * @param properties the list of teiid-related connection properties
     * @return true if data source is created. false if it already exists
     * @throws Exception if data source creation fails
     */
     ITeiidDataSource getOrCreateDataSource(String displayName,
                                                          String dsName,
                                                          String typeName,
                                                          Properties properties) throws Exception;

    /**
     * @param name the translator name (never <code>null</code> or empty)
     * @return a TeiidTranslator
     * @throws Exception 
     *
     */
     ITeiidTranslator getTranslator(String name) throws Exception;

    /**
     * 
     * @return collection of Teiid translators
     * @throws Exception 
     */
     Collection<ITeiidTranslator> getTranslators() throws Exception;

    /**
     * @return an unmodifiable collection of VDBs deployed on the teiid instance
     * @throws Exception 
     */
     Collection<ITeiidVdb> getVdbs() throws Exception;

     /**
      * @param name 
      * @return the {@link ITeiidVdb} with the given name
      * @throws Exception 
      */
     ITeiidVdb getVdb(String name) throws Exception;
     
     /**
      * @param name 
      * @return whether teiid instance contains a vdb with the given name
      * @throws Exception 
      */
     boolean hasVdb( String name ) throws Exception;
     
     /**
      * @param vdbName
      *  
      * @return <code>true</code> if the vdb is active
      * @throws Exception 
      */
     boolean isVdbActive(String vdbName) throws Exception;
     
     /**
      * @param vdbName
      *  
      * @return <code>true</code> if the vdb is loading
      * @throws Exception
      */
     boolean isVdbLoading(String vdbName) throws Exception;
     
     /**
      * @param vdbName
      *  
      * @return <code>true</code> if the vdb failed
      * @throws Exception
      */
     boolean hasVdbFailed(String vdbName) throws Exception;
     
     /**
      * @param vdbName 
      * 
      * @return <code>true</code> if the vdb was removed
      * @throws Exception
      */
     boolean wasVdbRemoved(String vdbName) throws Exception;
     
     /**
      * @param vdbName
      * 
      * @return any validity errors from the vdb when it was deployed
      * @throws Exception
      */
     List<String> retrieveVdbValidityErrors(String vdbName) throws Exception;
     
     /**
      * 
      * @param vdbName
      * @throws Exception
      */
     void undeployVdb(String vdbName) throws Exception;
     
    /**
     * Ping the admin client to determine whether if is still connected
     * @param connectivityType
     *
     * @return {@link IOutcome} describing state of ping
     * 
     * @throws Exception 
     */
     IOutcome ping(ConnectivityType connectivityType) throws Exception;
     
     /**
      * Get the location of the the admin driver class. Implementations have historically
      * derived this from the Admin class in the form:
      * 
      * Admin.class.getProtectionDomain().getCodeSource().getLocation().getFile();
      * 
      * @return {@link String} representation of location
      * 
      * @throws Exception 
      */
     String getAdminDriverPath() throws Exception;
     
     /**
      * Get the Teiid Instance driver for the given class
      * 
     * @param driverClass 
      * 
      * @return instance of {@link Driver}
      * 
     * @throws Exception 
      */
     Driver getTeiidDriver(String driverClass) throws Exception;
     
     /**
      * Deploys the Dynamic VDB (InputStream) to the related Teiid Instance
      * 
      * @param deploymentName the vdb deploymentName
      * @param inStream the VDB InputStream
      * 
      * @throws Exception if deployment fails
      */
     void deployDynamicVdb( String deploymentName, InputStream inStream ) throws Exception;

     /**
      * Undeploy the dynamic vdb
      * @param vdbName
      * @throws Exception
      */
     void undeployDynamicVdb(String vdbName) throws Exception;

     /**
      * Deploys a driver (jar or rar) to the related Teiid Instance
      * 
      * @param driverFile the file to deploy
      * 
      * @throws Exception if deployment fails
      */
     void deployDriver(File driverFile) throws Exception;

     /**
      * Get Model Schema DDL from the VDB
      * 
      * @param vdbName the name of the VDB
      * @param vdbVersion the VDB version
      * @param modelName the model name
      * @return the Schema DDL for the model
      * @throws Exception if deployment fails
      */
     String getSchema(String vdbName, int vdbVersion, String modelName) throws Exception;

     /**
      * Get Properties for a DataSource
      * 
      * @param name the data source name
      * @return the Properties for the data source
      * @throws Exception if deployment fails
      */
     Properties getDataSourceProperties(String name) throws Exception;

     /**
      * Get all DataSource template names
      * @return set of template names
      * @throws Exception
      */
     Set<String> getDataSourceTemplateNames() throws Exception;
    
     /**
      * Get Property definitions for the specified template
      * @param templateName
      * @return template property definitions
      * @throws Exception
      */
    Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws Exception;

}
