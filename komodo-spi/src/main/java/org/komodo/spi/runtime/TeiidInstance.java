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
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.runtime.version.TeiidVersion;
/**
 *
 *
 */
public interface TeiidInstance extends ExecutionAdmin, HostProvider {

    /**
     * Object used for synchronization locking on teiid instance implementations
     */
    Object TEIID_INSTANCE_LOCK = new Object();

    /**
     * The data source jndi property name.  Value is {@value} .
     */
    String DATASOURCE_JNDINAME = "jndi-name";  //$NON-NLS-1$
    
    /**
     * The data source className property name.  Value is {@value} .
     */
    String DATASOURCE_CLASSNAME = "class-name";  //$NON-NLS-1$

    /**
     * The data source driver property name.  Value is {@value} .
     */
    String DATASOURCE_DRIVERNAME = "driver-name";  //$NON-NLS-1$

    /**
     * The connection url property name. Value is {@value}.
     */
    String DATASOURCE_CONNECTION_URL = "connection-url"; //$NON-NLS-1$

    /**
     * The display name property.
     */
    String DATASOURCE_DISPLAYNAME = "display-name"; //$NON-NLS-1$
    
    /**
     * @return the version information of this instance, either the client or runtime version
     *                  depending on connection, ie. unconnected returns client while connected returns runtime.
     *
     * @throws Exception 
     */
    TeiidVersion getVersion();

    /**
     * @return the client version of this instance
     */
    TeiidVersion getClientVersion();

    /**
     * @return the runtime / server version of this instance
     * @throws Exception 
     */
    TeiidVersion getRuntimeVersion() throws Exception;

    /**
     * Disconnect then connect to this instance. This is preferable to 
     * calling {@link #disconnect()} and {@link #connect()} separately
     * since it only notifies at the end of the reconnection.
     */
    void reconnect();
    
    /**
     * @return TeiidAdminInfo
     */
    TeiidAdminInfo getTeiidAdminInfo();

    /**
     * @return TeiidJdbcInfo
     */
    TeiidJdbcInfo getTeiidJdbcInfo();
    
    /**
     * @return object managing notifications for this instance
     */
    EventManager getEventManager();

    /**
     * @return the host URL (never <code>null</code>)
     */
    String getUrl();

    /**
     * @return the unique identifier of this instance
     */
    String getId();

    /**
     * @return the version of teiid this instance was built with
     */
    TeiidVersion getSupportedVersion();

    /**
     * @return the teiid instance parent
     */
    TeiidParent getParent();

    /**
     * Get the parent instance name
     * @return the parent instanceName
     */
    String getParentName();

    /**
     * @return <code>true</code> if a connection to this instance exists and is working
     */
    boolean isConnected();

    /**
     * Return whether parent instance is connected.
     * 
     * @return true is started, otherwise false
     */
    boolean isParentConnected();
    
    /**
     * Notify clients of a refresh
     */
    void notifyRefresh();
    
    /**
     * @return the connection error message if the connection to the instance failed
     */
    String getConnectionError();

    /**
     * Construct a vdb data source
     * 
     * @param vdbName
     * @param displayName
     * @param jndiName
     * 
     * @return IState as to whether it succeeded
     */
    Outcome createVdbDataSource(String vdbName, String displayName, String jndiName);

    /**
     * Update this instance with the properties of the given instance
     * 
     * @param otherInstance
     */
    void update(TeiidInstance otherInstance);

    /**
     * @return the collection of data source drivers resident on the server
     * @throws Exception
     */
    Collection<ConnectionDriver> getDataSourceDrivers() throws Exception;

    /**
     * @return whether this teiid instance is still valid to be used
     */
    boolean isSound();
}
