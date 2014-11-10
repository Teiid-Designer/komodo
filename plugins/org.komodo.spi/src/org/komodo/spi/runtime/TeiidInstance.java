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

import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.runtime.version.ITeiidVersion;
/**
 *
 *
 */
public interface TeiidInstance extends ExecutionAdmin, HostProvider {

    /**
     * @return the version information of this instance
     * @throws Exception 
     */
    ITeiidVersion getVersion() throws Exception;

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
     * An appropriate name for this Teiid Instance
     * 
     * @return {@link #getCustomLabel()} if available otherwise {@link #getUrl()}
     */
    String getDisplayName();
    
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
     * @return the custom label or <code>null</code> if not being used
     */
    String getCustomLabel();
    
    /**
     * @return the connection error message if the connection to the instance failed
     */
    String getConnectionError();
    
    /**
     * @param customLabel the new custom label or <code>null</code> or empty if the custom label is not being used
     */
    void setCustomLabel(String customLabel);
    
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

}
