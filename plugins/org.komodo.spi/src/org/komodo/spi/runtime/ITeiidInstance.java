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

import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.state.IState;
/**
 * @since 8.0
 *
 */
public interface ITeiidInstance extends IExecutionAdmin, HostProvider {

    /**
     * @return the version information of this instance
     */
    ITeiidVersion getVersion();

    /**
     * Disconnect then connect to this instance. This is preferable to 
     * calling {@link #disconnect()} and {@link #connect()} separately
     * since it only notifies at the end of the reconnection.
     */
    void reconnect();
    
    /**
     * @return TeiidAdminInfo
     */
    ITeiidAdminInfo getTeiidAdminInfo();

    /**
     * @return TeiidJdbcInfo
     */
    ITeiidJdbcInfo getTeiidJdbcInfo();
    
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
    ITeiidParent getParent();

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
     * Attempts to establish communication with the instance.
     * 
     * @return a status if the instance connection can be established (never <code>null</code>)
     */
    IState ping();
    
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
     * Attempts to establish communication with the specified instance for testing purposes only.
     * 
     * This results in the connection being closed.
     * 
     * @return a status if the instance connection can be established (never <code>null</code>)
     */
    IState testPing();

    /**
     * Test the jdbc connection
     * 
     * @param host
     * @param port
     * @param username
     * @param password
     * 
     * @return status as to the ping's success
     */
    IState testJDBCPing(String host,
                                         String port,
                                         String username,
                                         String password);
    
    /**
     * Construct a vdb data source
     * 
     * @param vdbName
     * @param displayName
     * @param jndiName
     * 
     * @return IState as to whether it succeeded
     */
    IState createVdbDataSource(String vdbName, String displayName, String jndiName);

    /**
     * Update this instance with the properties of the given instance
     * 
     * @param otherInstance
     */
    void update(ITeiidInstance otherInstance);

}
