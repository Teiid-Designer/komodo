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

import java.util.Collection;

import org.komodo.spi.runtime.version.ITeiidServerVersion;

/**
 *
 */
public interface ITeiidServerManager extends EventManager {

    /**
     * State of the server manager
     */
    public enum RuntimeState {
        /**
         * State when the instance is first constructed
         */
        INVALID,

        /**
         * State when the instance is fully restored and ready to be used
         */
        STARTED,

        /**
         * State when the instance is restoring server configurations
         */
        RESTORING,

        /**
         * State when the instance is in the process of shutting down
         */
        SHUTTING_DOWN,

        /**
         * State when the instance has fully shutdown
         */
        SHUTDOWN
    }

    /**
     * Default teiid instance version property id
     */
    String DEFAULT_TEIID_SERVER_VERSION_ID = "defaultTeiidServerVersion"; //$NON-NLS-1$

    /**
     * Extension Point Element ID
     */
    String TEIID_SERVER_MANAGER_ELEMENT_ID = "serverManager"; //$NON-NLS-1$

    /**
     * Registers the specified <code>PersistedServer</code>.
     * 
     * @param teiidServer the server being added (never <code>null</code>)
     * @return a true if the instance was added to the registry
     */
    boolean addServer(ITeiidServer teiidServer);

    /**
     * @return defaultServer
     */
    ITeiidServer getDefaultServer();

    /**
     * @param id the id of the server being requested (never <code>null</code> )
     * @return the requested server or <code>null</code> if not found in the registry
     */
    ITeiidServer getServer(String id);

    /**
     * @param parentServer the parent server of the requested Teiid Instance
     * @return the requested server or <code>null</code> if not found in the registry
     */
    ITeiidServer getServer(ITeiidParent parentServer);

    /**
     * @return an unmodifiable collection of registered servers (never <code>null</code>)
     */
    Collection<ITeiidServer> getServers();

    /**
     * @return the state
     */
    RuntimeState getState();

    /**
     * @return true if manager is started
     */
    boolean isStarted();

    /**
     * Get the targeted Teiid Instance version
     *
     * @return Teiid Instance version
     */
    ITeiidServerVersion getDefaultServerVersion();

    /**
     * Is this server the default
     * 
     * @param teiidServer
     * 
     * @return true if this server is the default, false otherwise.
     */
    boolean isDefaultServer(ITeiidServer teiidServer);

    /**
     * @param teiidServer the server being tested (never <code>null</code>)
     * @return <code>true</code> if the server has been registered
     */
    boolean isRegistered(ITeiidServer teiidServer);

    /**
     * @param teiidServer the server being removed (never <code>null</code>)
     * @return a status indicating if the specified instance was removed from the registry (never <code>null</code>)
     */
    boolean removeServer(ITeiidServer teiidServer);

    /**
     * @param teiidServer Sets defaultServer to the specified value. May be null.
     */
    void setDefaultServer(ITeiidServer teiidServer);

    /**
     * Try and restore the manager's prior state
     */
    void restoreState();

    /**
     * Disposes of this manager.
     *
     * @throws Exception 
     */
    void dispose() throws Exception;

    /**
     * Add a listener to be notified in the event the default teiid instance
     * version is changed
     * 
     * @param listener
     */
    void addTeiidServerVersionListener(ITeiidServerVersionListener listener);

    /**
     * Remove a listener no longer interested in listening
     * to changes is server version
     * 
     * @param listener
     */
    void removeTeiidServerVersionListener(ITeiidServerVersionListener listener);
}
