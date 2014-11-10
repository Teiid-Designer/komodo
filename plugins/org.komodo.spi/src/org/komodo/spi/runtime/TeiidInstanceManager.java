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

import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
public interface TeiidInstanceManager extends EventManager {

    /**
     * State of the teiid instance manager
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
         * State when the instance is restoring teiid instance configurations
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
    String DEFAULT_TEIID_INSTANCE_VERSION_ID = "defaultTeiidInstanceVersion"; //$NON-NLS-1$

    /**
     * Extension Point Element ID
     */
    String TEIID_INSTANCE_MANAGER_ELEMENT_ID = "instanceManager"; //$NON-NLS-1$

    /**
     * Registers the specified <code>PersistedInstance</code>.
     * 
     * @param teiidInstance the teiid instance being added (never <code>null</code>)
     * @return a true if the instance was added to the registry
     */
    boolean addTeiidInstance(TeiidInstance teiidInstance);

    /**
     * @return defaultInstance
     */
    TeiidInstance getDefaultTeiidInstance();

    /**
     * @param id the id of the teiid instance being requested (never <code>null</code> )
     * @return the requested teiid instance or <code>null</code> if not found in the registry
     */
    TeiidInstance getTeiidInstance(String id);

    /**
     * @param teiidParent the parent of the requested Teiid Instance
     * @return the requested teiid parent or <code>null</code> if not found in the registry
     */
    TeiidInstance getTeiidInstance(TeiidParent teiidParent);

    /**
     * @return an unmodifiable collection of registered instances (never <code>null</code>)
     */
    Collection<TeiidInstance> getInstances();

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
    TeiidVersion getDefaultVersion();

    /**
     * Is this teiid instance the default
     * 
     * @param teiidInstance
     * 
     * @return true if this teiid instance is the default, false otherwise.
     */
    boolean isDefaultInstance(TeiidInstance teiidInstance);

    /**
     * @param teiidInstance the teiid instance being tested (never <code>null</code>)
     * @return <code>true</code> if the teiid instance has been registered
     */
    boolean isRegistered(TeiidInstance teiidInstance);

    /**
     * @param teiidInstance the instance being removed (never <code>null</code>)
     * @return a status indicating if the specified instance was removed from the registry (never <code>null</code>)
     */
    boolean removeTeiidInstance(TeiidInstance teiidInstance);

    /**
     * @param teiidInstance Sets default instance to the specified value. May be null.
     */
    void setDefaultInstance(TeiidInstance teiidInstance);

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
    void addTeiidInstanceVersionListener(TeiidInstanceVersionListener listener);

    /**
     * Remove a listener no longer interested in listening
     * to changes in instance version
     * 
     * @param listener
     */
    void removeTeiidInstanceVersionListener(TeiidInstanceVersionListener listener);
}
