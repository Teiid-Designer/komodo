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
 *
 *
 */
public interface EventManager {

    /**
     * Listeners already registered will not be added again. The new listener will receive events for all existing teiid instances.
     * 
     * @param listener the listener being register to receive events (never <code>null</code>)
     * @return <code>true</code> if listener was added
     */
    boolean addListener( ExecutionConfigurationListener listener );

    /**
     * Enable / disable the listeners in the event manager
     *
     * @param enable
     */
    void permitListeners(boolean enable);

    /**
     * @param event the event the registry listeners are to process
     */
    void notifyListeners( ExecutionConfigurationEvent event );

    /**
     * @param listener the listener being unregistered and will no longer receive events (never <code>null</code>)
     * @return <code>true</code> if listener was removed
     */
    boolean removeListener( ExecutionConfigurationListener listener );
    
    /**
     * An <code>EventManager</code> that does not do anything.
     */
    EventManager EVENT_MANAGER_ADAPTER = new EventManager() {

        @Override
        public boolean addListener( ExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void notifyListeners( ExecutionConfigurationEvent event ) {
            // nothing to do
        }

        @Override
        public boolean removeListener( ExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void permitListeners(boolean enable) {
            //TODO Not yet implemented
        }
        
    };

}
