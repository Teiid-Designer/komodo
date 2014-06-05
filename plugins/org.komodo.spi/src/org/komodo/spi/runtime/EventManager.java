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


/**
 *
 *
 * @since 8.0
 */
public interface EventManager {

    /**
     * Listeners already registered will not be added again. The new listener will receive events for all existing teiid instances.
     * 
     * @param listener the listener being register to receive events (never <code>null</code>)
     * @return <code>true</code> if listener was added
     */
    boolean addListener( IExecutionConfigurationListener listener );

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
    boolean removeListener( IExecutionConfigurationListener listener );
    
    /**
     * An <code>EventManager</code> that does not do anything.
     */
    EventManager EVENT_MANAGER_ADAPTER = new EventManager() {

        @Override
        public boolean addListener( IExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void notifyListeners( ExecutionConfigurationEvent event ) {
            // nothing to do
        }

        @Override
        public boolean removeListener( IExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void permitListeners(boolean enable) {
        }
        
    };

}
