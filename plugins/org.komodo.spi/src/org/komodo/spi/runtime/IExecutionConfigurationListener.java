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
 * The <code>IExecutionConfigurationListener</code> interface defines the API for a teiid instance registry listener.
 *
 *
 */
public interface IExecutionConfigurationListener {

    /**
     * @param event the event being processed (never <code>null</code>)
     */
    void configurationChanged( ExecutionConfigurationEvent event );

}
