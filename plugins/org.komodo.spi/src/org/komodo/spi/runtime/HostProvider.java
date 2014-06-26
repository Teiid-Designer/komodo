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
 *
 */
public interface HostProvider {

    /**
     * The default connection host. Value is {@value}.
     */
    String DEFAULT_HOST = "localhost"; //$NON-NLS-1$
    
    /**
     * A <code>HostProvider</code> that provides the default host.
     */
    DefaultHostProvider DEFAULT_HOST_PROVIDER = new DefaultHostProvider();
    
    /**
     * @return the host (never <code>null</code>)
     */
    String getHost();
    
    /**
     * The <code>DefaultHostProvider</code> provides the default host.
     * @see HostProvider#DEFAULT_HOST
     */
    class DefaultHostProvider implements HostProvider {
        /**
         * {@inheritDoc}
         *
         * @see HostProvider#getHost()
         */
        @Override
        public String getHost() {
            return DEFAULT_HOST;
        }
    }

}
