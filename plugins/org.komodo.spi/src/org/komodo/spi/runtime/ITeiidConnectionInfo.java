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

import org.komodo.spi.outcome.IOutcome;

/**
 * @since 8.0
 */
public interface ITeiidConnectionInfo {

    /**
     * Protocol address prefix for secure teiid instance connections
     */
    public static final String MMS = "mms://"; //$NON-NLS-1$
    /**
     * Protocol address prefix for teiid instance connections
     */
    public static final String MM = "mm://"; //$NON-NLS-1$

    /**
     * @return the password (can be <code>null</code> or empty)
     */
    String getPassword();

    /**
     * @return the port (can be <code>null</code> or empty)
     */
    String getPort();

    /**
     * @return the port number
     */

    int getPortNumber();

    /**
     * @return the connection type (never <code>null</code>)
     */
    String getType();
    
    /**
     * @return the host provider (never <code>null</code>)
     */
    HostProvider getHostProvider();

    /**
     * @return the URL (never <code>null</code>)
     */
    String getUrl();

    /**
     * @return the user name (can be <code>null</code> or empty)
     */
    String getUsername();

    /**
     * @return <code>true</code> if a secure connection protocol is being used
     */
    boolean isSecure();

    /**
     * The port, password, user name, persisting password, secure protocol, and host provider are set.
     * 
     * @param info the connection properties whose values are being used to update state
     */
    void setAll(ITeiidConnectionInfo info);

    /**
     * @param hostProvider the new value for host provider (never <code>null</code>)
     * @throws IllegalArgumentException if hostProvider is <code>null</code>
     */
    void setHostProvider(HostProvider hostProvider);

    /**
     * @param password the new value for password (can be empty or <code>null</code>)
     */
    void setPassword(String password);

    /**
     * @param port the new value for port (never empty or <code>null</code>)
     * @see #validate()
     */
    void setPort(String port);

    /**
     * @param secure the new value for if a secure connection protocol should be used
     */
    void setSecure(boolean secure);

    /**
     * @param username the new value for user name
     * @see #validate()
     */
    void setUsername(String username);

    /**
     * @return a status indicating if the connection info is in a validate state (never <code>null</code>)
     */
    IOutcome validate();

}
