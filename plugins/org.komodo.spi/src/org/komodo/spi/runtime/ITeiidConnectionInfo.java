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

import org.komodo.spi.outcome.IOutcome;

/**
 *
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
