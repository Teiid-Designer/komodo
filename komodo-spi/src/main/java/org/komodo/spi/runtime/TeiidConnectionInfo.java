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

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;


/**
 *
 */
public interface TeiidConnectionInfo {

    /**
     * Teiid JDBC protocol interface
     */
    String JDBC_PROTOCOL_PREFIX = "jdbc:teiid:"; //$NON-NLS-1$

    /**
     * Protocol address prefix for secure teiid instance connections
     */
    String MMS = "mms://"; //$NON-NLS-1$

    /**
     * Protocol address prefix for teiid instance connections
     */
    String MM = "mm://"; //$NON-NLS-1$

    /**
     * Generic VDB Name used in teiid jdbc urls
     */
    String VDB_GENERIC_NAME = "<vdbname>";

    public static class UrlConstructor implements StringConstants {

        /**
         * @param vdbName
         * @param secure
         * @param host
         * @param port
         * @return standard Teiid connection url with the given values
         */
        public static String createTeiidConnectionUrl(String vdbName, boolean secure, String host, int port) {
            if (vdbName == null)
                vdbName = VDB_GENERIC_NAME;

            String protocol = MM;
            if (secure)
                protocol = MMS;

            if (host == null)
                host = HostProvider.DEFAULT_HOST;

            if (port == -1)
                port = TeiidJdbcInfo.DEFAULT_PORT;

            StringBuffer buf = new StringBuffer(TeiidConnectionInfo.JDBC_PROTOCOL_PREFIX)
                                                .append(vdbName)
                                                .append(AT)
                                                .append(protocol)
                                                .append(host)
                                                .append(COLON)
                                                .append(port);

            return buf.toString();
        }

        /**
         * @return the default teiid connection url
         */
        public static String createDefaultTeiidConnectionUrl() {
            return createTeiidConnectionUrl(null, false, null, -1);
        }
    }

    /**
     * @return the password (can be <code>null</code> or empty)
     */
    String getPassword();

    /**
     * @return the port number
     */
    int getPort();

    /**
     * @return the connection type (never <code>null</code>)
     */
    ConnectivityType getType();
    
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
    void setPort(int port);

    /**
     * @param secure the new value for if a secure connection protocol should be used
     */
    void setSecure(boolean secure);

    /**
     * @param username the new value for user name
     * @see #validate()
     */
    void setUsername(String username);

}
