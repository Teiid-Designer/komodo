/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.teiid.runtime.client.instance;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.utils.ArgCheck;

/**
 * The <code>TeiidJdbcInfo</code> defines the properties needed to make a Teiid JDBC connection.
 */
public class TCTeiidJdbcInfo implements TeiidJdbcInfo, StringConstants {

    /**
     * The name of the VDB that this connection will connect to (never empty or <code>null</code>)
     */
    private String vdbname;

    /**
     * Host of the connection
     */
    private HostProvider host;

    /**
     * Port of the connection
     */
    private int port;

    /**
     * Username required for connection
     */
    private String username;

    /**
     * Password required for connection
     */
    private String password;

    /**
     * Is the connection encrypted, ie. using the MMS protocol rather than MM
     */
    private boolean secure;


    /**
     * Create default teiid jdbc info
     */
    public TCTeiidJdbcInfo() {
        this(DEFAULT_PORT, DEFAULT_JDBC_USERNAME, DEFAULT_JDBC_PASSWORD, false);
        setHostProvider(HostProvider.DEFAULT_HOST_PROVIDER);
    }

    /**
     * @param port the connection port (can be <code>null</code> or empty)
     * @param username the connection user name (can be <code>null</code> or empty)
     * @param password the connection password (can be <code>null</code> or empty)
     * @param secure <code>true</code> if a secure connection should be used
     */
    public TCTeiidJdbcInfo(int port, String username, String password, boolean secure) {
        this(VDB_PLACEHOLDER, port, username, password, secure);
    }

    /**
     * @param vdbname the VDB name (never empty or <code>null</code>)
     * @param port the connection port (can be <code>null</code> or empty)
     * @param username the connection user name (can be <code>null</code> or empty)
     * @param password the connection password (can be <code>null</code> or empty)
     * @param secure <code>true</code> if a secure connection should be used
     */
    private TCTeiidJdbcInfo(String vdbname, int port, String username, String password, boolean secure) {
        ArgCheck.isNotEmpty(vdbname, "vdbname"); //$NON-NLS-1$
        this.vdbname = vdbname;
        this.port = port;
        this.username = username;
        this.password = password;
        this.secure = secure;
    }

    /**
     * @param vdbname the VDB name (may not be empty or <code>null</code>)
     * @param teiidJdbcInfo the connection properties whose values are being used to construct this object
     * @throws IllegalArgumentException if vdbname is empty or <code>null</code>
     */
    public TCTeiidJdbcInfo(String vdbname, TeiidJdbcInfo teiidJdbcInfo) {
        this(vdbname, teiidJdbcInfo.getPort(), teiidJdbcInfo.getUsername(), teiidJdbcInfo.getPassword(), teiidJdbcInfo.isSecure());
        setHostProvider(teiidJdbcInfo.getHostProvider());
    }

    @Override
    public String getPassword() {
        return password;
    }

    @Override
    public int getPort() {
        return port;
    }

    @Override
    public HostProvider getHostProvider() {
        return host;
    }

    @Override
    public String getUsername() {
        return username;
    }

    @Override
    public boolean isSecure() {
        return secure;
    }

    @Override
    public void setHostProvider(HostProvider hostProvider) {
        this.host = hostProvider;
    }

    @Override
    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public void setPort(int port) {
        this.port = port;
    }

    @Override
    public void setSecure(boolean secure) {
        this.secure = secure;
    }

    @Override
    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public TeiidJdbcInfo clone() {
        TCTeiidJdbcInfo cloned = new TCTeiidJdbcInfo(getPort(), getUsername(), getPassword(), isSecure());
        cloned.setHostProvider(getHostProvider());
        return cloned;
    }

    @Override
    public ConnectivityType getType() {
        return ConnectivityType.JDBC;
    }

    /**
     * jdbc:teiid:<vdbname>@mm<s>://host:port
     */
    @Override
    public String getUrl(String vdbName) {
        StringBuilder sb = new StringBuilder();
        sb.append(JDBC_TEIID_PREFIX);
        sb.append(vdbName);
        sb.append('@');
        sb.append(isSecure() ? MMS : MM);
        sb.append(getHostProvider().getHost());
        sb.append(':');
        sb.append(getPort());

        return sb.toString();
    }

    @Override
    public String getUrl() {
        return getUrl(this.vdbname);
    }

}
