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
package org.komodo.shell;

import org.komodo.shell.api.IShellTeiidParent;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidParent;

/**
 * Implementation of {@link TeiidParent} for use by the shell framework
 */
public class ShellTeiidParent implements TeiidParent, IShellTeiidParent {

    private String host = HostProvider.DEFAULT_HOST;

    private int port = TeiidAdminInfo.DEFAULT_PORT;

    private String userName = TeiidAdminInfo.DEFAULT_ADMIN_USERNAME;

    private String password = TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD;

    private boolean secure = false;

    private TeiidInstance teiidInstance;

    private EventManager shellEventManager = new ShellEventManager();

    @Override
    public Object getParentObject() {
        return this;
    }

    @Override
    public String getId() {
        return null;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public TeiidInstance getTeiidInstance() {
        return teiidInstance;
    }

    @Override
    public void setTeiidInstance(TeiidInstance teiidInstance) {
        this.teiidInstance = teiidInstance;
    }

    @Override
    public EventManager getEventManager() {
        return shellEventManager ;
    }

    /**
     * @return the host
     */
    @Override
    public String getHost() {
        return this.host;
    }

    /**
     * @param host the host to set
     */
    @Override
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * @return the port
     */
    @Override
    public int getPort() {
        return this.port;
    }

    /**
     * @param port the port to set
     */
    @Override
    public void setPort(int port) {
        this.port = port;
    }

    /**
     * @return the username
     */
    @Override
    public String getUserName() {
        return this.userName;
    }

    /**
     * @param userName the username to set
     */
    @Override
    public void setUserName(String userName) {
        this.userName = userName;
    }

    /**
     * @return the password
     */
    @Override
    public String getPassword() {
        return this.password;
    }

    /**
     * @param password the password to set
     */
    @Override
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * @return is secure
     */
    @Override
    public boolean isSecure() {
        return this.secure;
    }

    /**
     * @param secure secure status
     */
    @Override
    public void setSecure(boolean secure) {
        this.secure = secure;
    }

}
