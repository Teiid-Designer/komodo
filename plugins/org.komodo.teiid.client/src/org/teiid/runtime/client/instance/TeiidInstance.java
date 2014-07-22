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

import java.io.File;
import java.io.InputStream;
import java.sql.Driver;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.IExecutionAdmin;
import org.komodo.spi.runtime.ITeiidAdminInfo;
import org.komodo.spi.runtime.ITeiidDataSource;
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.ITeiidJdbcInfo;
import org.komodo.spi.runtime.ITeiidParent;
import org.komodo.spi.runtime.ITeiidTranslator;
import org.komodo.spi.runtime.ITeiidVdb;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.utils.ArgCheck;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.admin.ExecutionAdmin;

/**
 *
 */
public class TeiidInstance implements ITeiidInstance, StringConstants {

    private final ITeiidParent parent;

    private final ITeiidAdminInfo adminInfo;

    private final ITeiidJdbcInfo jdbcInfo;

    private TeiidVersionProbe probe;
    
    private ITeiidVersion version;

    private IExecutionAdmin admin;

    private class TeiidAdminInfo implements ITeiidAdminInfo {

        @Override
        public HostProvider getHostProvider() {
            return parent;
        }

        @Override
        public String getUsername() {
            return parent.getUserName();
        }

        @Override
        public String getPassword() {
            return parent.getPassword();
        }

        @Override
        public int getPort() {
            return parent.getPort();
        }

        @Override
        public boolean isSecure() {
            return parent.isSecure();
        }

        @Override
        public ConnectivityType getType() {
            return ConnectivityType.ADMIN;
        }

        /**
         * mm<s>://host:port
         */
        @Override
        public String getUrl() {
            StringBuilder sb = new StringBuilder();
            sb.append(isSecure() ? MMS : MM);
            sb.append(getHostProvider().getHost());
            sb.append(':');
            sb.append(getPort());

            return sb.toString();
        }

        @Override
        public void setHostProvider(HostProvider hostProvider) {
            // Not required since all data depends on parent
        }

        @Override
        public void setPassword(String password) {
            // Not required since all data depends on parent
        }

        @Override
        public void setPort(int port) {
            // Not required since all data depends on parent
        }

        @Override
        public void setSecure(boolean secure) {
            // Not required since all data depends on parent
        }

        @Override
        public void setUsername(String username) {
            // Not required since all data depends on parent
        }
    }

    /**
     * Construct instance
     *
     * @param parent
     */
    public TeiidInstance(ITeiidParent parent, ITeiidJdbcInfo jdbcInfo) {
        ArgCheck.isNotNull(parent);
        this.parent = parent;
        parent.setTeiidInstance(this);
        this.adminInfo = new TeiidAdminInfo();
        this.jdbcInfo = jdbcInfo;
    }

    private TeiidVersionProbe getProbe() {
        if (probe == null)
            probe = new TeiidVersionProbe(parent);

        return probe;
    }

    @Override
    public ITeiidParent getParent() {
        return parent;
    }

    @Override
    public ITeiidAdminInfo getTeiidAdminInfo() {
        return adminInfo;
    }

    @Override
    public ITeiidJdbcInfo getTeiidJdbcInfo() {
        return jdbcInfo;
    }

    @Override
    public String getParentName() {
        return parent.getName();
    }

    @Override
    public ITeiidVersion getVersion() throws Exception {
        if (version == null) {
            try {
                this.version = getProbe().getVersion();
            } catch (Exception ex) {
                // Ensure this instance has a version if host is not available
                this.version = TeiidVersion.Version.DEFAULT_TEIID_VERSION.get();

                throw new Exception(Messages.getString(Messages.TeiidInstance.versionFailure, getHost()), ex);
            }
        }

        return version;
    }

    @Override
    public boolean isParentConnected() {
        return getProbe().isParentConnected();
    }

    /**
     * @return <code>true</code> if a connection to this server exists and is working
     */
    @Override
    public boolean isConnected() {
        if (! isParentConnected() || this.admin == null) {
            return false;
        }
        return ping(ConnectivityType.ADMIN).isOK();
    }

    @Override
    public EventManager getEventManager() {
        return parent.getEventManager();
    }

    @Override
    public String getHost() {
        return parent.getHost();
    }

    @Override
    public String getId() {
        return getUrl() + HYPHEN + getParent().getId();
    }

    @Override
    public String getUrl() {
        return getTeiidAdminInfo().getUrl();
    }

    @Override
    public void notifyRefresh() {
        getEventManager().notifyListeners(ExecutionConfigurationEvent.createTeiidRefreshEvent(this));
    }

    @Override
    public void connect() throws Exception {
        if (! isParentConnected()) {
            throw new Exception(Messages.getString(Messages.TeiidInstance.parentNotStartedMessage, getHost()));
        }

        if (this.admin == null) {
            try {
                /*
                * The version should be determined prior to getting an admin
                * instance and 'connect' (which is in fact nothing but a refresh),
                * since both stash the version and use it during init and refresh
                * of translators etc...
                *
                * Remove any default version that might have been set prior to the host being up
                */
                this.version = null;
                getVersion();

                /*
                 * By the time this has been called the teiid version should be correct
                 * for the given host and the host should be up, otherwise admin will
                 * end up back as null anyway.
                 */
                this.admin = new ExecutionAdmin(this);

                if (admin != null) {
                    /*
                     * Avoid the refresh listener being fired prematurely by the admin client.
                     * Want to fire the refresh ourselves using {#notifyRefresh} at the end
                     * of this function.
                     */
                    getEventManager().permitListeners(false);

                    this.admin.connect();
                }
            } catch (Exception ex) {
                throw ex;
            } finally {
                getEventManager().permitListeners(true);
            }

            getEventManager().notifyListeners(ExecutionConfigurationEvent.createTeiidConnectedEvent(this));
            notifyRefresh();
        }
    }

    @Override
    public void disconnect() {
        if (this.admin != null) {
            this.admin.disconnect();
            this.admin = null;
        }
        
        notifyRefresh();
    }

    @Override
    public IOutcome ping(ConnectivityType connectivityType) {
        try {
            boolean testCausesConnect = false;

            if (admin == null) {
                connect();
                testCausesConnect = true;
            }

            IOutcome outcome = admin.ping(connectivityType);

            // Only disconnect if this test ping caused
            // the connect
            if (testCausesConnect) {
                disconnect();
            }

            return outcome;

        } catch (Exception ex) {
            String msg = Messages.getString(Messages.ExecutionAdmin.cannotConnectToServer, getUrl());
            return OutcomeFactory.getInstance().createError(msg, ex);
        }
    }

    @Override
    public boolean dataSourceExists(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deleteDataSource(String dsName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITeiidDataSource getDataSource(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<ITeiidDataSource> getDataSources() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<String> getDataSourceTypeNames() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITeiidDataSource getOrCreateDataSource(String displayName, String dsName, String typeName, Properties properties)
        throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITeiidTranslator getTranslator(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<ITeiidTranslator> getTranslators() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<ITeiidVdb> getVdbs() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITeiidVdb getVdb(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean hasVdb(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isVdbActive(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isVdbLoading(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean hasVdbFailed(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean wasVdbRemoved(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<String> retrieveVdbValidityErrors(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void undeployVdb(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getAdminDriverPath() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Driver getTeiidDriver(String driverClass) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deployDynamicVdb(String deploymentName, InputStream inStream) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deployDriver(File driverFile) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSchema(String vdbName, int vdbVersion, String modelName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Properties getDataSourceProperties(String name) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void reconnect() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getDisplayName() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getCustomLabel() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getConnectionError() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setCustomLabel(String customLabel) {
        throw new UnsupportedOperationException();
    }

    @Override
    public IOutcome createVdbDataSource(String vdbName, String displayName, String jndiName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void update(ITeiidInstance otherInstance) {
        throw new UnsupportedOperationException();
    }

}
