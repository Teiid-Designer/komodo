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
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.ExecutionAdmin;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.admin.TCExecutionAdmin;

/**
 *
 */
public class TCTeiidInstance implements TeiidInstance, StringConstants {

    private TeiidParent parent;

    private TeiidAdminInfo adminInfo;

    private TeiidJdbcInfo jdbcInfo;

    private TeiidVersionProbe probe;

    private ITeiidVersion version;

    private ExecutionAdmin admin;

    private String customLabel;

    private String connectionError;

    private class TeiidAdminInfoImpl implements TeiidAdminInfo {

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
    public TCTeiidInstance(TeiidParent parent, TeiidJdbcInfo jdbcInfo) {
        initParent(parent);
        this.jdbcInfo = jdbcInfo;
    }

    private void initParent(TeiidParent parent) {
        ArgCheck.isNotNull(parent);
        this.parent = parent;
        parent.setTeiidInstance(this);
        this.adminInfo = new TeiidAdminInfoImpl();
    }

    private TeiidVersionProbe getProbe() {
        if (probe == null)
            probe = new TeiidVersionProbe(parent);

        return probe;
    }

    @Override
    public TeiidParent getParent() {
        return parent;
    }

    @Override
    public TeiidAdminInfo getTeiidAdminInfo() {
        return adminInfo;
    }

    @Override
    public TeiidJdbcInfo getTeiidJdbcInfo() {
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
        if (!isParentConnected() || this.admin == null) {
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
        if (!isParentConnected()) {
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
                this.admin = new TCExecutionAdmin(this);

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
    public void reconnect() {
        try {
            // Call disconnect() first to clear out Server & admin caches
            getEventManager().permitListeners(false);
            try {
                disconnect();
            } catch (Exception ex) {
                throw ex;
            } finally {
                getEventManager().permitListeners(true);
            }

            if (isParentConnected()) {
                // Refresh is implied in the getting of the admin object since it will
                // automatically load and refresh.
                connect();
            } else {
                throw new Exception(Messages.getString(Messages.TeiidInstance.parentNotStartedMessage));
            }

            setConnectionError(null);
        } catch (Exception e) {
            KLog.getLogger().error(EMPTY_STRING, e);
            String msg = Messages.getString(Messages.TeiidInstance.reconnectErrorMsg, this) + "\n" + e.getLocalizedMessage(); //$NON-NLS-1$
            setConnectionError(msg);
        }
    }

    @Override
    public Outcome ping(ConnectivityType connectivityType) {
        try {
            boolean testCausesConnect = false;

            if (admin == null) {
                connect();
                testCausesConnect = true;
            }

            Outcome outcome = admin.ping(connectivityType);

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
        connect();
        return admin.dataSourceExists(name);
    }

    @Override
    public void deleteDataSource(String dsName) throws Exception {
        connect();
        admin.deleteDataSource(dsName);
    }

    @Override
    public TeiidDataSource getDataSource(String name) throws Exception {
        connect();
        return admin.getDataSource(name);
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() throws Exception {
        connect();
        return admin.getDataSources();
    }

    @Override
    public Set<String> getDataSourceTypeNames() throws Exception {
        connect();
        return admin.getDataSourceTypeNames();
    }

    @Override
    public TeiidDataSource getOrCreateDataSource(String displayName, String dsName, String typeName, Properties properties) throws Exception {
        connect();
        ArgCheck.isNotNull(displayName, "displayName"); //$NON-NLS-1$
        ArgCheck.isNotNull(dsName, "dsName"); //$NON-NLS-1$
        ArgCheck.isNotNull(typeName, "typeName"); //$NON-NLS-1$
        ArgCheck.isNotNull(properties, "properties"); //$NON-NLS-1$

        for (Entry<Object, Object> entry : properties.entrySet()) {
            Object value = entry.getValue();
            String errorMsg = "No value for the connection property '" + entry.getKey() + "'"; //$NON-NLS-1$ //$NON-NLS-2$
            ArgCheck.isNotNull(value, errorMsg);
            ArgCheck.isNotEmpty(value.toString(), errorMsg);
        }

        return admin.getOrCreateDataSource(displayName, dsName, typeName, properties);
    }

    private String getVdbDataSourceConnectionUrl(String vdbName) {
        String host = this.adminInfo.getHostProvider().getHost();
        int port = this.adminInfo.getPort();
        return "jdbc:teiid:" + vdbName + "@mm://" + host + ':' + port; //$NON-NLS-1$ //$NON-NLS-2$
    }

    @Override
    public Outcome createVdbDataSource(String vdbName, String displayName, String jndiName) {
        Properties props = new Properties();
        String username = this.jdbcInfo.getUsername();
        String password = this.jdbcInfo.getPassword();
        if (username != null) {
            props.put("user-name", username); //$NON-NLS-1$
        }
        if (password != null) {
            props.put("password", password); //$NON-NLS-1$
        }

        props.put("driver-class", "org.teiid.jdbc.TeiidDriver"); //$NON-NLS-1$ //$NON-NLS-2$
        props.put("connection-url", getVdbDataSourceConnectionUrl(vdbName)); //$NON-NLS-1$

        try {
            connect();
            admin.getOrCreateDataSource(displayName, jndiName, "connector-jdbc", props); //$NON-NLS-1$
        } catch (Exception ex) {
            String msg = "Error creating data source for VDB " + vdbName; //$NON-NLS-1$
            return OutcomeFactory.getInstance().createError(msg, ex);
        }

        return OutcomeFactory.getInstance().createOK();
    }

    @Override
    public TeiidTranslator getTranslator(String name) throws Exception {
        connect();
        return admin.getTranslator(name);
    }

    @Override
    public Collection<TeiidTranslator> getTranslators() throws Exception {
        connect();
        return admin.getTranslators();
    }

    @Override
    public Collection<TeiidVdb> getVdbs() throws Exception {
        connect();
        return admin.getVdbs();
    }

    @Override
    public TeiidVdb getVdb(String name) throws Exception {
        connect();
        return admin.getVdb(name);
    }

    @Override
    public boolean hasVdb(String name) throws Exception {
        connect();
        return admin.hasVdb(name);
    }

    @Override
    public boolean isVdbActive(String vdbName) throws Exception {
        connect();
        return admin.isVdbActive(vdbName);
    }

    @Override
    public boolean isVdbLoading(String vdbName) throws Exception {
        connect();
        return admin.isVdbLoading(vdbName);
    }

    @Override
    public boolean hasVdbFailed(String vdbName) throws Exception {
        connect();
        return admin.hasVdbFailed(vdbName);
    }

    @Override
    public boolean wasVdbRemoved(String vdbName) throws Exception {
        connect();
        return admin.wasVdbRemoved(vdbName);
    }

    @Override
    public List<String> retrieveVdbValidityErrors(String vdbName) throws Exception {
        connect();
        return admin.retrieveVdbValidityErrors(vdbName);
    }

    @Override
    public void undeployVdb(String vdbName) throws Exception {
        connect();
        admin.undeployVdb(vdbName);
    }

    @Override
    public String getAdminDriverPath() throws Exception {
        connect();
        return admin.getAdminDriverPath();
    }

    @Override
    public Driver getTeiidDriver(String driverClass) throws Exception {
        connect();
        return admin.getTeiidDriver(driverClass);
    }

    @Override
    public void deployDynamicVdb(String deploymentName, InputStream inStream) throws Exception {
        connect();
        admin.deployDynamicVdb(deploymentName, inStream);
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws Exception {
        connect();
        admin.undeployDynamicVdb(vdbName);
    }

    @Override
    public void deployDriver(File driverFile) throws Exception {
        connect();
        admin.deployDriver(driverFile);
    }

    @Override
    public String getSchema(String vdbName, int vdbVersion, String modelName) throws Exception {
        connect();
        return admin.getSchema(vdbName, vdbVersion, modelName);
    }

    @Override
    public Properties getDataSourceProperties(String name) throws Exception {
        connect();
        return admin.getDataSourceProperties(name);
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws Exception {
        connect();
        return admin.getDataSourceTemplateNames();
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws Exception {
        connect();
        return admin.getTemplatePropertyDefns(templateName);
    }

    @Override
    public String getCustomLabel() {
        return this.customLabel;
    }

    @Override
    public void setCustomLabel(String customLabel) {
        this.customLabel = customLabel;
    }

    @Override
    public String getDisplayName() {
        return getCustomLabel() != null ? getCustomLabel() : getUrl();
    }

    @Override
    public String getConnectionError() {
        return this.connectionError;
    }

    private void setConnectionError(String connectionError) {
        this.connectionError = connectionError;
    }

    @Override
    public void update(TeiidInstance otherInstance) {
        ArgCheck.isNotNull(otherInstance);

        disconnect();
        connectionError = null;
        probe = null;
        version = null;
        setCustomLabel(otherInstance.getCustomLabel());
        initParent(otherInstance.getParent());

        TeiidJdbcInfo otherJdbcInfo = otherInstance.getTeiidJdbcInfo();
        jdbcInfo.setHostProvider(otherJdbcInfo.getHostProvider());
        jdbcInfo.setPassword(otherJdbcInfo.getPassword());
        jdbcInfo.setUsername(otherJdbcInfo.getUsername());
        jdbcInfo.setPort(otherJdbcInfo.getPort());
        jdbcInfo.setSecure(otherJdbcInfo.isSecure());
    }

}
