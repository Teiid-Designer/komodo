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
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
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
public class TeiidInstance implements ITeiidInstance {

    private final ITeiidParent parent;

    private final EventManager eventManager;

    private final TeiidVersionProbe probe;
    
    private ITeiidVersion version;

    private IExecutionAdmin admin;

    /**
     * Construct instance
     *
     * @param parent
     */
    public TeiidInstance(ITeiidParent parent) {
        ArgCheck.isNotNull(parent);
        this.parent = parent;
        this.parent.setTeiidInstance(this);
        this.eventManager = parent.getEventManager();
        probe = new TeiidVersionProbe(parent);
    }

    @Override
    public ITeiidParent getParent() {
        return parent;
    }

    @Override
    public String getParentName() {
        return parent.getName();
    }

    @Override
    public ITeiidVersion getVersion() throws Exception {
        if (version == null) {
            try {
                this.version = probe.getVersion();
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
        return probe.isParentConnected();
    }

    @Override
    public EventManager getEventManager() {
        return eventManager;
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
    public boolean dataSourceExists(String name) throws Exception {
        return false;
    }

    @Override
    public void deleteDataSource(String dsName) throws Exception {
    }

    @Override
    public ITeiidDataSource getDataSource(String name) throws Exception {
        return null;
    }

    @Override
    public Collection<ITeiidDataSource> getDataSources() throws Exception {
        return null;
    }

    @Override
    public Set<String> getDataSourceTypeNames() throws Exception {
        return null;
    }

    @Override
    public ITeiidDataSource getOrCreateDataSource(String displayName, String dsName, String typeName, Properties properties)
        throws Exception {
        return null;
    }

    @Override
    public ITeiidTranslator getTranslator(String name) throws Exception {
        return null;
    }

    @Override
    public Collection<ITeiidTranslator> getTranslators() throws Exception {
        return null;
    }

    @Override
    public Collection<ITeiidVdb> getVdbs() throws Exception {
        return null;
    }

    @Override
    public ITeiidVdb getVdb(String name) throws Exception {
        return null;
    }

    @Override
    public boolean hasVdb(String name) throws Exception {
        return false;
    }

    @Override
    public boolean isVdbActive(String vdbName) throws Exception {
        return false;
    }

    @Override
    public boolean isVdbLoading(String vdbName) throws Exception {
        return false;
    }

    @Override
    public boolean hasVdbFailed(String vdbName) throws Exception {
        return false;
    }

    @Override
    public boolean wasVdbRemoved(String vdbName) throws Exception {
        return false;
    }

    @Override
    public List<String> retrieveVdbValidityErrors(String vdbName) throws Exception {
        return null;
    }

    @Override
    public void undeployVdb(String vdbName) throws Exception {
    }

    @Override
    public IOutcome ping(PingType pingType) throws Exception {
        return null;
    }

    @Override
    public String getAdminDriverPath() throws Exception {
        return null;
    }

    @Override
    public Driver getTeiidDriver(String driverClass) throws Exception {
        return null;
    }

    @Override
    public void deployDynamicVdb(String deploymentName, InputStream inStream) throws Exception {
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws Exception {
    }

    @Override
    public void deployDriver(File driverFile) throws Exception {
    }

    @Override
    public String getSchema(String vdbName, int vdbVersion, String modelName) throws Exception {
        return null;
    }

    @Override
    public Properties getDataSourceProperties(String name) throws Exception {
        return null;
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws Exception {
        return null;
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws Exception {
        return null;
    }

    @Override
    public String getHost() {
        return null;
    }

    @Override
    public void reconnect() {
    }

    @Override
    public ITeiidAdminInfo getTeiidAdminInfo() {
        return null;
    }

    @Override
    public ITeiidJdbcInfo getTeiidJdbcInfo() {
        return null;
    }

    @Override
    public String getDisplayName() {
        return null;
    }

    @Override
    public String getUrl() {
        return null;
    }

    @Override
    public String getId() {
        return null;
    }

    @Override
    public boolean isConnected() {
        return false;
    }

    @Override
    public IOutcome ping() {
        return null;
    }

    @Override
    public String getCustomLabel() {
        return null;
    }

    @Override
    public String getConnectionError() {
        return null;
    }

    @Override
    public void setCustomLabel(String customLabel) {
    }

    @Override
    public IOutcome testPing() {
        return null;
    }

    @Override
    public IOutcome testJDBCPing(String host, String port, String username, String password) {
        return null;
    }

    @Override
    public IOutcome createVdbDataSource(String vdbName, String displayName, String jndiName) {
        return null;
    }

    @Override
    public void update(ITeiidInstance otherInstance) {
    }

}
