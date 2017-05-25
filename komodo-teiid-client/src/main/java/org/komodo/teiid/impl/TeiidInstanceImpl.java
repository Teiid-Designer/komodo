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
 * 021101301 USA.
 */
package org.komodo.teiid.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.TeiidConnectionInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.teiid.AbstractConnectionManager;
import org.komodo.teiid.AbstractTeiidInstance;
import org.komodo.teiid.Messages;
import org.komodo.utils.KLog;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Translator;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.jboss.AdminFactory;
import org.teiid.jdbc.TeiidDriver;

public class TeiidInstanceImpl extends AbstractTeiidInstance {

    private Admin admin;

    private final TeiidArtifactFactory factory = new TeiidArtifactFactory();

    private final JbossExtensions ext = new JbossExtensions();

    public TeiidInstanceImpl(TeiidParent parent, final TeiidVersion teiidVersion, TeiidJdbcInfo jdbcInfo) {
        super(parent, teiidVersion, jdbcInfo);
    }

    @Override
    protected AbstractConnectionManager getConnectionManager() {
        return ConnectionManager.getInstance();
    }

    @Override
    public TeiidVersion getRuntimeVersion() throws Exception {
        return ext.getTeiidRuntimeVersion(admin);
    }

    @Override
    protected boolean isCoherent() {
        return admin != null;
    }

    /**
     * @return <code>true</code> if a connection to this server exists and is working
     */
    @Override
    public boolean isConnected() {
        if (!isParentConnected() || !isCoherent()) {
            return false;
        }
        return ping(ConnectivityType.ADMIN).isOK();
    }

    @Override
    public void connect() throws Exception {
        if (!isParentConnected()) {
            throw new Exception(Messages.getString(Messages.TeiidInstance.parentNotStartedMessage, getHost()));
        }

        synchronized(TEIID_INSTANCE_LOCK) {
            if (this.admin == null) {
                try {
                    /*
                     * By the time this has been called the teiid version should be correct
                     * for the given host and the host should be up, otherwise admin will
                     * end up back as null anyway.
                     */
                    char[] passwordArray = null;
                    if (getTeiidAdminInfo().getPassword() != null) {
                        passwordArray = getTeiidAdminInfo().getPassword().toCharArray();
                    }

                    /*
                     * Avoid the refresh listener being fired prematurely by the admin client.
                     * Want to fire the refresh ourselves using {#notifyRefresh} at the end
                     * of this function.
                     */
                    getEventManager().permitListeners(false);

                    this.admin = AdminFactory.getInstance().createAdmin(getHost(),
                                                                        getTeiidAdminInfo().getPort(),
                                                                        getTeiidAdminInfo().getUsername(),
                                                                        passwordArray);

                    if (admin == null)
                        throw new Exception("Failed to create a teiid admin connection. Maybe incorrect password?");

                } catch (Exception ex) {
                    throw ex;
                } finally {
                    getEventManager().permitListeners(true);
                }

                getEventManager().notifyListeners(ExecutionConfigurationEvent.createTeiidConnectedEvent(this));
                notifyRefresh();
            }
        }
    }

    @Override
    public void disconnect() {
        if (this.admin != null) {
            this.admin.close();
            this.admin = null;
        }

        notifyRefresh();
    }

    @Override
    protected Outcome pingAdmin() throws Exception {
        if (admin == null) {
            try {
                connect();
            } catch (Exception ex) {
                return OutcomeFactory.getInstance().createError(ex.getLocalizedMessage(), ex);
            }
        }

        admin.getSessions();
        return OutcomeFactory.getInstance().createOK();
    }

    @Override
    protected Outcome pingJdbc() {
        String host = getHost();
        TeiidJdbcInfo teiidJdbcInfo = getTeiidJdbcInfo();

        String protocol = TeiidConnectionInfo.MM;
        if (teiidJdbcInfo.isSecure())
            protocol = TeiidConnectionInfo.MMS;

        Connection teiidJdbcConnection = null;
        String url = "jdbc:teiid:ping@" + protocol + host + ':' + teiidJdbcInfo.getPort(); //$NONNLS1$

        try {

            admin.deploy(PING_VDB, new ByteArrayInputStream(TEST_VDB.getBytes()));

            try {
                String urlAndCredentials = url + ";user=" + teiidJdbcInfo.getUsername() + ";password="
                                           + teiidJdbcInfo.getPassword() + ';'; //$NONNLS1$ //$NONNLS2$
                TeiidDriver teiidDriver = TeiidDriver.getInstance();
                teiidJdbcConnection = teiidDriver.connect(urlAndCredentials, null);
                //pass
            } catch (SQLException ex) {
                String msg = Messages.getString(Messages.ExecutionAdmin.instanceDeployUndeployProblemPingingTeiidJdbc, url);
                return OutcomeFactory.getInstance().createError(msg, ex);
            } finally {
                admin.undeploy(PING_VDB);

                if (teiidJdbcConnection != null) {
                    teiidJdbcConnection.close();
                }
            }
        } catch (Exception ex) {
            String msg = Messages.getString(Messages.ExecutionAdmin.instanceDeployUndeployProblemPingingTeiidJdbc, url);
            return OutcomeFactory.getInstance().createError(msg, ex);
        }

        return OutcomeFactory.getInstance().createOK();
    }

    @Override
    public boolean dataSourceExists(String name) throws Exception {
        connect();
        return admin.getDataSourceNames().contains(name);
    }

    @Override
    public void deleteDataSource(String dsName) throws Exception {
        connect();
        admin.deleteDataSource(dsName);
    }

    @Override
    public TeiidDataSource getDataSource(String name) throws Exception {
        connect();
        Properties dataSource = admin.getDataSource(name);
        if (dataSource == null)
            return null;

        return factory.createDataSource(name, dataSource);
    }

    @Override
    public Collection<ConnectionDriver> getDataSourceDrivers() throws Exception {
        return ext.getDataSourceDrivers(admin);
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() throws Exception {
        connect();
        Collection<String> dsNames = admin.getDataSourceNames();
        if (dsNames.isEmpty())
            return Collections.emptyList();

        List<TeiidDataSource> dsSources = new ArrayList<>();
        for (String dsName : dsNames) {
            TeiidDataSource dataSource = getDataSource(dsName);
            dsSources.add(dataSource);
        }

        return dsSources;
    }

    @Override
    protected void createDataSource(String deploymentName, String templateName, Properties properties) throws Exception {
        admin.createDataSource(deploymentName, templateName, properties);
    }

    @Override
    public Set<String> getDataSourceTypeNames() throws Exception {
        connect();
        return admin.getDataSourceTemplateNames();
    }

    @Override
    public TeiidTranslator getTranslator(String name) throws Exception {
        connect();
        return factory.createTranslator(admin.getTranslator(name));
    }

    @Override
    public Collection<TeiidTranslator> getTranslators() throws Exception {
        connect();
        Collection<? extends Translator> translators = admin.getTranslators();
        if (translators.isEmpty())
            return Collections.emptyList();

        List<TeiidTranslator> teiidTranslators = new ArrayList<>();
        for (Translator translator : translators) {
            teiidTranslators.add(factory.createTranslator(translator));
        }

        return teiidTranslators;
    }

    private boolean isDynamic(VDB vdb) {
        if (vdb == null)
            return false;

        if (! (vdb instanceof VDBMetaData))
            return false;

        return ((VDBMetaData) vdb).isXmlDeployment();
    }

    @Override
    public Collection<String> getVdbNames() throws Exception {
        connect();
        Collection<? extends VDB> vdbs = admin.getVDBs();
        if (vdbs.isEmpty())
            return Collections.emptyList();

        List<String> teiidVdbNames = new ArrayList<String>();
        for (VDB vdb : vdbs) {
            teiidVdbNames.add(vdb.getName());
        }

        return teiidVdbNames;
    }

    @Override
    public Collection<TeiidVdb> getVdbs() throws Exception {
        connect();
        Collection<? extends VDB> vdbs = admin.getVDBs();
        if (vdbs.isEmpty())
            return Collections.emptyList();

        List<TeiidVdb> teiidVdbs = new ArrayList<>();
        for (VDB vdb : vdbs) {
            if (! isDynamic(vdb))
                continue;

            teiidVdbs.add(factory.createVdb(vdb));
        }

        return teiidVdbs;
    }

    @Override
    public TeiidVdb getVdb(String name) throws Exception {
        connect();

        String methodName = "getVDB";
        Method method;
        VDB vdb;
        if (getVersion().isGreaterThan(Version.TEIID_9_0)) {
            method = admin.getClass().getMethod(methodName, String.class, String.class);
            vdb = (VDB) method.invoke(admin, name, "1");
        } else {
            method = admin.getClass().getMethod(methodName, String.class, int.class);
            vdb = (VDB) method.invoke(admin, name, 1);
        }

        if (vdb == null)
            return null;

        if (! isDynamic(vdb))
            return null;

        return factory.createVdb(vdb);
    }

    @Override
    public String getAdminDriverPath() throws Exception {
        connect();
        return Admin.class.getProtectionDomain().getCodeSource().getLocation().getFile();
    }

    @Override
    public Driver getTeiidDriver(String driverClass) throws Exception {
        connect();
        return TeiidDriver.getInstance();
    }

    @Override
    protected void deploy(String name, InputStream stream) throws Exception {
        admin.deploy(name, stream);
    }

    @Override
    protected void undeploy(String name) throws Exception {
        admin.undeploy(name);
    }

    @Override
    public String getSchema(String vdbName, String vdbVersion, String modelName) throws Exception {
        connect();

        String methodName = "getSchema";
        Method method;
        if (getVersion().isGreaterThan(Version.TEIID_9_0)) {
            method = admin.getClass().getMethod(methodName,
                                                                                    String.class, String.class, String.class,
                                                                                    EnumSet.class, String.class);
            return (String) method.invoke(admin, vdbName, vdbVersion, modelName, null, null);
        } else {
            method = admin.getClass().getMethod(methodName,
                                                String.class, int.class, String.class,
                                                EnumSet.class, String.class);
            int version;
            try {
                version = (int) Double.parseDouble(vdbVersion);
            } catch (NumberFormatException ex1) {
                try {
                    version = Integer.parseInt(vdbVersion);
                } catch (Exception ex2) {
                    KLog.getLogger().error("Cannot parse vdb version: " + vdbVersion + ". Defaulting to version 1", ex2);
                    version = 1;
                }
            }

            return (String) method.invoke(admin, vdbName, version, modelName, null, null);
        }
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws Exception {
        connect();
        return admin.getDataSourceTemplateNames();
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws Exception {
        connect();
        Collection<? extends PropertyDefinition> propDefs = this.admin.getTemplatePropertyDefinitions(templateName);
        if (propDefs.isEmpty())
            return Collections.emptyList();

        List<TeiidPropertyDefinition> teiidPropDefs = new ArrayList<>();
        for (PropertyDefinition propDef : propDefs) {
            teiidPropDefs.add(factory.createPropertyDefinition(propDef));
        }

        return teiidPropDefs;
    }
}
