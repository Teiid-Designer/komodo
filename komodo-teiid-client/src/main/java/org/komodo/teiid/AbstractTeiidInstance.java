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
package org.komodo.teiid;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.sql.Connection;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.teiid.core.util.ApplicationInfo;

public abstract class AbstractTeiidInstance implements TeiidInstance, StringConstants {

    protected class JndiManager implements StringConstants {

        private static final String PREFIX = JAVA + COLON + FORWARD_SLASH;

        public String getName(String name) {
            if (! name.startsWith(PREFIX))
                return name;

            name = name.replace(PREFIX, EMPTY_STRING);
            if (name.startsWith(FORWARD_SLASH))
                name = name.substring(1);

            return name;
        }
    }

    protected class TeiidAdminInfoImpl implements TeiidAdminInfo {

        @Override
        public HostProvider getHostProvider() {
            return parent;
        }

        @Override
        public String getUsername() {
            return parent.getUsername();
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

    protected static final String TEST_VDB = EMPTY_STRING + "<vdb name=\"ping\" version=\"1\">" + //$NON-NLS-1$
                                           "<model visible=\"true\" name=\"Foo\" type=\"PHYSICAL\" path=\"/dummy/Foo\">" + //$NON-NLS-1$
                                           "<source name=\"s\" translator-name=\"loopback\"/>" + //$NON-NLS-1$
                                           "<metadata type=\"DDL\"><![CDATA[CREATE FOREIGN TABLE G1 (e1 string, e2 integer);]]> </metadata>" //$NON-NLS-1$
                                           + "</model>" + "</vdb>";

    private final JndiManager manager = new JndiManager();

    private TeiidParent parent;

    private TeiidAdminInfo adminInfo;

    private TeiidJdbcInfo jdbcInfo;

    private TeiidVersion supportedVersion;

    private String connectionError;

    public AbstractTeiidInstance(TeiidParent parent, final TeiidVersion teiidVersion, TeiidJdbcInfo jdbcInfo) {
        ArgCheck.isNotNull(parent);
        ArgCheck.isNotNull(jdbcInfo);
        ArgCheck.isNotNull(parent);
        this.parent = parent;
        this.supportedVersion = teiidVersion;
        this.adminInfo = new TeiidAdminInfoImpl();
        this.jdbcInfo = jdbcInfo;
    }

    protected abstract AbstractConnectionManager getConnectionManager();

    @Override
    public boolean isSound() {
        if (getParent() == null)
            return false;

        if (getTeiidAdminInfo() == null)
            return false;

        if (getTeiidJdbcInfo() == null)
            return false;

        return getParent().isSound();
    }

    /**
     * Append the vdb file extension to the vdb name 
     * if not already appended.
     * 
     * @param vdbName
     * @return
     */
    protected String appendVdbExtension(String vdbName) {
        if (vdbName.endsWith(TeiidVdb.VDB_EXTENSION))
            return vdbName;

        return vdbName + TeiidVdb.VDB_DOT_EXTENSION;
    }

    /**
     * Append the suffix for dynamic VDB to the vdb name if not already appended.
     * 
     * @param vdbName
     * @return
     */
    protected String appendDynamicVdbSuffix(String vdbName) {
        if (vdbName.endsWith(TeiidVdb.DYNAMIC_VDB_SUFFIX))
            return vdbName;

        return vdbName + TeiidVdb.DYNAMIC_VDB_SUFFIX;
    }

    @Override
    public TeiidParent getParent() {
        return parent;
    }

    @Override
    public TeiidAdminInfo getTeiidAdminInfo() {
        return adminInfo;
    }

    protected String getVdbDataSourceConnectionUrl(String vdbName) {
        String host = this.adminInfo.getHostProvider().getHost();
        int port = this.adminInfo.getPort();
        return "jdbc:teiid:" + vdbName + "@mm://" + host + ':' + port; //$NONNLS1$ //$NONNLS2$
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
    public TeiidVersion getSupportedVersion() {
        return supportedVersion;
    }

    @Override
    public TeiidVersion getClientVersion() {
        return new DefaultTeiidVersion(ApplicationInfo.getInstance().getBuildNumber());
    }

    @Override
    public abstract TeiidVersion getRuntimeVersion() throws Exception;

    @Override
    public TeiidVersion getVersion() {
        TeiidVersion version;

        if (isConnected()) {
            try {
                version = getRuntimeVersion();
                if (version != null)
                    return version;

            } catch (Exception ex) {
                KLog.getLogger().error(ex.getMessage(), ex);
            }
        }

        return getClientVersion();
    }

    @Override
    public String getConnectionError() {
        return this.connectionError;
    }

    protected void setConnectionError(String connectionError) {
        this.connectionError = connectionError;
    }

    @Override
    public EventManager getEventManager() {
        return getParent().getEventManager();
    }

    @Override
    public String getHost() {
        return getParent().getHost();
    }

    @Override
    public String getId() {
        return getUrl() + HYPHEN + getParent().getId();
    }

    @Override
    public String getUrl() {
        return getTeiidJdbcInfo().getUrl();
    }

    @Override
    public void notifyRefresh() {
        if (getEventManager() == null)
            return;

        getEventManager().notifyListeners(ExecutionConfigurationEvent.createTeiidRefreshEvent(this));
    }

    @Override
    public boolean isParentConnected() {
        Socket socket = null;
        Reader in = null;
        InetSocketAddress endPoint = new InetSocketAddress(parent.getHost(), parent.getPort());

        if (endPoint.isUnresolved()) {
            return false;
        }

        try {
            socket = new Socket();
            socket.connect(endPoint, 1024);

            //
            // Teiid 9.1 on Wildfly 10.x uses the web port instead
            // which does not return any data so the read() hangs
            // and never returns.
            //
            if (getClientVersion().isLessThan(Version.TEIID_9_1)) {
                /*
                 * This may not seem necessary since a socket connection
                 * should be enough. However, TEIIDDES-1971 has shown
                 * that without actually using the socket, 'Connection reset
                 * by peer' messages with be reported in the server log.
                 */
                InputStream socketReader = socket.getInputStream();

                final char[] buffer = new char[100];
                in = new InputStreamReader(socketReader);
                int rsz = in.read(buffer, 0, buffer.length);
                if (rsz == -1) {
                    return false;
                }

                StringBuffer output = new StringBuffer();
                for (int i = 0; i < buffer.length; ++i) {
                    if (Character.isLetterOrDigit(buffer[i])) {
                        output.append(buffer[i]);
                    }
                }
            }

            return true;
        } catch (Exception ex) {
            return false;
        } finally {
            try {
                if (in != null)
                    in.close();

                if (socket != null && socket.isConnected()) {
                    socket.close();
                    socket = null;
                }
            } catch (Exception ex2) {
                /*
                 * Unlikely event that socket did not close correctly.
                 * Do nothing
                 */
            }
        }
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
            String msg = Messages.getString(Messages.TeiidInstance.reconnectErrorMsg, this) + "\n" + e.getLocalizedMessage(); //$NONNLS1$
            setConnectionError(msg);
        }
    }

    protected abstract Outcome pingAdmin() throws Exception;

    protected Outcome pingJdbc() {
        TeiidJdbcInfo teiidJdbcInfo = getTeiidJdbcInfo();
        Connection teiidJdbcConnection = null;

        try {

            deploy(PING_VDB, new ByteArrayInputStream(TEST_VDB.getBytes()));

            Thread.sleep(2000);

            try {
                teiidJdbcConnection = getConnectionManager().getConnection(
                                                                      PING_VDB_NAME,
                                                                      getHost(),
                                                                      teiidJdbcInfo.getPort(),
                                                                      teiidJdbcInfo.getUsername(),
                                                                      teiidJdbcInfo.getPassword(),
                                                                      teiidJdbcInfo.isSecure());

                //pass
                return OutcomeFactory.getInstance().createOK();

            } catch (Throwable ex) {
                String msg = Messages.getString(Messages.ExecutionAdmin.instanceDeployUndeployProblemPingingTeiidJdbc, getUrl());
                return OutcomeFactory.getInstance().createError(msg, new Exception(ex));
            } finally {
                if (teiidJdbcConnection != null) {
                    teiidJdbcConnection.close();
                }

                undeploy(PING_VDB);
            }
        } catch (Exception ex) {
            String msg = Messages.getString(Messages.ExecutionAdmin.instanceDeployUndeployProblemPingingTeiidJdbc, getUrl());
            return OutcomeFactory.getInstance().createError(msg, ex);
        }
    }

    protected abstract boolean isCoherent();

    @Override
    public Outcome ping(ConnectivityType connectivityType) {
        try {
            boolean testCausesConnect = false;
            String msg = Messages.getString(Messages.ExecutionAdmin.cannotConnectToServer, getTeiidAdminInfo().getUsername());

            if (! isCoherent()) {
                try {
                    connect();
                    testCausesConnect = true;
                } catch (Exception ex) {
                    return OutcomeFactory.getInstance().createError(msg, ex);
                }
            }

            Outcome outcome = null;
            if (! isCoherent())
                return OutcomeFactory.getInstance().createError(msg);

            switch (connectivityType) {
                case JDBC:
                    outcome = pingJdbc();
                    break;
                case ADMIN:
                default:
                    outcome = pingAdmin();
            }

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
    public Properties getDataSourceProperties(String name) throws Exception {
        connect();
        return getDataSource(name).getProperties();
    }

    public abstract Collection<ConnectionDriver> getDataSourceDrivers() throws Exception;

    /**
     * Look for an installed driver that has the driverClass which matches the supplied driverClass name.
     * 
     * @param requestDriverClass the driver class to match
     * @return the name of the matching driver, null if not found
     */
    private String getDSMatchForDriverClass(String requestDriverClass) throws Exception {
        if (requestDriverClass == null)
            return null;

        if (!isParentConnected())
            return null;

        try {
            Collection<ConnectionDriver> dataSourceDrivers = getDataSourceDrivers();
            for (ConnectionDriver driver : dataSourceDrivers) {
                String driverClassName = driver.getClassName();
                String driverName = driver.getName();

                if (requestDriverClass.equalsIgnoreCase(driverClassName))
                    return driverName;
            }

        } catch (Exception ex) {
            // Failed to get mapping
            KLog.getLogger().error(Messages.getString(Messages.ExecutionAdmin.failedToGetDriverMappings, requestDriverClass), ex);
        }

        return null;
    }

    protected abstract void createDataSource(String deploymentName, String templateName, Properties properties) throws Exception;

    @Override
    public TeiidDataSource getOrCreateDataSource(String displayName, String dsName, String typeName, Properties properties)
        throws Exception {
        ArgCheck.isNotEmpty(displayName, "displayName"); //$NONNLS1$
        ArgCheck.isNotEmpty(dsName, "dsName"); //$NONNLS1$
        ArgCheck.isNotEmpty(typeName, "typeName"); //$NONNLS1$
        ArgCheck.isNotEmpty(properties, "properties"); //$NONNLS1$

        for (Entry<Object, Object> entry : properties.entrySet()) {
            Object value = entry.getValue();
            String errorMsg = "No value for the connection property '" + entry.getKey() + "'"; //$NONNLS1$ //$NONNLS2$
            ArgCheck.isNotNull(value, errorMsg);
            ArgCheck.isNotEmpty(value.toString(), errorMsg);
        }

        connect();

        //
        // Check for jndi name prefix and drop it
        //
        dsName = manager.getName(dsName);

        // Check if exists, return false
        if (dataSourceExists(dsName)) {
            TeiidDataSource tds = getDataSource(dsName);
            if (tds != null) {
                return tds;
            }
        }

        // For JDBC types, find the matching installed driver.  This is done currently by matching
        // the profile driver classname to the installed driver classname
        String connProfileDriverClass = properties.getProperty("driverclass"); //$NONNLS1$
        if ("connectorjdbc".equals(typeName)) { //$NONNLS1$
            // List of driver jars on the connection profile
            String jarList = properties.getProperty("jarList"); //$NONNLS1$

            // Get first driver name with the driver class that matches the connection profile
            String dsNameMatch = getDSMatchForDriverClass(connProfileDriverClass);

            // If a matching datasource was found, set typename
            if (dsNameMatch != null) {
                typeName = dsNameMatch;
                // No matching datasource, attempt to deploy the driver if jarList is populated.
            } else if (jarList != null && jarList.trim().length() > 0) {
                // Try to deploy the jars
                deployJars(jarList);

                // Retry the name match after deployment.
                dsNameMatch = getDSMatchForDriverClass(connProfileDriverClass);
                if (dsNameMatch != null) {
                    typeName = dsNameMatch;
                }
            }
        }
        // Verify the "typeName" exists.
        if (!getDataSourceTypeNames().contains(typeName)) {
            if ("connectorjdbc".equals(typeName)) { //$NONNLS1$
                throw new Exception(Messages.getString(Messages.ExecutionAdmin.jdbcSourceForClassNameNotFound,
                                                       connProfileDriverClass,
                                                       getUrl()));
            } else {
                throw new Exception(Messages.getString(Messages.ExecutionAdmin.dataSourceTypeDoesNotExist, typeName, getUrl()));
            }
        }

        properties.setProperty(TeiidInstance.DATASOURCE_DISPLAYNAME, displayName);
        createDataSource(dsName, typeName, properties);

        // Check that local name list contains new dsName
        TeiidDataSource tds = getDataSource(dsName);
        if (tds != null) {
            this.getEventManager().notifyListeners(ExecutionConfigurationEvent.createAddDataSourceEvent(tds));
            return tds;
        }

        // We shouldn't get here if data source was created
        throw new Exception(Messages.getString(Messages.ExecutionAdmin.errorCreatingDataSource, dsName, typeName));
    }

    @Override
    public Outcome createVdbDataSource(String vdbName,
                                       String displayName,
                                       String jndiName) {
        Properties props = new Properties();
        String username = this.jdbcInfo.getUsername();
        String password = this.jdbcInfo.getPassword();
        if (username != null) {
            props.put("username", username); //$NONNLS1$
        }
        if (password != null) {
            props.put("password", password); //$NONNLS1$
        }

        props.put("driverclass", "org.teiid.jdbc.TeiidDriver"); //$NONNLS1$ //$NONNLS2$
        props.put("connectionurl", getVdbDataSourceConnectionUrl(vdbName)); //$NONNLS1$

        try {
            connect();
            getOrCreateDataSource(displayName, jndiName, "connectorjdbc", props); //$NONNLS1$
        } catch (Exception ex) {
            String msg = "Error creating data source for VDB " + vdbName; //$NONNLS1$
            return OutcomeFactory.getInstance().createError(msg, ex);
        }

        return OutcomeFactory.getInstance().createOK();
    }

    @Override
    public boolean hasVdb(String name) throws Exception {
        connect();
        return getVdb(name) != null;
    }

    @Override
    public boolean isVdbActive(String vdbName) throws Exception {
        connect();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).isActive();
    }

    @Override
    public boolean isVdbLoading(String vdbName) throws Exception {
        connect();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).isLoading();
    }

    @Override
    public boolean hasVdbFailed(String vdbName) throws Exception {
        connect();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).hasFailed();
    }

    @Override
    public boolean wasVdbRemoved(String vdbName) throws Exception {
        connect();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).wasRemoved();
    }

    @Override
    public List<String> retrieveVdbValidityErrors(String vdbName) throws Exception {
        connect();
        if (!hasVdb(vdbName))
            return Collections.emptyList();

        return getVdb(vdbName).getValidityErrors();
    }

    protected abstract void deploy(String name, InputStream stream) throws Exception;

    @Override
    public void deployDynamicVdb(String deploymentName, InputStream inStream) throws Exception {
        connect();

        ArgCheck.isNotNull(deploymentName, "deploymentName"); //$NONNLS1$
        ArgCheck.isNotNull(inStream, "inStream"); //$NONNLS1$

        deploy(deploymentName, inStream);

        // Give a 0.5 sec pause for the VDB to finish loading metadata.
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            // ignore
        }
    }

    @Override
    public void deployDriver(String driverName, File driverFile) throws Exception {
        connect();
        ArgCheck.isNotNull(driverName, "driverName"); //$NON-NLS-1$
        
        if (!driverFile.exists())
            throw new Exception(Messages.getString(Messages.ExecutionAdmin.jarDeploymentJarNotFound, driverFile.getPath()));

        if (!driverFile.canRead())
            throw new Exception(Messages.getString(Messages.ExecutionAdmin.jarDeploymentJarNotReadable, driverFile.getPath()));

        InputStream iStream = null;
        try {
            iStream = new FileInputStream(driverFile);
        } catch (FileNotFoundException ex) {
            throw ex;
        }

        try {
            deploy(driverName, iStream);

            // Give a 0.5 sec pause for the driver to finish loading.
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                // ignore
            }

        } catch (Exception ex) {
            // Jar deployment failed
            throw ex;
        }
    }

    @Override
    public void undeployDriver(String driverName) throws Exception {
        connect();
        ArgCheck.isNotNull(driverName, "driverName"); //$NON-NLS-1$

        try {
            undeploy(driverName);
        } catch (Exception ex) {
            // Jar deployment failed
            throw ex;
        }
    }

    /*
     * Deploy all jars in the supplied jarList
     *
     * @param jarList the colonseparated list of jar path locations
     */
    protected void deployJars(String jarList) throws Exception {
        // Path Entries are colon separated
        String[] jarPathStrs = jarList.split("[:]"); //$NONNLS1$

        // Attempt to deploy each jar
        for (String jarPathStr : jarPathStrs) {
            File theFile = new File(jarPathStr);
            if (!theFile.exists())
                // The file was not found
                throw new Exception(Messages.getString(Messages.ExecutionAdmin.jarDeploymentJarNotFound, theFile.getPath()));

            if (!theFile.canRead())
                throw new Exception(Messages.getString(Messages.ExecutionAdmin.jarDeploymentJarNotReadable, theFile.getPath()));

            String fileName = theFile.getName();
            InputStream iStream = null;
            try {
                iStream = new FileInputStream(theFile);
            } catch (FileNotFoundException ex) {
                continue;
            }

            try {
                deploy(fileName, iStream);
            } catch (Exception ex) {
                // Jar deployment failed
                throw new Exception(Messages.getString(Messages.ExecutionAdmin.jarDeploymentFailed, theFile.getPath()), ex);
            }
        }
    }

    protected abstract void undeploy(String name) throws Exception;

    @Override
    public void undeployDynamicVdb(String vdbName) throws Exception {
        connect();
        TeiidVdb vdb = getVdb(vdbName);
        if (vdb != null) {
            undeploy(appendDynamicVdbSuffix(vdbName));
        }
        vdb = getVdb(vdbName);

        if (vdb != null)
            this.getEventManager().notifyListeners(ExecutionConfigurationEvent.createUnDeployVDBEvent(vdb.getName()));
    }

    @Override
    public void undeployVdb(String vdbName) throws Exception {
        connect();
        TeiidVdb vdb = getVdb(vdbName);
        if (vdb != null) {
            undeploy(appendVdbExtension(vdbName));
        }
        vdb = getVdb(vdbName);

        if (vdb != null)
            this.getEventManager().notifyListeners(ExecutionConfigurationEvent.createUnDeployVDBEvent(vdb.getName()));
    }

    @Override
    public void update(TeiidInstance otherInstance) {
        ArgCheck.isNotNull(otherInstance);

        disconnect();
        connectionError = null;

        TeiidJdbcInfo otherJdbcInfo = otherInstance.getTeiidJdbcInfo();
        jdbcInfo.setHostProvider(otherJdbcInfo.getHostProvider());
        jdbcInfo.setPassword(otherJdbcInfo.getPassword());
        jdbcInfo.setUsername(otherJdbcInfo.getUsername());
        jdbcInfo.setPort(otherJdbcInfo.getPort());
        jdbcInfo.setSecure(otherJdbcInfo.isSecure());
    }
}
