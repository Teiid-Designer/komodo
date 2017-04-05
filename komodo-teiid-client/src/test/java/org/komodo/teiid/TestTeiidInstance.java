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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.teiid.impl.TeiidInstanceImpl;
import org.komodo.test.utils.DummyEventManager;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.teiid.adminapi.AdminProcessingException;

@RunWith( Arquillian.class )
public class TestTeiidInstance {

    private TeiidInstance teiidInstance;

    private TeiidInstance createTeiidInstance(TeiidParent parent, TeiidVersion teiidVersion,
                                                                                    TeiidJdbcInfo jdbcInfo) throws Exception {
        return new TeiidInstanceImpl(parent, teiidVersion, jdbcInfo);
    }

    private TeiidVersion getVersion() {
        return DefaultTeiidVersion.Version.DEFAULT_TEIID_VERSION.get();
    }

    public TeiidInstance getTeiidInstance() {
        return teiidInstance;
    }

    @Before
    public void setup() throws Exception {
        EventManager eventMgr = new DummyEventManager();

        TeiidParent parent = mock(TeiidParent.class);
        when(parent.getHost()).thenReturn(HostProvider.DEFAULT_HOST);
        when(parent.getUsername()).thenReturn(TeiidAdminInfo.DEFAULT_ADMIN_USERNAME);
        when(parent.getPassword()).thenReturn(TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD);
        when(parent.getPort()).thenReturn(TeiidAdminInfo.Util.defaultPort(getVersion()));
        when(parent.getEventManager()).thenReturn(eventMgr);

        TeiidJdbcInfo jdbcInfo = mock(TeiidJdbcInfo.class);
        when(jdbcInfo.getHostProvider()).thenReturn(parent);
        when(jdbcInfo.getUsername()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        when(jdbcInfo.getPassword()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);
        when(jdbcInfo.getPort()).thenReturn(TeiidJdbcInfo.DEFAULT_PORT);
        when(jdbcInfo.isSecure()).thenReturn(true);

        teiidInstance = createTeiidInstance(parent, getVersion(), jdbcInfo);
    }

    @After
    public void teardown() throws Exception {
        if (teiidInstance != null) {
            teiidInstance.disconnect();
            teiidInstance = null;
        }
    }

    @Test
    public void testVersion() throws Exception {
        TeiidVersion version = teiidInstance.getVersion();
        assertEquals(getVersion(), version);
    }

    @Test
    public void testConnection() throws Exception {
        teiidInstance.connect();

        // tests deployment using ping vdb
        assertTrue(teiidInstance.isConnected());
    }

    @Test
    public void testCreateDataSource() throws Exception {
        String displayName = "h2-connector";
        String type = "h2";
        String dsName = "accounts-ds";
        String jndiName = "java:/accounts-ds";
        String connUrl = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1";

        if (teiidInstance.dataSourceExists(dsName))
            teiidInstance.deleteDataSource(dsName);

        Properties properties = new Properties();
        properties.setProperty(TeiidInstance.DATASOURCE_JNDINAME, jndiName);
        properties.setProperty(TeiidInstance.DATASOURCE_CONNECTION_URL, connUrl);

        TeiidDataSource accountsDS = teiidInstance.getOrCreateDataSource(displayName, dsName, type, properties);
        assertNotNull(accountsDS);

        assertEquals(dsName, accountsDS.getName());
        assertEquals(type, accountsDS.getType());
        assertEquals(jndiName, accountsDS.getPropertyValue(TeiidInstance.DATASOURCE_JNDINAME));
        assertEquals(connUrl, accountsDS.getPropertyValue(TeiidInstance.DATASOURCE_CONNECTION_URL));

        teiidInstance.deleteDataSource(dsName);
        assertFalse(teiidInstance.dataSourceExists(dsName));
    }

    @Test
    public void testDeployment() throws Exception {
        teiidInstance.deployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE, TestUtilities.sampleExample());
        Thread.sleep(2000);

        Collection<TeiidVdb> vdbs = teiidInstance.getVdbs();
        assertEquals(1, vdbs.size());

        TeiidVdb vdb = teiidInstance.getVdb(TestUtilities.SAMPLE_VDB_NAME);
        assertNotNull(vdb);
        assertEquals(0, vdb.getValidityErrors().size());

        assertTrue(vdb.isActive());

        teiidInstance.undeployDynamicVdb(TestUtilities.SAMPLE_VDB_NAME);
        vdbs = teiidInstance.getVdbs();
        assertEquals(0, vdbs.size());
    }

    private void wait(int seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (Exception ex) {
            // Nothing required
        }
    }

    private TeiidDataSource deployDataSource() throws Exception {
        /*
             <jdbc-connection name="MySqlPool">
                <jndi-name>java:/MySqlDS</jndi-name>
                <driver-name>mysql-connector-java-5.1.39-bin.jarcom.mysql.jdbc.Driver_5_1</driver-name>
                <property name="track-statements">NOWARN</property>
                <property name="connection-url">jdbc:mysql://db4free.net:3306/usstates</property>
                <property name="share-prepared-statements">false</property>
                <property name="statistics-enabled">false</property>
                <property name="validate-on-match">false</property>
                <property name="allow-multiple-users">false</property>
                <property name="user-name">komodo</property>
                <property name="password">XUMz4vBKuA2v</property>
                <property name="use-fast-fail">false</property>
                <property name="set-tx-query-timeout">false</property>
                <property name="spy">false</property>
            </jdbc-connection>
         */

        String displayName = "MySqlPool";
        String jndiName = "java:/MySqlDS";
        String typeName = "mysql-connector_com.mysql.jdbc.Driver_5_1";
        Properties properties = new Properties();
        properties.setProperty("password", "XUMz4vBKuA2v");
        properties.setProperty("user-name", "komodo");
        properties.setProperty("validate-on-match", "false");
        properties.setProperty("connection-url", "jdbc:mysql://db4free.net:3306/usstates");
        properties.setProperty("jndi-name", "java:/MySqlDS");
        properties.setProperty("driver-name", "mysql-connector-java-5.1.39-bin.jar_com.mysql.jdbc.Driver_5_1");

        TeiidDataSource teiidDataSource = getTeiidInstance().getOrCreateDataSource(displayName, jndiName, typeName, properties);
        assertNotNull(teiidDataSource);
        return teiidDataSource;
    }

    @Test
    @Ignore( "Test demonstrates TEIID-4592 / 3834. Question when it will be fixed for Teiid 9.1+" )
    public void testDataSourceOperations() throws Exception {
        getTeiidInstance().connect();
        assertTrue(getTeiidInstance().isConnected());

        String MYSQL_DRIVER = "mysql-connector";

        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);

        File driverFile = TestUtilities.createTempFile(MYSQL_DRIVER, ".jar");
        driverFile.deleteOnExit();
        FileUtils.write(driverStream, driverFile);
        getTeiidInstance().deployDriver(MYSQL_DRIVER, driverFile);

        wait(2);

        TeiidDataSource teiidDataSource = deployDataSource();
        assertTrue(getTeiidInstance().dataSourceExists(teiidDataSource.getName()));

        String dsName = teiidDataSource.getName();
        getTeiidInstance().deleteDataSource(dsName);

        wait(4);

        getTeiidInstance().reconnect();

        assertFalse(getTeiidInstance().dataSourceExists(dsName));

        /*
         * Try redeploying
         */
        try {
            teiidDataSource = deployDataSource();
            wait(4);
            getTeiidInstance().reconnect();

            assertTrue(getTeiidInstance().dataSourceExists(dsName));
        } catch (Exception ex) {
            throw ex;
        } finally {
            if (getTeiidInstance().dataSourceExists(dsName)) {
                // Ensure everything tidied up at the end
                getTeiidInstance().deleteDataSource(dsName);
                wait(4);
            }
        }
    }

    @Test
    public void testDataSourceDrivers() throws Exception {
        getTeiidInstance().connect();
        assertTrue(getTeiidInstance().isConnected());
        Collection<ConnectionDriver> dataSourceDrivers = getTeiidInstance().getDataSourceDrivers();
        assertTrue(dataSourceDrivers.size() > 0);

        String[] driverNamesArr = {"h2", "teiid-local", "teiid"};
        List<String> driverNames = Arrays.asList(driverNamesArr);
        String[] classNamesArr = {"org.teiid.jdbc.TeiidDriver", "org.h2.Driver"};
        List<String> classNames = Arrays.asList(classNamesArr);

        Iterator<ConnectionDriver> iter = dataSourceDrivers.iterator();
        while (iter.hasNext()) {
            ConnectionDriver driver = iter.next();
            assertTrue(driverNames.contains(driver.getName()));
            assertTrue(classNames.contains(driver.getClassName()));
        }
    }

    @Test
    public void testRuntimeVersion() throws Exception {
        getTeiidInstance().connect();
        assertTrue(getTeiidInstance().isConnected());
        TeiidVersion version = getTeiidInstance().getRuntimeVersion();
        assertNotNull(version);
        assertEquals(TeiidVersionProvider.getInstance().getTeiidVersion(), version);
    }

    @Test
    public void testGetSchema() throws Exception {
        getTeiidInstance().connect();
        assertTrue(getTeiidInstance().isConnected());

        try {
            getTeiidInstance().getSchema("blah", "1.0", "model");
        } catch (InvocationTargetException ex) {
            //
            // Should throw this exception since blah does not exist but should not
            // throw a NumberFormatException or NoSuchMethodException
            //
            Throwable cause = ex.getCause();
            assertTrue(cause instanceof AdminProcessingException);
            assertTrue(cause.getMessage().contains("does not exist or is not ACTIVE"));
        }
    }
}
