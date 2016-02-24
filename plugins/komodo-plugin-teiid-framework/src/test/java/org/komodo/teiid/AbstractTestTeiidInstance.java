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
import java.util.Collection;
import java.util.Properties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.test.utils.TestUtilities;

public abstract class AbstractTestTeiidInstance {

    protected class DummyEventManager implements EventManager {

        @Override
        public boolean addListener(ExecutionConfigurationListener listener) {
            return true;
        }

        @Override
        public void permitListeners(boolean enable) {
            // Do Nothing
        }

        @Override
        public void notifyListeners(ExecutionConfigurationEvent event) {
            // Do Nothing
        }

        @Override
        public boolean removeListener(ExecutionConfigurationListener listener) {
            return true;
        }
    }

    private TeiidInstance teiidInstance;

    protected abstract TeiidInstance createTeiidInstance(TeiidParent parent,
                                                                                                     TeiidVersion teiidVersion,
                                                                                                     TeiidJdbcInfo jdbcInfo) throws Exception;

    protected abstract TeiidVersion getVersion();

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
        when(parent.getPort()).thenReturn(TeiidAdminInfo.DEFAULT_PORT);
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
}
