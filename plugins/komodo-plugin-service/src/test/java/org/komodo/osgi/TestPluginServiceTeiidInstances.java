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
package org.komodo.osgi;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.util.Set;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;

@RunWith(Arquillian.class)
public class TestPluginServiceTeiidInstances extends AbstractTestPluginService {

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

    @Test
    public void testTeiidInstanceConnection() throws Exception {
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

        Set<TeiidVersion> versions = PluginService.getInstance().getSupportedTeiidVersions();
        assertTrue(versions.size() > 0);

        for (TeiidVersion version : versions) {
            TeiidService teiidService = service.getTeiidService(version);
            assertNotNull(teiidService);

            TeiidInstance teiidInstance = teiidService.getTeiidInstance(parent, jdbcInfo); 
            teiidInstance.connect();
            assertTrue(teiidInstance.isConnected());

            teiidInstance.disconnect();
        }
    }
}
