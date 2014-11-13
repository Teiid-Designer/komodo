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
package org.komodo.eclipse.teiid84.client;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.VersionID;
import org.komodo.test.utils.AbstractTeiidVersionTest;
import org.mockito.Mockito;
import org.teiid.runtime.client.instance.TCTeiidInstance;

/**
 *
 */
@RunWith( Arquillian.class )
@SuppressWarnings( {"javadoc", "nls"} )
public class IntegrationTestTeiidInstance extends AbstractTeiidVersionTest {

    private static final VersionID TEIID_VERSION_ID = TeiidVersion.VersionID.TEIID_8_4;

    private TeiidParent teiidParent;

    private TCTeiidInstance teiidInstance;

    private TeiidJdbcInfo teiidJdbcInfo;

    /**
     * 
     */
    public IntegrationTestTeiidInstance() {
        super(TEIID_VERSION_ID);
    }

    @Before
    public void setup() throws Exception {
        teiidParent = Mockito.mock(TeiidParent.class);

        when(teiidParent.getHost()).thenReturn("localhost");
        when(teiidParent.getPort()).thenReturn(9999);
        when(teiidParent.getUserName()).thenReturn("admin");
        when(teiidParent.getPassword()).thenReturn("admin");

        teiidJdbcInfo = Mockito.mock(TeiidJdbcInfo.class);
        when(teiidJdbcInfo.getHostProvider()).thenReturn(teiidParent);
        when(teiidJdbcInfo.getPort()).thenReturn(TeiidJdbcInfo.DEFAULT_PORT);
        when(teiidJdbcInfo.getUsername()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        when(teiidJdbcInfo.getPassword()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);
        when(teiidJdbcInfo.isSecure()).thenReturn(true);

        teiidInstance = new TCTeiidInstance(teiidParent, teiidJdbcInfo);
    }

    @Test
    public void testVersion() throws Exception {
        TeiidVersion version = teiidInstance.getVersion();
        assertEquals(getTeiidVersion(), version);
    }
}
