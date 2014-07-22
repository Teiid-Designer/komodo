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
package org.komodo.eclipse.teiid87.client;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.spi.runtime.ITeiidJdbcInfo;
import org.komodo.spi.runtime.ITeiidParent;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.ITeiidVersion.VersionID;
import org.komodo.test.utils.AbstractTeiidVersionTest;
import org.mockito.Mockito;
import org.teiid.runtime.client.instance.TeiidInstance;

/**
 *
 */
@RunWith( Arquillian.class )
@SuppressWarnings( {"javadoc", "nls"} )
public class IntegrationTestTeiidInstance extends AbstractTeiidVersionTest {

    private static final VersionID TEIID_VERSION_ID = ITeiidVersion.VersionID.TEIID_8_7;

    private ITeiidParent teiidParent;

    private TeiidInstance teiidInstance;

    private ITeiidJdbcInfo teiidJdbcInfo;

    public IntegrationTestTeiidInstance() {
        super(TEIID_VERSION_ID);
    }

    @Before
    public void setup() throws Exception {
        teiidParent = Mockito.mock(ITeiidParent.class);

        when(teiidParent.getHost()).thenReturn("localhost");
        when(teiidParent.getPort()).thenReturn(9999);
        when(teiidParent.getUserName()).thenReturn("admin");
        when(teiidParent.getPassword()).thenReturn("admin");

        teiidJdbcInfo = Mockito.mock(ITeiidJdbcInfo.class);
        when(teiidJdbcInfo.getHostProvider()).thenReturn(teiidParent);
        when(teiidParent.getPort()).thenReturn(ITeiidJdbcInfo.DEFAULT_PORT);
        when(teiidParent.getUserName()).thenReturn(ITeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        when(teiidParent.getPassword()).thenReturn(ITeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);

        teiidInstance = new TeiidInstance(teiidParent, teiidJdbcInfo);
    }

    @Test
    public void testVersion() throws Exception {
        ITeiidVersion version = teiidInstance.getVersion();
        assertEquals(getTeiidVersion(), version);
    }
}
