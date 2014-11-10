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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.eclipse.teiid84.client.util.AdminUtil;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.VersionID;
import org.komodo.test.utils.AbstractTeiidVersionTest;
import org.komodo.test.utils.TeiidInstanceBuilder;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.runtime.client.admin.AdminSpec;

/**
 *
 */
@RunWith( Arquillian.class )
@SuppressWarnings( {"javadoc", "nls"} )
public class IntegrationTestDataSources extends AbstractTeiidVersionTest {

    private static final VersionID TEIID_VERSION_ID = TeiidVersion.VersionID.TEIID_8_4;

    private final AdminSpec adminSpec;

    private Admin admin;

    public IntegrationTestDataSources() {
        super(TEIID_VERSION_ID);
        adminSpec = AdminSpec.getInstance(getTeiidVersion());
    }

    @Before
    public void setup() throws Exception {

        TeiidInstanceBuilder teiidBuilder = createTeiidInstanceBuilder();
        admin = adminSpec.createAdmin(teiidBuilder.getTeiidInstance());
    }

    @After
    public void teardown() throws AdminException {
        AdminUtil.cleanUp(admin);
        admin.close();
    }

    @Test
    public void testDataSourceDrivers() throws Exception {
        Collection<DataSourceDriver> dataSourceDrivers = admin.getDataSourceDrivers();
        assertNotNull(dataSourceDrivers);
        assertFalse(dataSourceDrivers.isEmpty());
        for (DataSourceDriver driver : dataSourceDrivers) {
            assertTrue(driver.getName().contains("teiid") || driver.getName().equals("h2"));
            assertTrue(driver.getClassName().contains("TeiidDriver") || driver.getClassName().contains("h2"));
        }
    }
}
