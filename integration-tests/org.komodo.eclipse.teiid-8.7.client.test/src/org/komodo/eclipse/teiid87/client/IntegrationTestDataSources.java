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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.eclipse.teiid87.client.util.AdminUtil;
import org.komodo.spi.runtime.IDataSourceDriver;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.test.utils.TeiidInstanceBuilder;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.runtime.client.admin.AdminSpec;

/**
 *
 */
@RunWith( Arquillian.class )
@SuppressWarnings( "nls" )
public class IntegrationTestDataSources {

    private ITeiidVersion teiidVersion = new TeiidVersion(ITeiidVersion.VersionID.TEIID_8_7.toString());

    private AdminSpec adminSpec = AdminSpec.getInstance(teiidVersion);

    private Admin admin;

    @Before
    public void setup() throws Exception {

        TeiidInstanceBuilder teiidBuilder = new TeiidInstanceBuilder(teiidVersion);
        teiidBuilder.setHost("localhost");
        teiidBuilder.setPort(9999);
        teiidBuilder.setUserName("admin");
        teiidBuilder.setPassword("admin");

        admin = adminSpec.createAdmin(teiidBuilder.getTeiidInstance());
    }

    @After
    public void teardown() throws AdminException {
        AdminUtil.cleanUp(admin);
        admin.close();
    }

    @Test
    public void testDataSourceDrivers() throws Exception {
        Collection<IDataSourceDriver> dataSourceDrivers = admin.getDataSourceDrivers();
        assertNotNull(dataSourceDrivers);
        assertFalse(dataSourceDrivers.isEmpty());
        for (IDataSourceDriver driver : dataSourceDrivers) {
            assertTrue(driver.getName().contains("teiid") || driver.getName().equals("h2"));
            assertTrue(driver.getClassName().contains("TeiidDriver") || driver.getClassName().contains("h2"));
        }
    }
}
