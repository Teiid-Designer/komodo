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
package org.komodo.rest.service;

import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.repository.RepositoryImpl;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.spi.constants.StringConstants;
import org.komodo.test.utils.TestUtilities;
import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceQueryTests extends AbstractKomodoTeiidServiceTest implements StringConstants {

    @Override
    protected int getTestTotalInClass() {
        return 3;
    }

    @Before
    public void setup() throws Exception {
        importDataService();

        // Avoid deploying more than once
        if (! helperInstance.hasVdb(TestUtilities.US_STATES_VDB_NAME))
            deployDataService();

        //
        // Give the vdb time to become active
        //
        waitForVdb("usstates");
    }

    @Test
    public void shouldQueryTeiid() throws Exception {
        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery("SELECT * FROM state");
        queryAttr.setTarget("usstates");

        queryDataService(queryAttr, 59, 1);
    }

    @Test
    public void shouldQueryTeiidWithLimitAndOffset() throws Exception {
        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery("SELECT * FROM state");
        queryAttr.setTarget("usstates");

        int offset = 5;
        int limit = 10;
        queryAttr.setLimit(limit);
        queryAttr.setOffset(offset);

        queryDataService(queryAttr, limit, offset);
    }

    @Test
    public void shouldQueryTeiidUsingDataservice() throws Exception {
        String dsPath = RepositoryImpl.komodoWorkspacePath(null) + FORWARD_SLASH +
                                USER_NAME + FORWARD_SLASH + "UsStatesService";

        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery("SELECT * FROM state");
        queryAttr.setTarget(dsPath);

        queryDataService(queryAttr, 59, 1);
    }
}
