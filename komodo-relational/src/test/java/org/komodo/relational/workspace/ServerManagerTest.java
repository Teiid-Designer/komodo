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
package org.komodo.relational.workspace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.teiid.Teiid;
import org.komodo.repository.RepositoryImpl;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerManagerTest extends RelationalModelTest {

    private ServerManager serverMgr;

    @Before
    public void obtainServerManager() throws Exception {
        serverMgr = ServerManager.getInstance(_repo);
    }

    @After
    public void uncacheServerManager() {
        ServerManager.uncacheInstance(_repo);
        serverMgr = null;
    }

    @Test
    public void shouldCreateDefaultServer() throws Exception {
        Teiid teiid = serverMgr.getDefaultServer(getTransaction());
        assertNotNull(teiid);

        assertEquals(RepositoryImpl.SERVERS_ROOT + FORWARD_SLASH + ServerManager.DEFAULT_SERVER_NAME,
                                 teiid.getAbsolutePath());

        teiid.setAdminUser(getTransaction(), "admin2");
        assertEquals("admin2", teiid.getAdminUser(getTransaction()));
    }
}
