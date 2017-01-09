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
package org.komodo.relational.teiid.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class CachedTeiidImplTest extends RelationalModelTest {

    private static final String HOST = HostProvider.DEFAULT_HOST;

    private static final TeiidVersion TEIID_VERSION = TeiidVersionProvider.getInstance().getTeiidVersion();

    private static final String JDBC_USERNAME = TeiidJdbcInfo.DEFAULT_JDBC_USERNAME;

    private static final String JDBC_PASSWORD = TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD;

    private static final int JDBC_PORT = TeiidJdbcInfo.DEFAULT_PORT;

    private static final boolean JDBC_SECURE = false;

    private static final String ADMIN_USERNAME = TeiidAdminInfo.DEFAULT_ADMIN_USERNAME;

    private static final String ADMIN_PASSWORD = TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD;

    private static final int ADMIN_PORT = TeiidAdminInfo.Util.defaultPort(TEIID_VERSION);

    private static final boolean ADMIN_SECURE = false;

    private static final String TEIID_NAME = "server";

    protected Teiid teiid;

    @Before
    public void init() throws Exception {
        this.teiid = createTeiid( TEIID_NAME );

        this.teiid.setHost(getTransaction(), HOST);
        this.teiid.setVersion(getTransaction(), TEIID_VERSION);

        this.teiid.setAdminUser(getTransaction(), ADMIN_USERNAME);
        this.teiid.setAdminPassword(getTransaction(), ADMIN_PASSWORD);
        this.teiid.setAdminPort(getTransaction(), ADMIN_PORT);
        this.teiid.setAdminSecure(getTransaction(), ADMIN_SECURE);

        this.teiid.setJdbcUsername(getTransaction(), JDBC_USERNAME);
        this.teiid.setJdbcPassword(getTransaction(), JDBC_PASSWORD);
        this.teiid.setJdbcPort(getTransaction(), JDBC_PORT);
        this.teiid.setJdbcSecure(getTransaction(), JDBC_SECURE);
        commit(State.COMMITTED);
    }

    @Test
    public void shouldCloneTeiidInstance() throws Exception {
        //
        // Cannot only be created by sys
        //
        CachedTeiid cTeiid = RelationalModelFactory.createCachedTeiid(sysTx(), _repo, this.teiid);;
        assertNotNull(cTeiid);
        sysCommit();

        assertEquals(HOST, cTeiid.getHost(getTransaction()));
        assertEquals(TEIID_VERSION, cTeiid.getVersion(getTransaction()));
        assertEquals(ADMIN_USERNAME, cTeiid.getAdminUser(getTransaction()));
        assertEquals(ADMIN_PASSWORD, cTeiid.getAdminPassword(getTransaction()));
        assertEquals(ADMIN_PORT, cTeiid.getAdminPort(getTransaction()));
        assertEquals(ADMIN_SECURE, cTeiid.isAdminSecure(getTransaction()));
        assertEquals(JDBC_USERNAME, cTeiid.getJdbcUsername(getTransaction()));
        assertEquals(JDBC_PASSWORD, cTeiid.getJdbcPassword(getTransaction()));
        assertEquals(JDBC_PORT, cTeiid.getJdbcPort(getTransaction()));
        assertEquals(JDBC_SECURE, cTeiid.isJdbcSecure(getTransaction()));

        KomodoObject teiidCache = _repo.komodoTeiidCache(getTransaction());
        KomodoObject[] teiids = teiidCache.getChildrenOfType(getTransaction(), KomodoLexicon.CachedTeiid.NODE_TYPE);
        assertEquals(1, teiids.length);
        commit(State.COMMITTED);

        // Try creating a second time
        cTeiid = RelationalModelFactory.createCachedTeiid(sysTx(), _repo, this.teiid);
        assertNotNull(cTeiid);
        sysCommit();

        teiids = teiidCache.getChildrenOfType(getTransaction(), KomodoLexicon.CachedTeiid.NODE_TYPE);
        assertEquals(1, teiids.length); // Should only be 1 since if it exists it deletes it.
    }

    @Test
    public void shouldSequencerVdb() throws Exception {
        CachedTeiid cachedTeiid = RelationalModelFactory.createCachedTeiid(sysTx(), _repo, this.teiid);
        assertNotNull(cachedTeiid);
        sysCommit();

        String vdbName = TestUtilities.PORTFOLIO_VDB_NAME;
        InputStream stream = TestUtilities.portfolioExample();
        String content = StringUtils.inputStreamToString(stream);

        File tempFile = File.createTempFile(VDB_PREFIX, XML_SUFFIX);
        tempFile.deleteOnExit();
        Files.write(Paths.get(tempFile.getPath()), content.getBytes());

        UnitOfWork sysTx = sysTx();
        KomodoObject kobject = cachedTeiid.addChild(sysTx, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb vdb = new VdbImpl( sysTx, _repo, kobject.getAbsolutePath());
        vdb.setOriginalFilePath(sysTx, tempFile.getAbsolutePath());
        vdb.setVdbName( sysTx, vdbName );

        KomodoObject fileNode = vdb.addChild(sysTx, JcrLexicon.CONTENT.getString(), null);
        fileNode.setProperty(sysTx, JcrLexicon.DATA.getString(), content);
        sysCommit();

        Model[] models = vdb.getModels(getTransaction());
        assertEquals(5, models.length);
    }
}
