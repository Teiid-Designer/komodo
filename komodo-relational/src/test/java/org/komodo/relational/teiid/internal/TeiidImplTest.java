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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.KException;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidConnectionInfo;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.teiid.TeiidServiceProvider;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TeiidImplTest extends RelationalModelTest {

    private static final String TEIID_NAME = "server";

    protected Teiid teiid;

    private TeiidVersion version;

    private TeiidService unmockedTeiidService;

    private CachedTeiid cachedTeiid;

    private TeiidInstance mockTeiidInstance() throws Exception {
        TeiidInstance teiidInstance = mock(TeiidInstance.class);
        when(teiidInstance.getVersion()).thenReturn(version);
        when(teiidInstance.isConnected()).thenReturn(true);

        TeiidService teiidService = mock(TeiidService.class);
        when(teiidService.getVersion()).thenReturn(version);
        when(teiidService.getTeiidInstance(any(TeiidParent.class), any(TeiidJdbcInfo.class)))
                    .thenReturn(teiidInstance);

        TeiidServiceProvider.getInstance().setTeiidService(teiidService);

        return teiidInstance;
    }

    private void setTeiidCacheExpireThreshold(long expireThreshold) throws KException, Exception {
        KomodoObject teiidCache = _repo.komodoTeiidCache(sysTx());
        assertNotNull(teiidCache);

        teiidCache.setProperty(sysTx(), KomodoLexicon.TeiidCache.EXPIRATION_THRESHOLD, expireThreshold);
        sysCommit();
    }

    @Before
    public void init() throws Exception {
        setTeiidCacheExpireThreshold(CachedTeiid.DEFAULT_TEIID_CACHE_THRESHOLD); // Ensure the cache teiids are always overwritten

        this.teiid = createTeiid( TEIID_NAME );
        this.version = teiid.getVersion(getTransaction());
        this.unmockedTeiidService = TeiidServiceProvider.getInstance().getTeiidService(version);
    }

    @After
    public void teardown() throws Exception {
        TeiidServiceProvider.getInstance().setTeiidService(unmockedTeiidService);

        if (this.cachedTeiid != null) {
            this.cachedTeiid.remove(sysTx());
            this.cachedTeiid = null;
        }
    }

    @Test
    public void shouldHaveDefaultTeiidInstanceHost() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(getTransaction());
        assertThat( teiidInstance.getHost(), is( HostProvider.DEFAULT_HOST ) );
    }

    @Test
    public void shouldSetHost() throws Exception {
        String newValue = "NewHost";
        this.teiid.setHost( getTransaction(), newValue );
        assertThat( this.teiid.getHost( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldHaveDefaultTeiidInstanceUrl() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(getTransaction());
        assertThat( teiidInstance.getUrl(), is( TeiidConnectionInfo.UrlConstructor.createDefaultTeiidConnectionUrl()) );
    }

    @Test
    public void shouldCheckDefaultIsConnected() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(getTransaction());
        assertThat( teiidInstance.isConnected(), is( false ) );
    }

    @Test
    public void shouldSetAdminSecure() throws Exception {
        boolean newValue = false;
        this.teiid.setAdminSecure( getTransaction(), newValue );
        assertThat( this.teiid.isAdminSecure( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetAdminUser() throws Exception {
        String newValue = "MyUser";
        this.teiid.setAdminUser( getTransaction(), newValue );
        assertThat( this.teiid.getAdminUser( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetAdminPassword() throws Exception {
        String newValue = "pword";
        this.teiid.setAdminPassword( getTransaction(), newValue );
        assertThat( this.teiid.getAdminPassword( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetAdminPort() throws Exception {
        int newValue = 8888;
        this.teiid.setAdminPort( getTransaction(), newValue );
        assertThat( this.teiid.getAdminPort( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetJdbcPassword() throws Exception {
        String newValue = "pword";
        this.teiid.setJdbcPassword( getTransaction(), newValue );
        assertThat( this.teiid.getJdbcPassword( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetJdbcSecure() throws Exception {
        boolean newValue = true;
        this.teiid.setJdbcSecure( getTransaction(), newValue );
        assertThat( this.teiid.isJdbcSecure( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.teiid.getName( getTransaction() ), is( TEIID_NAME ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.teiid.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.Teiid.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.teiid.getTypeIdentifier( getTransaction() ), is(KomodoType.TEIID));
    }

    @Test
    public void shouldHaveDefaultAdminSecureAfterConstruction() throws Exception {
        assertThat( this.teiid.isAdminSecure( getTransaction() ), is( TeiidAdminInfo.DEFAULT_SECURE ) );
    }

    @Test
    public void shouldHaveDefaultAdminUserAfterConstruction() throws Exception {
        assertThat( this.teiid.getAdminUser( getTransaction() ), is( TeiidAdminInfo.DEFAULT_ADMIN_USERNAME ) );
    }

    @Test
    public void shouldHaveDefaultAdminPasswordAfterConstruction() throws Exception {
        assertThat( this.teiid.getAdminPassword( getTransaction() ), is( TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD ) );
    }

    @Test
    public void shouldHaveDefaultAdminPortAfterConstruction() throws Exception {
        TeiidVersion teiidVersion = TeiidVersionProvider.getInstance().getTeiidVersion();
        assertThat( this.teiid.getAdminPort( getTransaction() ), is( TeiidAdminInfo.Util.defaultPort(teiidVersion) ) );
    }

    @Test
    public void shouldHaveDefaultJdbcPasswordAfterConstruction() throws Exception {
        assertThat( this.teiid.getJdbcPassword( getTransaction() ), is( TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD ) );
    }

    @Test
    public void shouldHaveDefaultJdbcSecureAfterConstruction() throws Exception {
        assertThat( this.teiid.isJdbcSecure( getTransaction() ), is( TeiidJdbcInfo.DEFAULT_SECURE ) );
    }

    @Test
    public void shouldImportBareTeiidInstance() throws Exception {
        mockTeiidInstance();
        cachedTeiid = this.teiid.importContent(sysTx());
    }

    @Test
    public void shouldImportTeiidInstanceWithContent() throws Exception {
        TeiidInstance teiidInstance = mockTeiidInstance();

        String dsName = "Products";
        Properties properties = new Properties();
        properties.setProperty(TeiidInstance.DATASOURCE_CONNECTION_URL, "jdbc:teiid:Products@mm://localhost:31000");
        properties.setProperty(TeiidInstance.DATASOURCE_JNDINAME, "java:/Products");
        properties.setProperty(TeiidInstance.DATASOURCE_DRIVERNAME, "teiid,");
        TeiidDataSource ds = mock(TeiidDataSource.class);
        when(ds.getName()).thenReturn(dsName);
        when(ds.getConnectionUrl()).thenReturn(properties.getProperty(TeiidInstance.DATASOURCE_CONNECTION_URL));
        when(ds.getJndiName()).thenReturn(properties.getProperty(TeiidInstance.DATASOURCE_JNDINAME));
        when(ds.getType()).thenReturn(properties.getProperty(TeiidInstance.DATASOURCE_DRIVERNAME));
        when(ds.getProperties()).thenReturn(new Properties());

        Collection<TeiidDataSource> dataSources = new ArrayList<>();
        dataSources.add(ds);
        when(teiidInstance.getDataSources()).thenReturn(dataSources);

        InputStream portfolioExample = TestUtilities.portfolioExample();
        String portfolioXml = StringUtils.inputStreamToString(portfolioExample);
        TeiidVdb portfolioVdb = mock(TeiidVdb.class);
        when(portfolioVdb.getName()).thenReturn(TestUtilities.PORTFOLIO_VDB_FILE);
        when(portfolioVdb.export()).thenReturn(portfolioXml);

        InputStream partsExample = TestUtilities.partsExample();
        String partsXml = StringUtils.inputStreamToString(partsExample);
        TeiidVdb partsVdb = mock(TeiidVdb.class);
        when(partsVdb.getName()).thenReturn(TestUtilities.PARTS_VDB_FILE);
        when(partsVdb.export()).thenReturn(partsXml);

        Collection<TeiidVdb> vdbs = new ArrayList<>();
        vdbs.add(portfolioVdb);
        vdbs.add(partsVdb);
        when(teiidInstance.getVdbs()).thenReturn(vdbs);

        TeiidTranslator translator = mock(TeiidTranslator.class);
        String trName = "h2";
        when(translator.getName()).thenReturn(trName);
        when(translator.getDescription()).thenReturn("A translator for open source H2 Database");
        when(translator.getProperties()).thenReturn(new Properties());

        Collection<TeiidTranslator> translators = new ArrayList<>();
        translators.add(translator);
        when(teiidInstance.getTranslators()).thenReturn(translators);

        //
        // Have to import the cached teiid as sys
        //
        cachedTeiid = this.teiid.importContent(sysTx());
        assertNotNull(cachedTeiid);
        sysCommit();

        //
        // Can read the contents of the cached teiid as user
        //
        assertEquals(this.teiid.getName(getTransaction()), cachedTeiid.getName(getTransaction()));
        assertEquals(this.teiid.getAdminUser(getTransaction()), cachedTeiid.getAdminUser(getTransaction()));
        assertEquals(this.teiid.getJdbcUsername(getTransaction()), cachedTeiid.getJdbcUsername(getTransaction()));
        assertEquals(this.teiid.getVersion(getTransaction()), cachedTeiid.getVersion(getTransaction()));

        KomodoObject[] cDataSrcs = cachedTeiid.getConnections(getTransaction());
        
        assertNotNull(cDataSrcs);
        assertEquals(1, cDataSrcs.length);
        assertEquals(dsName, cDataSrcs[0].getName(getTransaction()));

        KomodoObject[] cVdbs = cachedTeiid.getVdbs(getTransaction());
        assertNotNull(cVdbs);
        assertEquals(2, cVdbs.length);

        for (KomodoObject ko : cVdbs) {
            if (TestUtilities.PORTFOLIO_VDB_FILE.equals(ko.getName(getTransaction()))) {
                assertEquals(5, ko.getChildrenOfType(getTransaction(), VdbLexicon.Vdb.DECLARATIVE_MODEL).length);
            } else if (TestUtilities.PARTS_VDB_FILE.equals(ko.getName(getTransaction()))) {
                assertEquals(2, ko.getChildrenOfType(getTransaction(), VdbLexicon.Vdb.DECLARATIVE_MODEL).length);
            } else
                fail();
        }

        KomodoObject[] cTranslators = cachedTeiid.getTranslators(getTransaction());
        assertNotNull(cTranslators);
        assertEquals(1, cTranslators.length);
        assertEquals(trName, cTranslators[0].getName(getTransaction()));

        traverse(getTransaction(), cachedTeiid.getAbsolutePath());
    }

    @Test
    public void shouldNotReimportUnlessCacheTeiidHasExpired() throws Exception {
        mockTeiidInstance();
        UnitOfWork sysTx = sysTx();
        cachedTeiid = this.teiid.importContent(sysTx);
        Long origTimestamp = cachedTeiid.getTimestamp(sysTx);
        assertNotNull(origTimestamp);

        // Default expiration is 10 minutes so should return the same cachedTeiid
        cachedTeiid = this.teiid.importContent(sysTx);
        Long newTimestamp = cachedTeiid.getTimestamp(sysTx);

        assertEquals(origTimestamp, newTimestamp);
    }

    @Test
    public void shouldReimportAsCacheTeiidHasExpired() throws Exception {
        setTeiidCacheExpireThreshold(0); // Ensure the cache teiids are always overwritten

        mockTeiidInstance();
        UnitOfWork sysTx = sysTx();
        cachedTeiid = this.teiid.importContent(sysTx);
        Long origTimestamp = cachedTeiid.getTimestamp(sysTx);
        assertNotNull(origTimestamp);

        // Expiration time is 0 so should create a new one with a different timestamp;
        cachedTeiid = this.teiid.importContent(sysTx);
        Long newTimestamp = cachedTeiid.getTimestamp(sysTx);

        assertNotEquals(origTimestamp, newTimestamp);
    }
}
