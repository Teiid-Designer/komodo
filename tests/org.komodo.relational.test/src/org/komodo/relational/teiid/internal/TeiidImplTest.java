/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.teiid.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TeiidImplTest extends RelationalModelTest {

    private static final String TEIID_NAME = "server";

    protected Teiid teiid;

    @Before
    public void init() throws Exception {
        this.teiid = createTeiid( TEIID_NAME );
    }

    @Test
    public void shouldHaveDefaultTeiidInstanceHost() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(this.uow);
        assertThat( teiidInstance.getHost(), is( HostProvider.DEFAULT_HOST ) );
    }
    
    @Test
    public void shouldHaveDefaultTeiidInstanceUrl() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(this.uow);
        assertThat( teiidInstance.getUrl(), is( "mms://localhost:9999" ) );
    }
    
    @Test
    public void shouldCheckDefaultIsConnected() throws Exception {
        TeiidInstance teiidInstance = this.teiid.getTeiidInstance(this.uow);
        assertThat( teiidInstance.isConnected(), is( false ) );
    }
    
    @Test
    public void shouldSetAdminSecure() throws Exception {
        boolean newValue = false;
        this.teiid.setAdminSecure( this.uow, newValue );
        assertThat( this.teiid.isAdminSecure( this.uow ), is( newValue ) );
    }
    
    @Test
    public void shouldSetAdminUser() throws Exception {
        String newValue = "MyUser";
        this.teiid.setAdminUser( this.uow, newValue );
        assertThat( this.teiid.getAdminUser( this.uow ), is( newValue ) );
    }
    
    @Test
    public void shouldSetAdminPassword() throws Exception {
        String newValue = "pword";
        this.teiid.setAdminPassword( this.uow, newValue );
        assertThat( this.teiid.getAdminPassword( this.uow ), is( newValue ) );
    }
    
    @Test
    public void shouldSetAdminPort() throws Exception {
        int newValue = 8888;
        this.teiid.setAdminPort( this.uow, newValue );
        assertThat( this.teiid.getAdminPort( this.uow ), is( newValue ) );
    }
    
    @Test
    public void shouldSetJdbcPassword() throws Exception {
        String newValue = "pword";
        this.teiid.setJdbcPassword( this.uow, newValue );
        assertThat( this.teiid.getJdbcPassword( this.uow ), is( newValue ) );
    }
    
    @Test
    public void shouldSetJdbcSecure() throws Exception {
        boolean newValue = true;
        this.teiid.setJdbcSecure( this.uow, newValue );
        assertThat( this.teiid.isJdbcSecure( this.uow ), is( newValue ) );
    }
        
    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.teiid.getName( this.uow ), is( TEIID_NAME ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.teiid.getPrimaryType( this.uow ).getName(), is( KomodoLexicon.Teiid.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.teiid.getTypeIdentifier( this.uow ), is(KomodoType.TEIID));
    }

    @Test
    public void shouldHaveDefaultAdminSecureAfterConstruction() throws Exception {
        assertThat( this.teiid.isAdminSecure( this.uow ), is( TeiidAdminInfo.DEFAULT_SECURE ) );
    }

    @Test
    public void shouldHaveDefaultAdminUserAfterConstruction() throws Exception {
        assertThat( this.teiid.getAdminUser( this.uow ), is( TeiidAdminInfo.DEFAULT_ADMIN_USERNAME ) );
    }

    @Test
    public void shouldHaveDefaultAdminPasswordAfterConstruction() throws Exception {
        assertThat( this.teiid.getAdminPassword( this.uow ), is( TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD ) );
    }

    @Test
    public void shouldHaveDefaultAdminPortAfterConstruction() throws Exception {
        assertThat( this.teiid.getAdminPort( this.uow ), is( TeiidAdminInfo.DEFAULT_PORT ) );
    }

    @Test
    public void shouldHaveDefaultJdbcPasswordAfterConstruction() throws Exception {
        assertThat( this.teiid.getJdbcPassword( this.uow ), is( TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD ) );
    }
    
    @Test
    public void shouldHaveDefaultJdbcSecureAfterConstruction() throws Exception {
        assertThat( this.teiid.isJdbcSecure( this.uow ), is( TeiidJdbcInfo.DEFAULT_SECURE ) );
    }


}
