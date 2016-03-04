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
        assertThat( teiidInstance.getUrl(), is( "mms://localhost:9999" ) );
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
        assertThat( this.teiid.getAdminPort( getTransaction() ), is( TeiidAdminInfo.DEFAULT_PORT ) );
    }

    @Test
    public void shouldHaveDefaultJdbcPasswordAfterConstruction() throws Exception {
        assertThat( this.teiid.getJdbcPassword( getTransaction() ), is( TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD ) );
    }
    
    @Test
    public void shouldHaveDefaultJdbcSecureAfterConstruction() throws Exception {
        assertThat( this.teiid.isJdbcSecure( getTransaction() ), is( TeiidJdbcInfo.DEFAULT_SECURE ) );
    }


}
