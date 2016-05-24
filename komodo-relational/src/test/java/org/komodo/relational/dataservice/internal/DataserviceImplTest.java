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
package org.komodo.relational.dataservice.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceImplTest extends RelationalModelTest {

    private static final String SERVICE_NAME = "myService";

    protected Dataservice dataservice;

    @Before
    public void init() throws Exception {
        this.dataservice = createDataservice( SERVICE_NAME );
    }

    @Test
    public void shouldHaveName() throws Exception {
        assertThat( this.dataservice.getName( getTransaction() ), is( SERVICE_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final String[] rawProps = this.dataservice.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final Filter[] filters = this.dataservice.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }
    
    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataservice.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.DataService.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataservice.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASERVICE));
    }
    
    @Test
    public void shouldAddVdb() throws Exception {
        final String name = "childVdb";
        final Vdb vdb = this.dataservice.addVdb(getTransaction(), name, "externalPath");
        
        assertThat( vdb, is( notNullValue() ) );
        assertThat( vdb.getName( getTransaction() ), is( name ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction() )[0], is( instanceOf( Vdb.class ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChild( getTransaction(), name ), is( vdb ) );
        assertThat( this.dataservice.getChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb ) );
    }

    @Test
    public void shouldGetVdbs() throws Exception {
        final String name1 = "childVdb1";
        final String name2 = "childVdb2";
        final Vdb vdb1 = this.dataservice.addVdb(getTransaction(), name1, "externalPath1");
        final Vdb vdb2 = this.dataservice.addVdb(getTransaction(), name2, "externalPath1");
        
        assertThat( vdb1, is( notNullValue() ) );
        assertThat( vdb2, is( notNullValue() ) );
        assertThat( vdb1.getName( getTransaction() ), is( name1 ) );
        assertThat( vdb2.getName( getTransaction() ), is( name2 ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 2 ) );
        assertThat( this.dataservice.getChildren( getTransaction() )[1], is( instanceOf( Vdb.class ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), name1 ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name2 ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name1, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name2, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChild( getTransaction(), name1 ), is( vdb1 ) );
        assertThat( this.dataservice.getChild( getTransaction(), name2 ), is( vdb2 ) );
        assertThat( this.dataservice.getChild( getTransaction(), name1, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb1) );
        assertThat( this.dataservice.getChild( getTransaction(), name2, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb2) );
    }

    @Test
    public void shouldExport() throws Exception {
        // TBD. 
    }

}
