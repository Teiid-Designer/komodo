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
package org.komodo.relational.folder.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.model.Schema;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class FolderImplTest extends RelationalModelTest {

    private static final String FOLDER_NAME = "myFolder";

    protected Folder folder;

    @Before
    public void init() throws Exception {
        this.folder = createFolder( FOLDER_NAME );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.folder.getName( getTransaction() ), is( FOLDER_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.folder.getPropertyNames( getTransaction() );
        final String[] rawProps = this.folder.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.folder.getPropertyNames( getTransaction() );
        final Filter[] filters = this.folder.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.folder.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.Folder.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.folder.getTypeIdentifier( getTransaction() ), is(KomodoType.FOLDER));
    }

    @Test
    public void shouldAddFolder() throws Exception {
        final String name = "aFolder";
        final Folder anotherFolder = this.folder.addFolder(getTransaction(), name);

        assertThat( anotherFolder, is( notNullValue() ) );
        assertThat( anotherFolder.getName( getTransaction() ), is( name ) );
        assertThat( this.folder.getFolders( getTransaction() ).length, is( 1 ) );
        assertThat( this.folder.getChildren( getTransaction() )[0], is( instanceOf( Folder.class ) ) );

        assertThat( this.folder.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.folder.hasChild( getTransaction(), name, KomodoLexicon.Folder.NODE_TYPE ), is( true ) );
        assertThat( this.folder.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.folder.getChild( getTransaction(), name ), is( anotherFolder ) );
        assertThat( this.folder.getChild( getTransaction(), name, KomodoLexicon.Folder.NODE_TYPE ), is( anotherFolder ) );
    }

    @Test
    public void shouldAddVdb() throws Exception {
        final String name = "aVdb";
        final Vdb vdb = this.folder.addVdb(getTransaction(), name, "externalPath");

        assertThat( vdb, is( notNullValue() ) );
        assertThat( vdb.getName( getTransaction() ), is( name ) );
        assertThat( this.folder.getVdbs( getTransaction() ).length, is( 1 ) );
        assertThat( this.folder.getChildren( getTransaction() )[0], is( instanceOf( Vdb.class ) ) );

        assertThat( this.folder.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.folder.hasChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.folder.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.folder.getChild( getTransaction(), name ), is( vdb ) );
        assertThat( this.folder.getChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb ) );
    }

    @Test
    public void shouldAddSchema() throws Exception {
        final String name = "schema";
        final Schema schema = this.folder.addSchema(getTransaction(), name);

        assertThat( schema, is( notNullValue() ) );
        assertThat( schema.getName( getTransaction() ), is( name ) );
        assertThat( this.folder.getSchemas( getTransaction() ).length, is( 1 ) );
        assertThat( this.folder.getChildren( getTransaction() )[0], is( instanceOf( Schema.class ) ) );

        assertThat( this.folder.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.folder.hasChild( getTransaction(), name, KomodoLexicon.Schema.NODE_TYPE ), is( true ) );
        assertThat( this.folder.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.folder.getChild( getTransaction(), name ), is( schema ) );
        assertThat( this.folder.getChild( getTransaction(), name, KomodoLexicon.Schema.NODE_TYPE ), is( schema ) );
    }

    @Test
    public void shouldAddConnection() throws Exception {
        final String name = "connection";
        final Connection ds = this.folder.addConnection(getTransaction(), name);

        assertThat( ds, is( notNullValue() ) );
        assertThat( ds.getName( getTransaction() ), is( name ) );
        assertThat( this.folder.getConnections( getTransaction() ).length, is( 1 ) );
        assertThat( this.folder.getChildren( getTransaction() )[0], is( instanceOf( Connection.class ) ) );

        assertThat( this.folder.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.folder.hasChild( getTransaction(), name, DataVirtLexicon.Connection.NODE_TYPE ), is( true ) );
        assertThat( this.folder.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.folder.getChild( getTransaction(), name ), is( ds ) );
        assertThat( this.folder.getChild( getTransaction(), name, DataVirtLexicon.Connection.NODE_TYPE ), is( ds ) );
    }

    @Test
    public void shouldAddDataservice() throws Exception {
        final String name = "dataService";
        final Dataservice service = this.folder.addDataservice(getTransaction(), name);

        assertThat( service, is( notNullValue() ) );
        assertThat( service.getName( getTransaction() ), is( name ) );
        assertThat( this.folder.getDataservices( getTransaction() ).length, is( 1 ) );
        assertThat( this.folder.getChildren( getTransaction() )[0], is( instanceOf( Dataservice.class ) ) );

        assertThat( this.folder.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.folder.hasChild( getTransaction(), name, DataVirtLexicon.DataService.NODE_TYPE ), is( true ) );
        assertThat( this.folder.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.folder.getChild( getTransaction(), name ), is( service ) );
        assertThat( this.folder.getChild( getTransaction(), name, DataVirtLexicon.DataService.NODE_TYPE ), is( service ) );
    }

}
