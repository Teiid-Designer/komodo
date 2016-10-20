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
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItem;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataRoleImplTest extends RelationalModelTest {

    private DataRole dataRole;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.dataRole = vdb.addDataRole( getTransaction(), "dataRole" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotDataRole() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new DataRoleImpl( getTransaction(), _repo, this.dataRole.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldAddMappedRole() throws Exception {
        final String name = "role";
        final String[] mappedRoles = this.dataRole.addMappedRole( getTransaction(), name );

        assertThat( mappedRoles, is( notNullValue() ) );
        assertThat( this.dataRole.getMappedRoles( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataRole.getMappedRoles( getTransaction() )[0], is( name ) );
    }

    @Test
    public void shouldAddMultipleMappedRoles() throws Exception {
        this.dataRole.addMappedRole( getTransaction(), "roleA" );
        this.dataRole.addMappedRole( getTransaction(), "roleB" );

        assertThat( this.dataRole.getMappedRoles( getTransaction() ).length, is( 2 ) );
    }

    @Test
    public void shouldAddPermission() throws Exception {
        final String name = "permission";
        final Permission permission = this.dataRole.addPermission( getTransaction(), name );
        assertThat( permission, is( notNullValue() ) );
        assertThat( this.dataRole.getPermissions( getTransaction() ).length, is( 1 ) );

        final Permission added = this.dataRole.getPermissions( getTransaction() )[0];
        assertThat( added, is( permission ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.PERMISSION ) );
        assertThat( this.dataRole.getChildren( getTransaction() )[0], is( instanceOf( Permission.class ) ) );

        assertThat( this.dataRole.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.dataRole.hasChild( getTransaction(), name, VdbLexicon.DataRole.Permission.PERMISSION ), is( true ) );
        assertThat( this.dataRole.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataRole.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.dataRole.getChild( getTransaction(), name, VdbLexicon.DataRole.Permission.PERMISSION ), is( added ) );
    }

    @Test( expected = KException.class )
    public void shouldFailGetChildWhenTypeIsWrong() throws Exception {
        final String name = "permission";
        this.dataRole.addPermission( getTransaction(), name );
        this.dataRole.getChild( getTransaction(), name, "bogusType" );
    }

    @Test( expected = KException.class )
    public void shouldFailWhenChildNotFound() throws Exception {
        this.dataRole.getChild( getTransaction(), "bogus" );
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.dataRole.getChildTypes() ), hasItem( Permission.IDENTIFIER ) );
        assertThat( this.dataRole.getChildTypes().length, is( 1 ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataRole.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.DATA_ROLE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataRole.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_DATA_ROLE));
    }

    @Test
    public void shouldHaveDefaultAllowCreateTempTablesValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isAllowCreateTempTables( getTransaction() ), is( DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES ) );
    }

    @Test
    public void shouldHaveDefaultAnyAuthenticatedValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isAnyAuthenticated( getTransaction() ), is( DataRole.DEFAULT_ANY_AUTHENTICATED ) );
    }

    @Test
    public void shouldHaveDefaultGrantAllValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isGrantAll( getTransaction() ), is( DataRole.DEFAULT_GRANT_ALL ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataRole.getPropertyNames( getTransaction() );
        final String[] rawProps = this.dataRole.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.dataRole.getParent( getTransaction() ), is( instanceOf( Vdb.class ) ) );
    }

    @Test
    public void shouldHaveStrongTypedChildren() throws Exception {
        this.dataRole.addPermission( getTransaction(), "permission" );
        assertThat( this.dataRole.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataRole.getChildren( getTransaction() )[0], is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyMappedRole() throws Exception {
        this.dataRole.addMappedRole( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyPermission() throws Exception {
        this.dataRole.addPermission( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullMappedRole() throws Exception {
        this.dataRole.addMappedRole( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullPermission() throws Exception {
        this.dataRole.addPermission( getTransaction(), null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataRole.getPropertyNames( getTransaction() );
        final Filter[] filters = this.dataRole.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.dataRole.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveMappedRolesAfterConstruction() throws Exception {
        assertThat( this.dataRole.getMappedRoles( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataRole.getMappedRoles( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHavePermissionsAfterConstruction() throws Exception {
        assertThat( this.dataRole.getPermissions( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataRole.getPermissions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveMappedRole() throws Exception {
        final String name = "role";
        this.dataRole.addMappedRole( getTransaction(), name );
        assertThat( this.dataRole.getMappedRoles( getTransaction() ).length, is( 1 ) );

        this.dataRole.removeMappedRole( getTransaction(), name );
        assertThat( this.dataRole.getMappedRoles( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePermission() throws Exception {
        final String name = "permission";
        this.dataRole.addPermission( getTransaction(), name );
        assertThat( this.dataRole.getPermissions( getTransaction() ).length, is( 1 ) );

        this.dataRole.removePermission( getTransaction(), name );
        assertThat( this.dataRole.getPermissions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.dataRole.rename( getTransaction(), newName );
        assertThat( this.dataRole.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetAllowCreateTempTablesValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
        this.dataRole.setAllowCreateTempTables( getTransaction(), newValue );
        assertThat( this.dataRole.isAllowCreateTempTables( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetAnyAuthenticatedValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_ANY_AUTHENTICATED;
        this.dataRole.setAnyAuthenticated( getTransaction(), newValue );
        assertThat( this.dataRole.isAnyAuthenticated( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.dataRole.setDescription( getTransaction(), newValue );
        assertThat( this.dataRole.getDescription( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetGrantAllValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_GRANT_ALL;
        this.dataRole.setGrantAll( getTransaction(), newValue );
        assertThat( this.dataRole.isGrantAll( getTransaction() ), is( newValue ) );
    }

}
