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
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.response.RestVdbDataRole;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDataRoleTest {

    private static final String NAME = "MyDataRole";
    private static final String[] MAPPED_ROLES = { "sledge", "hammer" };

    private RestVdbDataRole dataRole;

    @Before
    public void init() {
        this.dataRole = new RestVdbDataRole();
        this.dataRole.setName(NAME);
        this.dataRole.setMappedRoles( MAPPED_ROLES );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
        thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( this.dataRole.getMappedRoles() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole, is( thatDataRole ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyEntries() {
        final RestVdbDataRole empty1 = new RestVdbDataRole();
        final RestVdbDataRole empty2 = new RestVdbDataRole();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyDataRole() {
        final RestVdbDataRole empty = new RestVdbDataRole();
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getMappedRoles().length, is( 0 ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().size(), is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
         thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( this.dataRole.getMappedRoles() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole.hashCode(), is( thatDataRole.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowCreateTempTablesIsDifferent() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
         thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( !this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( this.dataRole.getMappedRoles() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole.isAllowCreateTempTables(), is( not( thatDataRole.isAllowCreateTempTables() ) ) );
        assertThat( this.dataRole, is( not( thatDataRole ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAnyAuthenticatedIsDifferent() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
         thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( !this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( this.dataRole.getMappedRoles() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole.isAnyAuthenticated(), is( not( thatDataRole.isAnyAuthenticated() ) ) );
        assertThat( this.dataRole, is( not( thatDataRole ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenGrantAllIsDifferent() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
         thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( !this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( this.dataRole.getMappedRoles() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole.isGrantAll(), is( not( thatDataRole.isGrantAll() ) ) );
        assertThat( this.dataRole, is( not( thatDataRole ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenMappedRolesAreDifferent() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
         thatDataRole.setName(this.dataRole.getName() );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setMappedRoles( new String[] { "red", "white", "blue" } );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( Arrays.deepEquals( this.dataRole.getMappedRoles(), thatDataRole.getMappedRoles() ), is( false ) );
        assertThat( this.dataRole, is( not( thatDataRole ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbDataRole thatDataRole = new RestVdbDataRole();
        thatDataRole.setName(this.dataRole.getName() + "blah" );
        thatDataRole.setAllowCreateTempTables( this.dataRole.isAllowCreateTempTables() );
        thatDataRole.setAnyAuthenticated( this.dataRole.isAnyAuthenticated() );
        thatDataRole.setGrantAll( this.dataRole.isGrantAll() );
        thatDataRole.setLinks( this.dataRole.getLinks() );
        thatDataRole.setProperties( this.dataRole.getProperties() );

        assertThat( this.dataRole.getName(), is( not( thatDataRole.getName() ) ) );
        assertThat( this.dataRole, is( not( thatDataRole ) ) );
    }

    @Test
    public void shouldSetAllowCreateTempTables() {
        final boolean newValue = !this.dataRole.isAllowCreateTempTables();
        this.dataRole.setAllowCreateTempTables( newValue );
        assertThat( this.dataRole.isAllowCreateTempTables(), is( newValue ) );
    }

    @Test
    public void shouldSetAnyAuthenticated() {
        final boolean newValue = !this.dataRole.isAnyAuthenticated();
        this.dataRole.setAnyAuthenticated( newValue );
        assertThat( this.dataRole.isAnyAuthenticated(), is( newValue ) );
    }

    @Test
    public void shouldSetGrantAll() {
        final boolean newValue = !this.dataRole.isGrantAll();
        this.dataRole.setGrantAll( newValue );
        assertThat( this.dataRole.isGrantAll(), is( newValue ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.dataRole.setName( newName );
        assertThat( this.dataRole.getName(), is( newName ) );
    }

}
