/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.RelationalObject.Filter;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataRoleImplTest extends RelationalModelTest {

    private DataRole dataRole;
    private Vdb vdb;

    @Before
    public void init() throws Exception {
        this.vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb" );
        this.dataRole = RelationalModelFactory.createDataRole( this.uow, _repo, this.vdb, "dataRole" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotDataRole() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new DataRoleImpl( this.uow, _repo, this.vdb.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldAddMappedRole() throws Exception {
        final String name = "role";
        final String[] mappedRoles = this.dataRole.addMappedRole( this.uow, name );

        assertThat( mappedRoles, is( notNullValue() ) );
        assertThat( this.dataRole.getMappedRoles( this.uow ).length, is( 1 ) );
        assertThat( this.dataRole.getMappedRoles( this.uow )[0], is( name ) );
    }

    @Test
    public void shouldAddMultipleMappedRoles() throws Exception {
        this.dataRole.addMappedRole( this.uow, "roleA" );
        this.dataRole.addMappedRole( this.uow, "roleB" );

        assertThat( this.dataRole.getMappedRoles( this.uow ).length, is( 2 ) );
    }

    @Test
    public void shouldAddPermission() throws Exception {
        final String name = "permission";
        final Permission permission = this.dataRole.addPermission( this.uow, name );
        assertThat( permission, is( notNullValue() ) );
        assertThat( this.dataRole.getPermissions( this.uow ).length, is( 1 ) );

        final Permission added = this.dataRole.getPermissions( this.uow )[0];
        assertThat( added, is( permission ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.DataRole.Permission.PERMISSION ) );
        assertThat( this.dataRole.getChildren( this.uow )[0], is( instanceOf( Permission.class ) ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataRole.getPrimaryType( this.uow ).getName(), is( VdbLexicon.DataRole.DATA_ROLE ) );
    }

    @Test
    public void shouldHaveDefaultAllowCreateTempTablesValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isAllowCreateTempTables( this.uow ), is( DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES ) );
    }

    @Test
    public void shouldHaveDefaultAnyAuthenticatedValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isAnyAuthenticated( this.uow ), is( DataRole.DEFAULT_ANY_AUTHENTICATED ) );
    }

    @Test
    public void shouldHaveDefaultGrantAllValueAfterConstruction() throws Exception {
        assertThat( this.dataRole.isGrantAll( this.uow ), is( DataRole.DEFAULT_GRANT_ALL ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataRole.getPropertyNames( this.uow );
        final String[] rawProps = this.dataRole.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.dataRole.getParent( this.uow ), is( instanceOf( Vdb.class ) ) );
    }

    @Test
    public void shouldHaveStrongTypedChildren() throws Exception {
        this.dataRole.addPermission( this.uow, "permission" );
        assertThat( this.dataRole.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.dataRole.getChildren( this.uow )[0], is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyMappedRole() throws Exception {
        this.dataRole.addMappedRole( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyPermission() throws Exception {
        this.dataRole.addPermission( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullMappedRole() throws Exception {
        this.dataRole.addMappedRole( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullPermission() throws Exception {
        this.dataRole.addPermission( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataRole.getPropertyNames( this.uow );
        final Filter[] filters = this.dataRole.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.dataRole.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveMappedRolesAfterConstruction() throws Exception {
        assertThat( this.dataRole.getMappedRoles( this.uow ), is( notNullValue() ) );
        assertThat( this.dataRole.getMappedRoles( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHavePermissionsAfterConstruction() throws Exception {
        assertThat( this.dataRole.getPermissions( this.uow ), is( notNullValue() ) );
        assertThat( this.dataRole.getPermissions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveMappedRole() throws Exception {
        final String name = "role";
        this.dataRole.addMappedRole( this.uow, name );
        assertThat( this.dataRole.getMappedRoles( this.uow ).length, is( 1 ) );

        this.dataRole.removeMappedRole( this.uow, name );
        assertThat( this.dataRole.getMappedRoles( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePermission() throws Exception {
        final String name = "permission";
        this.dataRole.addPermission( this.uow, name );
        assertThat( this.dataRole.getPermissions( this.uow ).length, is( 1 ) );

        this.dataRole.removePermission( this.uow, name );
        assertThat( this.dataRole.getPermissions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetAllowCreateTempTablesValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
        this.dataRole.setAllowCreateTempTables( this.uow, newValue );
        assertThat( this.dataRole.isAllowCreateTempTables( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetAnyAuthenticatedValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_ANY_AUTHENTICATED;
        this.dataRole.setAnyAuthenticated( this.uow, newValue );
        assertThat( this.dataRole.isAnyAuthenticated( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.dataRole.setDescription( this.uow, newValue );
        assertThat( this.dataRole.getDescription( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetGrantAllValue() throws Exception {
        final boolean newValue = !DataRole.DEFAULT_GRANT_ALL;
        this.dataRole.setGrantAll( this.uow, newValue );
        assertThat( this.dataRole.isGrantAll( this.uow ), is( newValue ) );
    }

}
