/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class DataRoleImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private DataRole dataRole;
    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.dataRole = RelationalModelFactory.createDataRole(transaction, _repo, this.vdb, ("dataRole" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.dataRole.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.DATA_ROLE));
    }

    @Test
    public void shouldAddMappedRole() throws Exception {
        create();
        final String name = ("role" + _counter);
        final String[] mappedRoles = this.dataRole.addMappedRole(null, name);

        assertThat(mappedRoles, is(notNullValue()));
        assertThat(this.dataRole.getMappedRoles(null).length, is(1));
        assertThat(this.dataRole.getMappedRoles(null)[0], is(name));
    }

    @Test
    public void shouldAddPermission() throws Exception {
        create();

        final String name = ("permission" + _counter);
        final Permission permission = this.dataRole.addPermission(null, name);
        assertThat(permission, is(notNullValue()));
        assertThat(this.dataRole.getPermissions(null).length, is(1));

        final Permission added = this.dataRole.getPermissions(null)[0];
        assertThat(added, is(permission));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.PERMISSION));
    }

    @Test
    public void shouldHaveDefaultAllowCreateTempTablesValueAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.isAllowCreateTempTables(null), is(DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES));
    }

    @Test
    public void shouldHaveDefaultAnyAuthenticatedValueAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.isAnyAuthenticated(null), is(DataRole.DEFAULT_ANY_AUTHENTICATED));
    }

    @Test
    public void shouldHaveDefaultGrantAllValueAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.isGrantAll(null), is(DataRole.DEFAULT_GRANT_ALL));
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyMappedRole() throws Exception {
        create();
        this.dataRole.addMappedRole(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyPermission() throws Exception {
        create();
        this.dataRole.addPermission(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullMappedRole() throws Exception {
        create();
        this.dataRole.addMappedRole(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullPermission() throws Exception {
        create();
        this.dataRole.addPermission(null, null);
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.getDescription(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveMappedRolesAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.getMappedRoles(null), is(notNullValue()));
        assertThat(this.dataRole.getMappedRoles(null).length, is(0));
    }

    @Test
    public void shouldNotHavePermissionsAfterConstruction() throws Exception {
        create();
        assertThat(this.dataRole.getPermissions(null), is(notNullValue()));
        assertThat(this.dataRole.getPermissions(null).length, is(0));
    }

    @Test
    public void shouldRemoveMappedRole() throws Exception {
        create();

        final String name = ("role" + _counter);
        this.dataRole.addMappedRole(null, name);
        assertThat(this.dataRole.getMappedRoles(null).length, is(1));

        this.dataRole.removeMappedRole(null, name);
        assertThat(this.dataRole.getMappedRoles(null).length, is(0));
    }

    @Test
    public void shouldRemovePermission() throws Exception {
        create();

        final String name = ("permission" + _counter);
        this.dataRole.addPermission(null, name);
        assertThat(this.dataRole.getPermissions(null).length, is(1));

        this.dataRole.removePermission(null, name);
        assertThat(this.dataRole.getPermissions(null).length, is(0));
    }

    @Test
    public void shouldSetAllowCreateTempTablesValue() throws Exception {
        create();
        final boolean newValue = !DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
        this.dataRole.setAllowCreateTempTables(null, newValue);
        assertThat(this.dataRole.isAllowCreateTempTables(null), is(newValue));
    }

    @Test
    public void shouldSetAnyAuthenticatedValue() throws Exception {
        create();
        final boolean newValue = !DataRole.DEFAULT_ANY_AUTHENTICATED;
        this.dataRole.setAnyAuthenticated(null, newValue);
        assertThat(this.dataRole.isAnyAuthenticated(null), is(newValue));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        create();
        final String newValue = "newDescription";
        this.dataRole.setDescription(null, newValue);
        assertThat(this.dataRole.getDescription(null), is(newValue));
    }

    @Test
    public void shouldSetGrantAllValue() throws Exception {
        create();
        final boolean newValue = !DataRole.DEFAULT_GRANT_ALL;
        this.dataRole.setGrantAll(null, newValue);
        assertThat(this.dataRole.isGrantAll(null), is(newValue));
    }

}
