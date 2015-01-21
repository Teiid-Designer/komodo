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
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class PermissionImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private DataRole dataRole;
    private Permission permission;
    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.dataRole = RelationalModelFactory.createDataRole(transaction, _repo, this.vdb, ("dataRole" + suffix));
        this.permission = RelationalModelFactory.createPermission(transaction, _repo, this.dataRole, ("permission" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.dataRole.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.DATA_ROLE));
        assertThat(this.permission.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.PERMISSION));
    }

    @Test
    public void shouldAddCondition() throws Exception {
        create();

        final String name = ("condition" + _counter);
        final Condition condition = this.permission.addCondition(null, name);
        assertThat(condition, is(notNullValue()));
        assertThat(this.permission.getConditions(null).length, is(1));

        final Condition added = this.permission.getConditions(null)[0];
        assertThat(added, is(condition));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.Condition.CONDITION));
    }

    @Test
    public void shouldAddMask() throws Exception {
        create();

        final String name = ("mask" + _counter);
        final Mask mask = this.permission.addMask(null, name);
        assertThat(mask, is(notNullValue()));
        assertThat(this.permission.getMasks(null).length, is(1));

        final Mask added = this.permission.getMasks(null)[0];
        assertThat(added, is(mask));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.Mask.MASK));
    }

    @Test
    public void shouldHaveDefaultAllowAlterValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowAlter(null), is(Permission.DEFAULT_ALLOW_ALTER));
    }

    @Test
    public void shouldHaveDefaultAllowCreateValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowCreate(null), is(Permission.DEFAULT_ALLOW_CREATE));
    }

    @Test
    public void shouldHaveDefaultAllowDeleteValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowDelete(null), is(Permission.DEFAULT_ALLOW_DELETE));
    }

    @Test
    public void shouldHaveDefaultAllowExecuteValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowExecute(null), is(Permission.DEFAULT_ALLOW_EXECUTE));
    }

    @Test
    public void shouldHaveDefaultAllowLanguageValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowLanguage(null), is(Permission.DEFAULT_ALLOW_LANGUAGE));
    }

    @Test
    public void shouldHaveDefaultAllowReadValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowRead(null), is(Permission.DEFAULT_ALLOW_READ));
    }

    @Test
    public void shouldHaveDefaultAllowUpdateValueAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.isAllowUpdate(null), is(Permission.DEFAULT_ALLOW_UPDATE));
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyCondition() throws Exception {
        create();
        this.permission.addCondition(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyMask() throws Exception {
        create();
        this.permission.addMask(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullCondition() throws Exception {
        create();
        this.permission.addCondition(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullMask() throws Exception {
        create();
        this.permission.addMask(null, null);
    }

    @Test
    public void shouldNotHaveConditionsAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.getConditions(null), is(notNullValue()));
        assertThat(this.permission.getConditions(null).length, is(0));
    }

    @Test
    public void shouldNotHaveMakssAfterConstruction() throws Exception {
        create();
        assertThat(this.permission.getMasks(null), is(notNullValue()));
        assertThat(this.permission.getMasks(null).length, is(0));
    }

    @Test
    public void shouldRemoveCondition() throws Exception {
        create();

        final String name = ("condition" + _counter);
        this.permission.addCondition(null, name);
        assertThat(this.permission.getConditions(null).length, is(1));

        this.permission.removeCondition(null, name);
        assertThat(this.permission.getConditions(null).length, is(0));
    }

    @Test
    public void shouldRemoveMask() throws Exception {
        create();

        final String name = ("mask" + _counter);
        this.permission.addMask(null, name);
        assertThat(this.permission.getMasks(null).length, is(1));

        this.permission.removeMask(null, name);
        assertThat(this.permission.getMasks(null).length, is(0));
    }

    @Test
    public void shouldSetAllowAlterValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_ALTER;
        this.permission.setAllowAlter(null, newValue);
        assertThat(this.permission.isAllowAlter(null), is(newValue));
    }

    @Test
    public void shouldSetAllowCreateValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_CREATE;
        this.permission.setAllowCreate(null, newValue);
        assertThat(this.permission.isAllowCreate(null), is(newValue));
    }

    @Test
    public void shouldSetAllowDeleteValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_DELETE;
        this.permission.setAllowDelete(null, newValue);
        assertThat(this.permission.isAllowDelete(null), is(newValue));
    }

    @Test
    public void shouldSetAllowExecuteValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_EXECUTE;
        this.permission.setAllowExecute(null, newValue);
        assertThat(this.permission.isAllowExecute(null), is(newValue));
    }

    @Test
    public void shouldSetAllowLanguageValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_LANGUAGE;
        this.permission.setAllowLanguage(null, newValue);
        assertThat(this.permission.isAllowLanguage(null), is(newValue));
    }

    @Test
    public void shouldSetAllowReadValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_READ;
        this.permission.setAllowRead(null, newValue);
        assertThat(this.permission.isAllowRead(null), is(newValue));
    }

    @Test
    public void shouldSetAllowUpdateValue() throws Exception {
        create();
        final boolean newValue = !Permission.DEFAULT_ALLOW_UPDATE;
        this.permission.setAllowUpdate(null, newValue);
        assertThat(this.permission.isAllowUpdate(null), is(newValue));
    }

}
