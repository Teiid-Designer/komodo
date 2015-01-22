/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ConditionImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private Condition condition;
    private DataRole dataRole;
    private Permission permission;
    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.dataRole = RelationalModelFactory.createDataRole(transaction, _repo, this.vdb, ("dataRole" + suffix));
        this.permission = RelationalModelFactory.createPermission(transaction, _repo, this.dataRole, ("permission" + suffix));
        this.condition = RelationalModelFactory.createCondition(transaction, _repo, this.permission, ("condition" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.dataRole.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.DATA_ROLE));
        assertThat(this.permission.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.PERMISSION));
        assertThat(this.condition.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.Condition.CONDITION));
    }

    @Test
    public void shouldHaveConstraintDefaultValueAfterConstruction() throws Exception {
        create();
        assertThat(this.condition.isConstraint(null), is(Condition.DEFAULT_CONSTRAINT));
    }

    @Test
    public void shouldSetConstraintValue() throws Exception {
        create();
        final boolean newValue = !Condition.DEFAULT_CONSTRAINT;
        this.condition.setConstraint(null, newValue);
        assertThat(this.condition.isConstraint(null), is(newValue));
    }

}
