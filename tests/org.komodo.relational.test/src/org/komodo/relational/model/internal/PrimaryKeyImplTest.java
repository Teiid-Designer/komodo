/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class PrimaryKeyImplTest extends RelationalModelTest {

    private static final String NAME = "primaryKey";

    private PrimaryKey primaryKey;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "table");
        this.primaryKey = RelationalModelFactory.createPrimaryKey(null, _repo, this.table, NAME);
    }

    @Test
    public void shouldFailConstructionIfNotPrimaryKey() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new PrimaryKeyImpl(null, _repo, this.table.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat(this.primaryKey.getConstraintType(), is(TableConstraint.ConstraintType.PRIMARY_KEY));
        assertThat(this.primaryKey.getProperty(null, TeiidDdlLexicon.Constraint.TYPE).getStringValue(null),
                   is(TableConstraint.ConstraintType.PRIMARY_KEY.toString()));
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.primaryKey.hasDescriptor(null, TeiidDdlLexicon.Constraint.TABLE_ELEMENT), is(true));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat(this.primaryKey.getName(null), is(NAME));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.primaryKey.getTable(null), is(this.table));
    }

}
