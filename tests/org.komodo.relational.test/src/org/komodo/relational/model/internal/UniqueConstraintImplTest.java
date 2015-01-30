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
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class UniqueConstraintImplTest extends RelationalModelTest {

    private static final String NAME = "uniqueconstraint";

    private Table table;
    private UniqueConstraint uniqueConstraint;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "table");
        this.uniqueConstraint = RelationalModelFactory.createUniqueConstraint(null, _repo, this.table, NAME);
    }

    @Test
    public void shouldFailConstructionIfNotUniqueConstraint() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new UniqueConstraintImpl(null, _repo, this.table.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat(this.uniqueConstraint.getConstraintType(), is(TableConstraint.ConstraintType.UNIQUE));
        assertThat(this.uniqueConstraint.getProperty(null, TeiidDdlLexicon.Constraint.TYPE).getStringValue(null),
                   is(TableConstraint.ConstraintType.UNIQUE.toString()));
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.uniqueConstraint.hasDescriptor(null, TeiidDdlLexicon.Constraint.TABLE_ELEMENT), is(true));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.uniqueConstraint.getTable(null), is(this.table));
    }

}
