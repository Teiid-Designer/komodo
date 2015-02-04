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
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class TableConstraintTest extends RelationalModelTest {

    private static final String NAME = "tableConstraint";

    private TableConstraint constraint;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "table");
        this.constraint = RelationalModelFactory.createAccessPattern(null, _repo, this.table, NAME);
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final Column column = RelationalModelFactory.createColumn(null, _repo, mock(Table.class), "column");
        this.constraint.addColumn(null, column);

        assertThat(this.constraint.hasProperty(null, TeiidDdlLexicon.Constraint.REFERENCES), is(true));
        assertThat(this.constraint.getProperty(null, TeiidDdlLexicon.Constraint.REFERENCES).getValues(null).length, is(1));
        assertThat(this.constraint.getColumns(null).length, is(1));
    }

    @Test
    public void shouldHaveTableAfterConstruction() throws Exception {
        assertThat(this.constraint.getTable(null), is(this.table));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullColumn() throws Exception {
        this.constraint.addColumn(null, null);
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat(this.constraint.getColumns(null).length, is(0));
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final Column column = RelationalModelFactory.createColumn(null, _repo, mock(Table.class), "column");
        this.constraint.addColumn(null, column);
        this.constraint.removeColumn(null, column);

        assertThat(this.constraint.hasProperty(null, TeiidDdlLexicon.Constraint.REFERENCES), is(false));
    }

    @Test(expected = KException.class)
    public void shouldFailWhenRemovingColumnThatWasNeverAdded() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction("shouldFailWhenRemovingColumnThatWasNeverAdded", false, null);
        final Column column = RelationalModelFactory.createColumn(transaction, _repo, mock(Table.class), "column");
        this.constraint.removeColumn(transaction, column);
        transaction.commit();
    }

}
