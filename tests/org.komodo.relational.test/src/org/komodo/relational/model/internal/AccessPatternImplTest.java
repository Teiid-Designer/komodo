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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class AccessPatternImplTest extends RelationalModelTest {

    private static final String NAME = "accesspattern";

    private Table table;
    private AccessPattern modelObject;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, null, "table");
        this.modelObject = RelationalModelFactory.createAccessPattern(null, _repo, this.table, NAME);
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final UnitOfWork transaction = null; //_repo.createTransaction("shouldAddColumn", false, null);
        final Column column = RelationalModelFactory.createColumn(transaction, _repo, null, "column");
        this.modelObject.addColumn(transaction, column);

        assertThat(this.modelObject.hasProperty(null, TeiidDdlLexicon.Constraint.REFERENCES), is(true));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.Constraint.REFERENCES).getValues().length, is(1));
        assertThat(this.modelObject.getColumns(null).length, is(1));
    }

    @Test
    public void shouldHaveCorrectConstraintType() {
        assertThat(this.modelObject.getConstraintType(), is(TableConstraint.ConstraintType.ACCESS_PATTERN));
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldHaveTableAfterConstruction() throws Exception {
        assertThat(this.modelObject.getTable(null), is(this.table));
    }

}
