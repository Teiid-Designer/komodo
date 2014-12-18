/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.RelationalModelFactory;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class IndexImplTest extends RelationalModelTest {

    private static final String NAME = "index";

    private Table parentTable;
    private Index modelObject;

    @Before
    public void init() throws Exception {
        this.parentTable = RelationalModelFactory.createTable(null, _repo, null, "parentTable");
        this.modelObject = RelationalModelFactory.createIndex(null, _repo, this.parentTable, NAME);
    }

    @Test
    public void shouldAddColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, null, "columnA");
        this.modelObject.addColumn(null, columnA);

        final Column columnB = RelationalModelFactory.createColumn(null, _repo, null, "columnB");
        this.modelObject.addColumn(null, columnB);

        assertThat(this.modelObject.getColumns(null).length, is(2));
        assertThat(Arrays.asList(this.modelObject.getColumns(null)), hasItems(columnA, columnB));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.modelObject.getTable(null), is(this.parentTable));
    }

    @Test
    public void shouldAllowEmptyExpression() throws Exception {
        this.modelObject.setExpression(null, "");
        assertThat(this.modelObject.getExpression(null), is(nullValue()));
    }

    @Test
    public void shouldAllowNullExpression() throws Exception {
        this.modelObject.setExpression(null, null);
        assertThat(this.modelObject.getExpression(null), is(nullValue()));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumn() throws Exception {
        this.modelObject.addColumn(null, null);
    }

    @Test
    public void shouldHaveCorrectConstraintType() {
        assertThat(this.modelObject.getConstraintType(), is(TableConstraint.ConstraintType.INDEX));
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat(this.modelObject.getColumns(null), is(notNullValue()));
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldNotHaveExpressionAfterConstruction() throws Exception {
        assertThat(this.modelObject.getExpression(null), is(nullValue()));
        assertThat(this.modelObject.hasProperty(null, TeiidDdlLexicon.Constraint.EXPRESSION), is(false));
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, null, "removeColumnA");
        this.modelObject.addColumn(null, columnA);
        this.modelObject.removeColumn(null, columnA);
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldSetExpression() throws Exception {
        final String value = "expression";
        this.modelObject.setExpression(null, value);
        assertThat(this.modelObject.getExpression(null), is(value));
    }

}
