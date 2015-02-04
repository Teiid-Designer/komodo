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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class IndexImplTest extends RelationalModelTest {

    private static final String NAME = "index";

    private Index index;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "table");
        this.index = RelationalModelFactory.createIndex(null, _repo, this.table, NAME);
    }

    @Test
    public void shouldFailConstructionIfNotIndex() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new IndexImpl(null, _repo, this.table.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldAddColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, mock(Table.class), "columnA");
        this.index.addColumn(null, columnA);

        final Column columnB = RelationalModelFactory.createColumn(null, _repo, mock(Table.class), "columnB");
        this.index.addColumn(null, columnB);

        assertThat(this.index.getColumns(null).length, is(2));
        assertThat(Arrays.asList(this.index.getColumns(null)), hasItems(columnA, columnB));
    }

    @Test
    public void shouldAllowEmptyExpression() throws Exception {
        this.index.setExpression(null, "");
        assertThat(this.index.getExpression(null), is(nullValue()));
    }

    @Test
    public void shouldAllowNullExpression() throws Exception {
        this.index.setExpression(null, null);
        assertThat(this.index.getExpression(null), is(nullValue()));
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat(this.index.getConstraintType(), is(TableConstraint.ConstraintType.INDEX));
        assertThat(this.index.getProperty(null, TeiidDdlLexicon.Constraint.TYPE).getStringValue(null),
                   is(TableConstraint.ConstraintType.INDEX.toString()));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.index.getTable(null), is(this.table));
    }

    @Test
    public void shouldNotHaveExpressionAfterConstruction() throws Exception {
        assertThat(this.index.getExpression(null), is(nullValue()));
        assertThat(this.index.hasProperty(null, TeiidDdlLexicon.Constraint.EXPRESSION), is(false));
    }

    @Test
    public void shouldRemoveExpressionWithEmptyString() throws Exception {
        this.index.setExpression(null, "expression");
        this.index.setExpression(null, StringConstants.EMPTY_STRING);
        assertThat(this.index.getExpression(null), is(nullValue()));
    }

    @Test
    public void shouldRemoveExpressionWithNull() throws Exception {
        this.index.setExpression(null, "expression");
        this.index.setExpression(null, null);
        assertThat(this.index.getExpression(null), is(nullValue()));
    }

    @Test
    public void shouldSetExpression() throws Exception {
        final String value = "expression";
        this.index.setExpression(null, value);
        assertThat(this.index.getExpression(null), is(value));
    }

}
