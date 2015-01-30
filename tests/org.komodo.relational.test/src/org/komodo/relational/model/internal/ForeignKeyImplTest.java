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
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

@SuppressWarnings( {"javadoc", "nls"} )
public class ForeignKeyImplTest extends RelationalModelTest {

    private static final String NAME = "foreignKey";

    private ForeignKey foreignKey;
    private Table parentTable;
    private Table refTable;

    @Before
    public void init() throws Exception {
        this.parentTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "parentTable");
        this.refTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "refTable");
        this.foreignKey = RelationalModelFactory.createForeignKey(null, _repo, this.parentTable, NAME, this.refTable);
    }

    @Test
    public void shouldAddReferencesColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, this.refTable, "columnRefA");
        this.foreignKey.addReferencesColumn(null, columnA);

        final Column columnB = RelationalModelFactory.createColumn(null, _repo, this.refTable, "columnRefB");
        this.foreignKey.addReferencesColumn(null, columnB);

        assertThat(this.foreignKey.getReferencesColumns(null).length, is(2));
        assertThat(Arrays.asList(this.foreignKey.getReferencesColumns(null)), hasItems(columnA, columnB));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumn() throws Exception {
        this.foreignKey.addColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullReferencesColumn() throws Exception {
        this.foreignKey.addReferencesColumn(null, null);
    }

    @Test
    public void shouldFailConstructionIfNotForeignKey() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ForeignKeyImpl(null, _repo, this.parentTable.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat(this.foreignKey.getConstraintType(), is(TableConstraint.ConstraintType.FOREIGN_KEY));
        assertThat(this.foreignKey.getProperty(null, TeiidDdlLexicon.Constraint.TYPE).getStringValue(null),
                   is(TableConstraint.ConstraintType.FOREIGN_KEY.toString()));
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.foreignKey.hasDescriptor(null, Constraint.FOREIGN_KEY_CONSTRAINT), is(true));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.foreignKey.getTable(null), is(this.parentTable));
    }

    @Test
    public void shouldHaveReferencesTableAfterConstruction() throws Exception {
        assertThat(this.foreignKey.getReferencesTable(null), is(this.refTable));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullTableReference() throws Exception {
        this.foreignKey.setReferencesTable(null, null);
    }

    @Test
    public void shouldNotHaveColumnReferencesAfterConstruction() throws Exception {
        assertThat(this.foreignKey.getReferencesColumns(null), is(notNullValue()));
        assertThat(this.foreignKey.getReferencesColumns(null).length, is(0));
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat(this.foreignKey.getColumns(null), is(notNullValue()));
        assertThat(this.foreignKey.getColumns(null).length, is(0));
    }

    @Test
    public void shouldRemoveReferencesColumn() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, this.refTable, "removeRefColumnA");
        this.foreignKey.addReferencesColumn(null, columnA);
        this.foreignKey.removeReferencesColumn(null, columnA);
        assertThat(this.foreignKey.getReferencesColumns(null).length, is(0));
    }

    @Test
    public void shouldSetTableReference() throws Exception {
        final Table newTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "newTable");
        this.foreignKey.setReferencesTable(null, newTable);

        assertThat(this.foreignKey.getReferencesTable(null), is(newTable));
    }

}
