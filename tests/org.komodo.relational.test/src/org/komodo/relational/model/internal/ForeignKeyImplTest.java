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
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;

@SuppressWarnings( {"javadoc", "nls"} )
public class ForeignKeyImplTest extends RelationalModelTest {

    private static final String NAME = "foreignKey";

    private ForeignKey modelObject;
    private Table parentTable;
    private Table refTable;

    @Before
    public void init() throws Exception {
        this.parentTable = RelationalModelFactory.createTable(null, _repo, null, "parentTable");
        this.refTable = RelationalModelFactory.createTable(null, _repo, null, "refTable");
        this.modelObject = RelationalModelFactory.createForeignKey(null, _repo, parentTable, NAME, refTable);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumn() throws Exception {
        this.modelObject.addColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullReferencesColumn() throws Exception {
        this.modelObject.addReferencesColumn(null, null);
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
    public void shouldAddReferencesColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, null, "columnRefA");
        this.modelObject.addReferencesColumn(null, columnA);

        final Column columnB = RelationalModelFactory.createColumn(null, _repo, null, "columnRefB");
        this.modelObject.addReferencesColumn(null, columnB);

        assertThat(this.modelObject.getReferencesColumns(null).length, is(2));
        assertThat(Arrays.asList(this.modelObject.getReferencesColumns(null)), hasItems(columnA, columnB));
    }

    @Test
    public void shouldHaveCorrectConstraintType() {
        assertThat(this.modelObject.getConstraintType(), is(TableConstraint.ConstraintType.FOREIGN_KEY));
    }

    @Test
    public void shouldHaveReferencesTableAfterConstruction() throws Exception {
        assertThat(this.modelObject.getReferencesTable(null), is(this.refTable));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.modelObject.getTable(null), is(this.parentTable));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullTableReference() throws Exception {
        this.modelObject.setReferencesTable(null, null);
    }

    @Test
    public void shouldNotHaveColumnReferencesAfterConstruction() throws Exception {
        assertThat(this.modelObject.getReferencesColumns(null), is(notNullValue()));
        assertThat(this.modelObject.getReferencesColumns(null).length, is(0));
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat(this.modelObject.getColumns(null), is(notNullValue()));
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldSetTableReference() throws Exception {
        final Table newTable = RelationalModelFactory.createTable(null, _repo, null, "newTable");
        this.modelObject.setReferencesTable(null, newTable);

        assertThat(this.modelObject.getReferencesTable(null), is(newTable));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailSettingNullTableReference() throws Exception {
        this.modelObject.setReferencesTable(null, null);
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, null, "removeColumnA");
        this.modelObject.addColumn(null, columnA);
        this.modelObject.removeColumn(null, columnA);
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldRemoveReferencesColumn() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn(null, _repo, null, "removeRefColumnA");
        this.modelObject.addReferencesColumn(null, columnA);
        this.modelObject.removeReferencesColumn(null, columnA);
        assertThat(this.modelObject.getReferencesColumns(null).length, is(0));
    }

}
