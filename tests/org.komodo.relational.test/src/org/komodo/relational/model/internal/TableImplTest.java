/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants.OnCommit;
import org.komodo.relational.RelationalConstants.TemporaryType;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.RelationalModelFactory;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class TableImplTest extends RelationalModelTest {

    private static final String NAME = "table";

    private Table modelObject;

    @Before
    public void init() throws Exception {
        this.modelObject = RelationalModelFactory.createTable(null, _repo, null, NAME);
    }

    @Test
    public void shouldAddAccessPattern() throws Exception {
        final String name = "accesspattern";
        final AccessPattern accessPattern = this.modelObject.addAccessPattern(null, name);
        assertThat(accessPattern, is(notNullValue()));
        assertThat(accessPattern.getName(null), is(name));
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String name = "column";
        final Column column = this.modelObject.addColumn(null, name);
        assertThat(column, is(notNullValue()));
        assertThat(column.getName(null), is(name));
    }

    @Test
    public void shouldAddForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, null, "refTable");

        final String name = "foreignKey";
        final ForeignKey foreignKey = this.modelObject.addForeignKey(null, name, refTable);

        assertThat(foreignKey, is(notNullValue()));
        assertThat(foreignKey.getName(null), is(name));
    }

    @Test
    public void shouldAddIndex() throws Exception {
        final String name = "index";
        final Index index = this.modelObject.addIndex(null, name);

        assertThat(index, is(notNullValue()));
        assertThat(index.getName(null), is(name));
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.modelObject.addStatementOption(null, name, value);
        assertThat(statementOption, is(notNullValue()));
        assertThat(statementOption.getName(null), is(name));
        assertThat(statementOption.getOption(null), is(value));
    }

    @Test
    public void shouldAddUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        final UniqueConstraint uniqueConstraint = this.modelObject.addUniqueConstraint(null, name);

        assertThat(uniqueConstraint, is(notNullValue()));
        assertThat(uniqueConstraint.getName(null), is(name));
    }

    @Test
    public void shouldAllowEmptyQueryExpression() throws Exception {
        this.modelObject.setQueryExpression(null, "");
    }

    @Test
    public void shouldAllowNullOnCommitValue() throws Exception {
        this.modelObject.setOnCommitValue(null, null);
    }

    @Test
    public void shouldAllowNullTemporaryTypeValue() throws Exception {
        this.modelObject.setTemporaryTableType(null, null);
    }

    @Test
    public void shouldAllowNullQueryExpression() throws Exception {
        this.modelObject.setQueryExpression(null, null);
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.modelObject.setSchemaElementType(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyAccessPatternName() throws Exception {
        this.modelObject.addAccessPattern(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyColumnName() throws Exception {
        this.modelObject.addColumn(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyForeignKeyName() throws Exception {
        this.modelObject.addForeignKey(null, "", mock(Table.class));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyIndexName() throws Exception {
        this.modelObject.addIndex(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.modelObject.addStatementOption(null, "", "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionValue() throws Exception {
        this.modelObject.addStatementOption(null, "blah", "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUniqueConstraintName() throws Exception {
        this.modelObject.addUniqueConstraint(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullAccessPatternName() throws Exception {
        this.modelObject.addAccessPattern(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumnName() throws Exception {
        this.modelObject.addColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullForeignKeyName() throws Exception {
        this.modelObject.addForeignKey(null, null, mock(Table.class));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullIndexName() throws Exception {
        this.modelObject.addIndex(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.modelObject.addStatementOption(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.modelObject.addStatementOption(null, "blah", null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUniqueConstraintName() throws Exception {
        this.modelObject.addUniqueConstraint(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingMissingPrimaryKey() throws Exception {
        this.modelObject.removePrimaryKey(null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyAccessPatternName() throws Exception {
        this.modelObject.removeAccessPattern(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyColumnName() throws Exception {
        this.modelObject.removeColumn(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyForeignKeyName() throws Exception {
        this.modelObject.removeForeignKey(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyIndexName() throws Exception {
        this.modelObject.removeIndex(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyUniqueConstraintName() throws Exception {
        this.modelObject.removeUniqueConstraint(null, "");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveMissingPrimaryKey() throws Exception {
        this.modelObject.removePrimaryKey(null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullAccessPatternName() throws Exception {
        this.modelObject.removeAccessPattern(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullColumnName() throws Exception {
        this.modelObject.removeColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullForeignKeyName() throws Exception {
        this.modelObject.removeForeignKey(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullIndexName() throws Exception {
        this.modelObject.removeIndex(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullUniqueConstraintName() throws Exception {
        this.modelObject.removeUniqueConstraint(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownAccessPattern() throws Exception {
        this.modelObject.removeAccessPattern(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownColumn() throws Exception {
        this.modelObject.removeColumn(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownForeignKey() throws Exception {
        this.modelObject.removeForeignKey(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownIndex() throws Exception {
        this.modelObject.removeIndex(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.modelObject.removeStatementOption(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownUniqueConstraint() throws Exception {
        this.modelObject.removeUniqueConstraint(null, "unknown");
    }

    @Test
    public void shouldGetAccessPatterns() throws Exception {
        final int numAccessPatterns = 5;

        for (int i = 0; i < numAccessPatterns; ++i) {
            this.modelObject.addAccessPattern(null, "accesspattern" + i);
        }

        assertThat(this.modelObject.getAccessPatterns(null).length, is(numAccessPatterns));
    }

    @Test
    public void shouldGetColumns() throws Exception {
        final int numColumns = 5;

        for (int i = 0; i < numColumns; ++i) {
            this.modelObject.addColumn(null, "column" + i);
        }

        assertThat(this.modelObject.getColumns(null).length, is(numColumns));
    }

    @Test
    public void shouldGetForeignKeys() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, null, "refTable");
        final int numForeignKeys = 5;

        for (int i = 0; i < numForeignKeys; ++i) {
            this.modelObject.addForeignKey(null, "foreignKey" + i, refTable);
        }

        assertThat(this.modelObject.getForeignKeys(null).length, is(numForeignKeys));
    }

    @Test
    public void shouldGetIndexes() throws Exception {
        final int numIndexes = 5;

        for (int i = 0; i < numIndexes; ++i) {
            this.modelObject.addIndex(null, "index" + i);
        }

        assertThat(this.modelObject.getIndexes(null).length, is(numIndexes));
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.modelObject.addStatementOption(null, "statementoption" + i, "statementvalue" + i);
        }

        assertThat(this.modelObject.getStatementOptions(null).length, is(numStatementOptions));
    }

    @Test
    public void shouldGetUniqueConstraints() throws Exception {
        final int numUniqueConstraints = 5;

        for (int i = 0; i < numUniqueConstraints; ++i) {
            this.modelObject.addUniqueConstraint(null, "foreignKey" + i);
        }

        assertThat(this.modelObject.getUniqueConstraints(null).length, is(numUniqueConstraints));
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getSchemaElementType(null), is(SchemaElementType.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldNotHaveOnCommitValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.ON_COMMIT_VALUE), is(false));
        assertThat(this.modelObject.getOnCommitValue(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveTemporaryTypeValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.TEMPORARY), is(false));
        assertThat(this.modelObject.getTemporaryTableType(null), is(nullValue()));
    }

    @Test
    public void shouldNotHavePrimaryKeyAfterConstruction() throws Exception {
        assertThat(this.modelObject.getPrimaryKey(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveQueryExpressionAfterConstruction() throws Exception {
        assertThat(this.modelObject.getQueryExpression(null), is(nullValue()));
    }

    @Test
    public void shouldRemoveAccessPattern() throws Exception {
        final String name = "accesspattern";
        this.modelObject.addAccessPattern(null, name);
        this.modelObject.removeAccessPattern(null, name);
        assertThat(this.modelObject.getAccessPatterns(null).length, is(0));
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final String name = "column";
        this.modelObject.addColumn(null, name);
        this.modelObject.removeColumn(null, name);
        assertThat(this.modelObject.getColumns(null).length, is(0));
    }

    @Test
    public void shouldRemoveForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, null, "refTable");

        final String name = "foreignKey";
        this.modelObject.addForeignKey(null, name, refTable);
        this.modelObject.removeForeignKey(null, name);

        assertThat(this.modelObject.getForeignKeys(null).length, is(0));
    }

    @Test
    public void shouldRemoveIndex() throws Exception {
        final String name = "index";
        this.modelObject.addIndex(null, name);
        this.modelObject.removeIndex(null, name);

        assertThat(this.modelObject.getIndexes(null).length, is(0));
    }

    @Test
    public void shouldRemovePrimaryKey() throws Exception {
        this.modelObject.setPrimaryKey(null, "primaryKey");
        this.modelObject.removePrimaryKey(null);
        assertThat(this.modelObject.getPrimaryKey(null), is(nullValue()));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.modelObject.addStatementOption(null, name, "blah");
        this.modelObject.removeStatementOption(null, name);
        assertThat(this.modelObject.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldRemoveUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        this.modelObject.addUniqueConstraint(null, name);
        this.modelObject.removeUniqueConstraint(null, name);

        assertThat(this.modelObject.getUniqueConstraints(null).length, is(0));
    }

    @Test
    public void shouldSetOnCommitProperty() throws Exception {
        final OnCommit value = OnCommit.DELETE_ROWS;
        this.modelObject.setOnCommitValue(null, value);
        assertThat(this.modelObject.getOnCommitValue(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.ON_COMMIT_VALUE).getStringValue(), is(value.toString()));
    }

    @Test
    public void shouldSetTemporaryTableTypeProperty() throws Exception {
        final TemporaryType value = TemporaryType.GLOBAL;
        this.modelObject.setTemporaryTableType(null, value);
        assertThat(this.modelObject.getTemporaryTableType(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.TEMPORARY).getStringValue(), is(value.toString()));
    }

    @Test
    public void shouldSetPrimaryKey() throws Exception {
        final String name = "primaryKey";
        final PrimaryKey pk = this.modelObject.setPrimaryKey(null, name);
        assertThat(pk, is(notNullValue()));
        assertThat(pk.getName(null), is(name));
        assertThat(this.modelObject.hasChild(null, name), is(true));
    }

    @Test
    public void shouldSetQueryExpression() throws Exception {
        final String value = "queryexpression";
        this.modelObject.setQueryExpression(null, value);
        assertThat(this.modelObject.getQueryExpression(null), is(value));
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.modelObject.setSchemaElementType(null, value);
        assertThat(this.modelObject.getSchemaElementType(null), is(value));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.SchemaElement.TYPE).getStringValue(), is(value.toString()));
    }

}
