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
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants.OnCommit;
import org.komodo.relational.RelationalConstants.TemporaryType;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class TableImplTest extends RelationalModelTest {

    private static final String NAME = "table";

    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable(null, _repo, mock(Model.class), NAME);
    }

    @Test
    public void shouldAddAccessPattern() throws Exception {
        final String name = "accesspattern";
        final AccessPattern accessPattern = this.table.addAccessPattern(null, name);
        assertThat(accessPattern, is(notNullValue()));
        assertThat(accessPattern.getName(null), is(name));
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String name = "column";
        final Column column = this.table.addColumn(null, name);
        assertThat(column, is(notNullValue()));
        assertThat(column.getName(null), is(name));
    }

    @Test
    public void shouldAddForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "refTable");
        final String name = "foreignKey";
        final ForeignKey foreignKey = this.table.addForeignKey(null, name, refTable);

        assertThat(foreignKey, is(notNullValue()));
        assertThat(foreignKey.getName(null), is(name));
    }

    @Test
    public void shouldAddIndex() throws Exception {
        final String name = "index";
        final Index index = this.table.addIndex(null, name);

        assertThat(index, is(notNullValue()));
        assertThat(index.getName(null), is(name));
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.table.addStatementOption(null, name, value);
        assertThat(statementOption, is(notNullValue()));
        assertThat(statementOption.getName(null), is(name));
        assertThat(statementOption.getOption(null), is(value));
    }

    @Test
    public void shouldAddUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        final UniqueConstraint uniqueConstraint = this.table.addUniqueConstraint(null, name);

        assertThat(uniqueConstraint, is(notNullValue()));
        assertThat(uniqueConstraint.getName(null), is(name));
    }

    @Test
    public void shouldAllowEmptyQueryExpression() throws Exception {
        this.table.setQueryExpression(null, StringConstants.EMPTY_STRING);
    }

    @Test
    public void shouldAllowNullOnCommitValue() throws Exception {
        this.table.setOnCommitValue(null, null);
    }

    @Test
    public void shouldAllowNullQueryExpression() throws Exception {
        this.table.setQueryExpression(null, null);
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.table.setSchemaElementType(null, null);
    }

    @Test
    public void shouldAllowNullTemporaryTypeValue() throws Exception {
        this.table.setTemporaryTableType(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyAccessPatternName() throws Exception {
        this.table.addAccessPattern(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyColumnName() throws Exception {
        this.table.addColumn(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyForeignKeyName() throws Exception {
        this.table.addForeignKey(null, StringConstants.EMPTY_STRING, mock(Table.class));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyIndexName() throws Exception {
        this.table.addIndex(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.table.addStatementOption(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionValue() throws Exception {
        this.table.addStatementOption(null, "blah", StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullAccessPatternName() throws Exception {
        this.table.addAccessPattern(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumnName() throws Exception {
        this.table.addColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullForeignKeyName() throws Exception {
        this.table.addForeignKey(null, null, mock(Table.class));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullIndexName() throws Exception {
        this.table.addIndex(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.table.addStatementOption(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.table.addStatementOption(null, "blah", null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint(null, null);
    }

    @Test
    public void shouldFailConstructionIfNotTable() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new TableImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey(null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyAccessPatternName() throws Exception {
        this.table.removeAccessPattern(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyColumnName() throws Exception {
        this.table.removeColumn(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyForeignKeyName() throws Exception {
        this.table.removeForeignKey(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyIndexName() throws Exception {
        this.table.removeIndex(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.table.removeStatementOption(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey(null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullAccessPatternName() throws Exception {
        this.table.removeAccessPattern(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullColumnName() throws Exception {
        this.table.removeColumn(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullForeignKeyName() throws Exception {
        this.table.removeForeignKey(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullIndexName() throws Exception {
        this.table.removeIndex(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.table.removeStatementOption(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownAccessPattern() throws Exception {
        this.table.removeAccessPattern(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownColumn() throws Exception {
        this.table.removeColumn(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownForeignKey() throws Exception {
        this.table.removeForeignKey(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownIndex() throws Exception {
        this.table.removeIndex(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.table.removeStatementOption(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownUniqueConstraint() throws Exception {
        this.table.removeUniqueConstraint(null, "unknown");
    }

    @Test
    public void shouldGetAccessPatterns() throws Exception {
        final int numAccessPatterns = 5;

        for (int i = 0; i < numAccessPatterns; ++i) {
            this.table.addAccessPattern(null, "accesspattern" + i);
        }

        assertThat(this.table.getAccessPatterns(null).length, is(numAccessPatterns));
    }

    @Test
    public void shouldGetColumns() throws Exception {
        final int numColumns = 5;

        for (int i = 0; i < numColumns; ++i) {
            this.table.addColumn(null, "column" + i);
        }

        assertThat(this.table.getColumns(null).length, is(numColumns));
    }

    @Test
    public void shouldGetForeignKeys() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "refTable");
        final int numForeignKeys = 5;

        for (int i = 0; i < numForeignKeys; ++i) {
            this.table.addForeignKey(null, "foreignKey" + i, refTable);
        }

        assertThat(this.table.getForeignKeys(null).length, is(numForeignKeys));
    }

    @Test
    public void shouldGetIndexes() throws Exception {
        final int numIndexes = 5;

        for (int i = 0; i < numIndexes; ++i) {
            this.table.addIndex(null, "index" + i);
        }

        assertThat(this.table.getIndexes(null).length, is(numIndexes));
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.table.addStatementOption(null, "statementoption" + i, "statementvalue" + i);
        }

        assertThat(this.table.getStatementOptions(null).length, is(numStatementOptions));
    }

    @Test
    public void shouldGetUniqueConstraints() throws Exception {
        final int numUniqueConstraints = 5;

        for (int i = 0; i < numUniqueConstraints; ++i) {
            this.table.addUniqueConstraint(null, "foreignKey" + i);
        }

        assertThat(this.table.getUniqueConstraints(null).length, is(numUniqueConstraints));
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.table.getSchemaElementType(null), is(SchemaElementType.DEFAULT_VALUE));
        assertThat(this.table.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldNotHaveOnCommitValueAfterConstruction() throws Exception {
        assertThat(this.table.hasProperty(null, StandardDdlLexicon.ON_COMMIT_VALUE), is(false));
        assertThat(this.table.getOnCommitValue(null), is(nullValue()));
    }

    @Test
    public void shouldNotHavePrimaryKeyAfterConstruction() throws Exception {
        assertThat(this.table.getPrimaryKey(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveQueryExpressionAfterConstruction() throws Exception {
        assertThat(this.table.getQueryExpression(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveTemporaryTypeValueAfterConstruction() throws Exception {
        assertThat(this.table.hasProperty(null, StandardDdlLexicon.TEMPORARY), is(false));
        assertThat(this.table.getTemporaryTableType(null), is(nullValue()));
    }

    @Test
    public void shouldRemoveAccessPattern() throws Exception {
        final String name = "accesspattern";
        this.table.addAccessPattern(null, name);
        this.table.removeAccessPattern(null, name);
        assertThat(this.table.getAccessPatterns(null).length, is(0));
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final String name = "column";
        this.table.addColumn(null, name);
        this.table.removeColumn(null, name);
        assertThat(this.table.getColumns(null).length, is(0));
    }

    @Test
    public void shouldRemoveForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable(null, _repo, mock(Model.class), "refTable");
        final String name = "foreignKey";
        this.table.addForeignKey(null, name, refTable);
        this.table.removeForeignKey(null, name);

        assertThat(this.table.getForeignKeys(null).length, is(0));
    }

    @Test
    public void shouldRemoveIndex() throws Exception {
        final String name = "index";
        this.table.addIndex(null, name);
        this.table.removeIndex(null, name);

        assertThat(this.table.getIndexes(null).length, is(0));
    }

    @Test
    public void shouldRemovePrimaryKey() throws Exception {
        this.table.setPrimaryKey(null, "primaryKey");
        this.table.removePrimaryKey(null);
        assertThat(this.table.getPrimaryKey(null), is(nullValue()));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.table.addStatementOption(null, name, "blah");
        this.table.removeStatementOption(null, name);
        assertThat(this.table.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldRemoveUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        this.table.addUniqueConstraint(null, name);
        this.table.removeUniqueConstraint(null, name);

        assertThat(this.table.getUniqueConstraints(null).length, is(0));
    }

    @Test
    public void shouldSetOnCommitProperty() throws Exception {
        final OnCommit value = OnCommit.DELETE_ROWS;
        this.table.setOnCommitValue(null, value);
        assertThat(this.table.getOnCommitValue(null), is(value));
        assertThat(this.table.getProperty(null, StandardDdlLexicon.ON_COMMIT_VALUE).getStringValue(null), is(value.toString()));
    }

    @Test
    public void shouldSetPrimaryKey() throws Exception {
        final String name = "primaryKey";
        final PrimaryKey pk = this.table.setPrimaryKey(null, name);
        assertThat(pk, is(notNullValue()));
        assertThat(pk.getName(null), is(name));
        assertThat(this.table.hasChild(null, name), is(true));
    }

    @Test
    public void shouldSetQueryExpression() throws Exception {
        final String value = "queryexpression";
        this.table.setQueryExpression(null, value);
        assertThat(this.table.getQueryExpression(null), is(value));
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.table.setSchemaElementType(null, value);
        assertThat(this.table.getSchemaElementType(null), is(value));
        assertThat(this.table.getProperty(null, TeiidDdlLexicon.SchemaElement.TYPE).getStringValue(null), is(value.toString()));
    }

    @Test
    public void shouldSetTemporaryTableTypeProperty() throws Exception {
        final TemporaryType value = TemporaryType.GLOBAL;
        this.table.setTemporaryTableType(null, value);
        assertThat(this.table.getTemporaryTableType(null), is(value));
        assertThat(this.table.getProperty(null, StandardDdlLexicon.TEMPORARY).getStringValue(null), is(value.toString()));
    }

}
