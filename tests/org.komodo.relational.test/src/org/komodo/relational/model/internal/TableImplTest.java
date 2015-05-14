/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
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
import org.komodo.relational.model.Table.OnCommit;
import org.komodo.relational.model.Table.TemporaryType;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TableImplTest extends RelationalModelTest {

    private static final String NAME = "myTable";

    private Model model;
    private Table table;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "path" );
        this.model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        this.table = RelationalModelFactory.createTable( this.uow, _repo, this.model, NAME );
        commit();
    }

    @Test
    public void shouldAddAccessPattern() throws Exception {
        final String name = "accesspattern";
        final AccessPattern accessPattern = this.table.addAccessPattern( this.uow, name );
        assertThat( accessPattern, is( notNullValue() ) );
        assertThat( accessPattern.getName( this.uow ), is( name ) );
        assertThat( this.table.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.table.getChildren( this.uow )[0], is( instanceOf( AccessPattern.class ) ) );
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String name = "column";
        final Column column = this.table.addColumn( this.uow, name );
        assertThat( column, is( notNullValue() ) );
        assertThat( this.table.getColumns( this.uow ).length, is( 1 ) );
        assertThat( column.getName( this.uow ), is( name ) );
        assertThat( this.table.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.table.getChildren( this.uow )[0], is( instanceOf( Column.class ) ) );
    }

    @Test
    public void shouldAddForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "refTable" );
        final String name = "foreignKey";
        final ForeignKey foreignKey = this.table.addForeignKey( this.uow, name, refTable );

        assertThat( foreignKey, is( notNullValue() ) );
        assertThat( foreignKey.getName( this.uow ), is( name ) );
        assertThat( this.table.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.table.getChildren( this.uow )[0], is( instanceOf( ForeignKey.class ) ) );
    }

    @Test
    public void shouldAddIndex() throws Exception {
        final String name = "index";
        final Index index = this.table.addIndex( this.uow, name );

        assertThat( index, is( notNullValue() ) );
        assertThat( index.getName( this.uow ), is( name ) );
        assertThat( this.table.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.table.getChildren( this.uow )[0], is( instanceOf( Index.class ) ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.table.setStatementOption( this.uow, name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( this.uow ), is( name ) );
        assertThat( statementOption.getOption( this.uow ), is( value ) );
    }

    @Test
    public void shouldAddUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        final UniqueConstraint uniqueConstraint = this.table.addUniqueConstraint( this.uow, name );

        assertThat( uniqueConstraint, is( notNullValue() ) );
        assertThat( uniqueConstraint.getName( this.uow ), is( name ) );
        assertThat( this.table.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.table.getChildren( this.uow )[0], is( instanceOf( UniqueConstraint.class ) ) );
    }

    @Test
    public void shouldAllowEmptyQueryExpression() throws Exception {
        this.table.setQueryExpression( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowNullOnCommitValue() throws Exception {
        this.table.setOnCommitValue( this.uow, null );
    }

    @Test
    public void shouldAllowNullQueryExpression() throws Exception {
        this.table.setQueryExpression( this.uow, null );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.table.setSchemaElementType( this.uow, null );
    }

    @Test
    public void shouldAllowNullTemporaryTypeValue() throws Exception {
        this.table.setTemporaryTableType( this.uow, null );
    }

    @Test
    public void shouldCountChildren() throws Exception {
        this.table.addAccessPattern( this.uow, "accessPattern" );
        this.table.addColumn( this.uow, "column" );
        this.table.addIndex( this.uow, "index" );
        this.table.addUniqueConstraint( this.uow, "uniqueConstraint" );
        this.table.setPrimaryKey( this.uow, "primaryKey" );
        assertThat( this.table.getChildren( this.uow ).length, is( 5 ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyAccessPatternName() throws Exception {
        this.table.addAccessPattern( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyColumnName() throws Exception {
        this.table.addColumn( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyForeignKeyName() throws Exception {
        this.table.addForeignKey( this.uow, StringConstants.EMPTY_STRING, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyIndexName() throws Exception {
        this.table.addIndex( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.table.setStatementOption( this.uow, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullAccessPatternName() throws Exception {
        this.table.addAccessPattern( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumnName() throws Exception {
        this.table.addColumn( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullForeignKeyName() throws Exception {
        this.table.addForeignKey( this.uow, null, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullIndexName() throws Exception {
        this.table.addIndex( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.table.setStatementOption( this.uow, null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( this.uow, null );
    }

    @Test
    public void shouldFailConstructionIfNotTable() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TableImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey( this.uow );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( this.uow, "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( this.uow, "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyAccessPatternName() throws Exception {
        this.table.removeAccessPattern( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyColumnName() throws Exception {
        this.table.removeColumn( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyForeignKeyName() throws Exception {
        this.table.removeForeignKey( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyIndexName() throws Exception {
        this.table.removeIndex( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.table.removeStatementOption( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey( this.uow );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullAccessPatternName() throws Exception {
        this.table.removeAccessPattern( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullColumnName() throws Exception {
        this.table.removeColumn( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullForeignKeyName() throws Exception {
        this.table.removeForeignKey( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullIndexName() throws Exception {
        this.table.removeIndex( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.table.removeStatementOption( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownAccessPattern() throws Exception {
        this.table.removeAccessPattern( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownColumn() throws Exception {
        this.table.removeColumn( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownForeignKey() throws Exception {
        this.table.removeForeignKey( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownIndex() throws Exception {
        this.table.removeIndex( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.table.removeStatementOption( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownUniqueConstraint() throws Exception {
        this.table.removeUniqueConstraint( this.uow, "unknown" );
    }

    @Test
    public void shouldGetAccessPatterns() throws Exception {
        final int numAccessPatterns = 5;

        for ( int i = 0; i < numAccessPatterns; ++i ) {
            this.table.addAccessPattern( this.uow, "accesspattern" + i );
        }

        assertThat( this.table.getAccessPatterns( this.uow ).length, is( numAccessPatterns ) );
    }

    @Test
    public void shouldGetColumns() throws Exception {
        final int numColumns = 5;

        for ( int i = 0; i < numColumns; ++i ) {
            this.table.addColumn( this.uow, "column" + i );
        }

        assertThat( this.table.getColumns( this.uow ).length, is( numColumns ) );
    }

    @Test
    public void shouldGetForeignKeys() throws Exception {
        final Table refTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "refTable" );
        final int numForeignKeys = 5;

        for ( int i = 0; i < numForeignKeys; ++i ) {
            this.table.addForeignKey( this.uow, "foreignKey" + i, refTable );
        }

        assertThat( this.table.getForeignKeys( this.uow ).length, is( numForeignKeys ) );
    }

    @Test
    public void shouldGetIndexes() throws Exception {
        final int numIndexes = 5;

        for ( int i = 0; i < numIndexes; ++i ) {
            this.table.addIndex( this.uow, "index" + i );
        }

        assertThat( this.table.getIndexes( this.uow ).length, is( numIndexes ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.table.setStatementOption( this.uow, "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.table.getStatementOptions( this.uow ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldGetUniqueConstraints() throws Exception {
        final int numUniqueConstraints = 5;

        for ( int i = 0; i < numUniqueConstraints; ++i ) {
            this.table.addUniqueConstraint( this.uow, "foreignKey" + i );
        }

        assertThat( this.table.getUniqueConstraints( this.uow ).length, is( numUniqueConstraints ) );
    }

    @Test
    public void shouldHaveDefaultCardinalityAfterConstruction() throws Exception {
        assertThat( this.table.getCardinality( this.uow ), is( Table.DEFAULT_CARDINALITY ) );
    }

    @Test
    public void shouldHaveDefaultMaterializedAfterConstruction() throws Exception {
        assertThat( this.table.isMaterialized( this.uow ), is( Table.DEFAULT_MATERIALIZED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.table.isUpdatable( this.uow ), is( Table.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( this.uow );
        final String[] rawProps = this.table.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.table.getParent( this.uow ), is( instanceOf( Model.class ) ) );
        assertThat( this.table.getParent( this.uow ), is( ( KomodoObject )this.model ) );
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.table.getSchemaElementType( this.uow ), is( SchemaElementType.DEFAULT_VALUE ) );
        assertThat( this.table.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( this.uow );
        final Filter[] filters = this.table.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.table.setCardinality( this.uow, 5 );
        this.table.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.table.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveOnCommitValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( this.uow, StandardDdlLexicon.ON_COMMIT_VALUE ), is( false ) );
        assertThat( this.table.getOnCommitValue( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHavePrimaryKeyAfterConstruction() throws Exception {
        assertThat( this.table.getPrimaryKey( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveQueryExpressionAfterConstruction() throws Exception {
        assertThat( this.table.getQueryExpression( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveTemporaryTypeValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( this.uow, StandardDdlLexicon.TEMPORARY ), is( false ) );
        assertThat( this.table.getTemporaryTableType( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveAccessPattern() throws Exception {
        final String name = "accesspattern";
        this.table.addAccessPattern( this.uow, name );
        this.table.removeAccessPattern( this.uow, name );
        assertThat( this.table.getAccessPatterns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final String name = "column";
        this.table.addColumn( this.uow, name );
        this.table.removeColumn( this.uow, name );
        assertThat( this.table.getColumns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveForeignKey() throws Exception {
        final Table refTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( this.uow, name, refTable );
        this.table.removeForeignKey( this.uow, name );

        assertThat( this.table.getForeignKeys( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveIndex() throws Exception {
        final String name = "index";
        this.table.addIndex( this.uow, name );
        this.table.removeIndex( this.uow, name );

        assertThat( this.table.getIndexes( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePrimaryKey() throws Exception {
        this.table.setPrimaryKey( this.uow, "primaryKey" );
        this.table.removePrimaryKey( this.uow );
        assertThat( this.table.getPrimaryKey( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.table.setStatementOption( this.uow, name, "blah" );
        this.table.removeStatementOption( this.uow, name );
        assertThat( this.table.getStatementOptions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        this.table.addUniqueConstraint( this.uow, name );
        this.table.removeUniqueConstraint( this.uow, name );

        assertThat( this.table.getUniqueConstraints( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetCardinality() throws Exception {
        final int value = 10;
        this.table.setCardinality( this.uow, value );
        assertThat( this.table.getCardinality( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.table.setDescription( this.uow, value );
        assertThat( this.table.getDescription( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetMaterialized() throws Exception {
        final boolean value = !Table.DEFAULT_MATERIALIZED;
        this.table.setMaterialized( this.uow, value );
        assertThat( this.table.isMaterialized( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetMaterializedTable() throws Exception {
        final String value = "materializedTable";
        this.table.setMaterializedTable( this.uow, value );
        assertThat( this.table.getMaterializedTable( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.table.setNameInSource( this.uow, value );
        assertThat( this.table.getNameInSource( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetOnCommitProperty() throws Exception {
        final OnCommit value = OnCommit.DELETE_ROWS;
        this.table.setOnCommitValue( this.uow, value );
        assertThat( this.table.getOnCommitValue( this.uow ), is( value ) );
        assertThat( this.table.getProperty( this.uow, StandardDdlLexicon.ON_COMMIT_VALUE ).getStringValue( this.uow ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetPrimaryKey() throws Exception {
        final String name = "primaryKey";
        final PrimaryKey pk = this.table.setPrimaryKey( this.uow, name );
        assertThat( pk, is( notNullValue() ) );
        assertThat( pk.getName( this.uow ), is( name ) );
        assertThat( this.table.hasChild( this.uow, name ), is( true ) );
    }

    @Test
    public void shouldSetQueryExpression() throws Exception {
        final String value = ( "select * from " + NAME );
        this.table.setQueryExpression( this.uow, value );
        assertThat( this.table.getQueryExpression( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.table.setSchemaElementType( this.uow, value );
        assertThat( this.table.getSchemaElementType( this.uow ), is( value ) );
        assertThat( this.table.getProperty( this.uow, TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( this.uow ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetTemporaryTableTypeProperty() throws Exception {
        final TemporaryType value = TemporaryType.GLOBAL;
        this.table.setTemporaryTableType( this.uow, value );
        assertThat( this.table.getTemporaryTableType( this.uow ), is( value ) );
        assertThat( this.table.getProperty( this.uow, StandardDdlLexicon.TEMPORARY ).getStringValue( this.uow ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Table.DEFAULT_UPDATABLE;
        this.table.setUpdatable( this.uow, value );
        assertThat( this.table.isUpdatable( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.table.setUuid( this.uow, value );
        assertThat( this.table.getUuid( this.uow ), is( value ) );
    }

}
