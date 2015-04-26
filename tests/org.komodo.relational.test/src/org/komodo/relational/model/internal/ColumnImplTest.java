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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private Column column;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable( null, _repo, mock( Model.class ), "table" );
        this.column = RelationalModelFactory.createColumn( null, _repo, this.table, NAME );
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( null, "blah" );
        this.column.setDescription( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMaxValue() throws Exception {
        this.column.setMaxValue( null, "blah" );
        this.column.setMaxValue( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getMaxValue( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMinValue() throws Exception {
        this.column.setMinValue( null, "blah" );
        this.column.setMinValue( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getMinValue( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( null, "blah" );
        this.column.setNameInSource( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNativeType() throws Exception {
        this.column.setNativeType( null, "blah" );
        this.column.setNativeType( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNativeType( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( null, "blah" );
        this.column.setDescription( null, null );
        assertThat( this.column.getDescription( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMaxValue() throws Exception {
        this.column.setMaxValue( null, "blah" );
        this.column.setMaxValue( null, null );
        assertThat( this.column.getMaxValue( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMinValue() throws Exception {
        this.column.setMinValue( null, "blah" );
        this.column.setMinValue( null, null );
        assertThat( this.column.getMinValue( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( null, "blah" );
        this.column.setNameInSource( null, null );
        assertThat( this.column.getNameInSource( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNativeType() throws Exception {
        this.column.setNativeType( null, "blah" );
        this.column.setNativeType( null, null );
        assertThat( this.column.getNativeType( null ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotColumn() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ColumnImpl( null, _repo, _repo.komodoLibrary( null ).getAbsolutePath() );
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( null, null );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.column.getName( null ), is( NAME ) );
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getLength( null ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDatatypeName( null ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getPrecision( null ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getScale( null ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat( this.column.isAutoIncremented( null ), is( Column.DEFAULT_AUTO_INCREMENTED ) );
        assertThat( this.column.hasProperty( null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultCaseSensitiveAfterConstruction() throws Exception {
        assertThat( this.column.isCaseSensitive( null ), is( Column.DEFAULT_CASE_SENSITIVE ) );
    }

    @Test
    public void shouldHaveDefaultCharOctetLengthAfterConstruction() throws Exception {
        assertThat( this.column.getCharOctetLength( null ), is( Column.DEFAULT_CHAR_OCTET_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultCurrencyAfterConstruction() throws Exception {
        assertThat( this.column.isCurrency( null ), is( Column.DEFAULT_CURRENCY ) );
    }

    @Test
    public void shouldHaveDefaultDistinctValuesAfterConstruction() throws Exception {
        assertThat( this.column.getDistinctValues( null ), is( Column.DEFAULT_DISTINCT_VALUES ) );
    }

    @Test
    public void shouldHaveDefaultFixedLengthAfterConstruction() throws Exception {
        assertThat( this.column.isFixedLength( null ), is( Column.DEFAULT_FIXED_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultNullValueCountAfterConstruction() throws Exception {
        assertThat( this.column.getNullValueCount( null ), is( Column.DEFAULT_NULL_VALUE_COUNT ) );
    }

    @Test
    public void shouldHaveDefaultRadixAfterConstruction() throws Exception {
        assertThat( this.column.getRadix( null ), is( Column.DEFAULT_RADIX ) );
    }

    @Test
    public void shouldHaveDefaultSearchableAfterConstruction() throws Exception {
        assertThat( this.column.getSearchable( null ), is( Searchable.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveDefaultSelectableAfterConstruction() throws Exception {
        assertThat( this.column.isSelectable( null ), is( Column.DEFAULT_SELECTABLE ) );
    }

    @Test
    public void shouldHaveDefaultSignedAfterConstruction() throws Exception {
        assertThat( this.column.isSigned( null ), is( Column.DEFAULT_SIGNED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.column.isUpdatable( null ), is( Column.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.column.getNullable( null ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.NULLABLE ).getStringValue( null ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentTable() throws Exception {
        assertThat( this.column.getParent( null ), is( instanceOf( Table.class ) ) );
        assertThat( this.column.getParent( null ), is( ( KomodoObject )this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.column.addChild( null, "blah", null );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.column.setCaseSensitive( null, true );
        this.column.setStatementOption( null, "sledge", "hammer" );
        assertThat( this.column.getChildren( null ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveCollationNameAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
        assertThat( this.column.getCollationName( null ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithEmptyString() throws Exception {
        this.column.setCollationName( null, "collationName" );
        this.column.setCollationName( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getCollationName( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithNull() throws Exception {
        this.column.setCollationName( null, "collationName" );
        this.column.setCollationName( null, null );
        assertThat( this.column.getCollationName( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue( null, "defaultValue" );
        this.column.setDefaultValue( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue( null, "defaultValue" );
        this.column.setDefaultValue( null, null );
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.column.setAutoIncremented( null, value );
        assertThat( this.column.isAutoIncremented( null ), is( value ) );
        assertThat( this.column.getProperty( null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ).getBooleanValue( null ),
                    is( value ) );
    }

    @Test
    public void shouldSetCaseSensitive() throws Exception {
        final boolean value = !Column.DEFAULT_CASE_SENSITIVE;
        this.column.setCaseSensitive( null, value );
        assertThat( this.column.isCaseSensitive( null ), is( value ) );
    }

    @Test
    public void shouldSetCharOctetLength() throws Exception {
        final int value = 10;
        this.column.setCharOctetLength( null, value );
        assertThat( this.column.getCharOctetLength( null ), is( value ) );
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.column.setCollationName( null, value );
        assertThat( this.column.getCollationName( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.COLLATION_NAME ).getStringValue( null ), is( value ) );
    }

    @Test
    public void shouldSetCurrency() throws Exception {
        final boolean value = !Column.DEFAULT_CURRENCY;
        this.column.setCurrency( null, value );
        assertThat( this.column.isCurrency( null ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.column.setLength( null, value );
        assertThat( this.column.getLength( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName( null, value );
        assertThat( this.column.getDatatypeName( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_NAME ).getStringValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.column.setPrecision( null, value );
        assertThat( this.column.getPrecision( null ), is( value ) );
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DATATYPE_PRECISION).getLongValue(null), is((long)value));
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.column.setScale( null, value );
        assertThat( this.column.getScale( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( null ), is( ( long )value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue( null, value );
        assertThat( this.column.getDefaultValue( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.column.setDescription( null, value );
        assertThat( this.column.getDescription( null ), is( value ) );
    }

    @Test
    public void shouldSetDistinctValues() throws Exception {
        final long value = ( Column.DEFAULT_DISTINCT_VALUES + 5 );
        this.column.setDistinctValues( null, value );
        assertThat( this.column.getDistinctValues( null ), is( value ) );
    }

    @Test
    public void shouldSetFixedLength() throws Exception {
        final boolean value = !Column.DEFAULT_FIXED_LENGTH;
        this.column.setFixedLength( null, value );
        assertThat( this.column.isFixedLength( null ), is( value ) );
    }

    @Test
    public void shouldSetMaxValue() throws Exception {
        final String value = "maxValue";
        this.column.setMaxValue( null, value );
        assertThat( this.column.getMaxValue( null ), is( value ) );
    }

    @Test
    public void shouldSetMinValue() throws Exception {
        final String value = "minValue";
        this.column.setMinValue( null, value );
        assertThat( this.column.getMinValue( null ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( null, value );
        assertThat( this.column.getNameInSource( null ), is( value ) );
    }

    @Test
    public void shouldSetNativeType() throws Exception {
        final String value = "nativeType";
        this.column.setNativeType( null, value );
        assertThat( this.column.getNativeType( null ), is( value ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable( null, value );
        assertThat( this.column.getNullable( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.NULLABLE ).getStringValue( null ), is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullValueCount() throws Exception {
        final long value = 10;
        this.column.setNullValueCount( null, value );
        assertThat( this.column.getNullValueCount( null ), is( value ) );
    }

    @Test
    public void shouldSetRadix() throws Exception {
        final int value = 10;
        this.column.setRadix( null, value );
        assertThat( this.column.getRadix( null ), is( value ) );
    }

    @Test
    public void shouldSetSearchable() throws Exception {
        final Searchable value = Searchable.ALL_EXCEPT_LIKE;
        this.column.setSearchable( null, value );
        assertThat( this.column.getSearchable( null ), is( value ) );
    }

    @Test
    public void shouldSetSelectable() throws Exception {
        final boolean value = !Column.DEFAULT_SELECTABLE;
        this.column.setSelectable( null, value );
        assertThat( this.column.isSelectable( null ), is( value ) );
    }

    @Test
    public void shouldSetSigned() throws Exception {
        final boolean value = !Column.DEFAULT_SIGNED;
        this.column.setSigned( null, value );
        assertThat( this.column.isSigned( null ), is( value ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Column.DEFAULT_UPDATABLE;
        this.column.setUpdatable( null, value );
        assertThat( this.column.isUpdatable( null ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.column.setUuid( null, value );
        assertThat( this.column.getUuid( null ), is( value ) );
    }

}
