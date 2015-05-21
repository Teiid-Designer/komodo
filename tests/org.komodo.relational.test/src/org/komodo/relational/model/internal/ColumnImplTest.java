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
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private Column column;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "table" );
        this.column = RelationalModelFactory.createColumn( this.uow, _repo, this.table, NAME );
        commit();
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( this.uow, "blah" );
        this.column.setDescription( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMaxValue() throws Exception {
        this.column.setMaxValue( this.uow, "blah" );
        this.column.setMaxValue( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getMaxValue( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMinValue() throws Exception {
        this.column.setMinValue( this.uow, "blah" );
        this.column.setMinValue( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getMinValue( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( this.uow, "blah" );
        this.column.setNameInSource( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNativeType() throws Exception {
        this.column.setNativeType( this.uow, "blah" );
        this.column.setNativeType( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNativeType( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( this.uow, "blah" );
        this.column.setDescription( this.uow, null );
        assertThat( this.column.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMaxValue() throws Exception {
        this.column.setMaxValue( this.uow, "blah" );
        this.column.setMaxValue( this.uow, null );
        assertThat( this.column.getMaxValue( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMinValue() throws Exception {
        this.column.setMinValue( this.uow, "blah" );
        this.column.setMinValue( this.uow, null );
        assertThat( this.column.getMinValue( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( this.uow, "blah" );
        this.column.setNameInSource( this.uow, null );
        assertThat( this.column.getNameInSource( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNativeType() throws Exception {
        this.column.setNativeType( this.uow, "blah" );
        this.column.setNativeType( this.uow, null );
        assertThat( this.column.getNativeType( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotColumn() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ColumnImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( this.uow, null );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.column.getName( this.uow ), is( NAME ) );
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getLength( this.uow ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDatatypeName( this.uow ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getPrecision( this.uow ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getScale( this.uow ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat( this.column.isAutoIncremented( this.uow ), is( Column.DEFAULT_AUTO_INCREMENTED ) );
        assertThat( this.column.hasProperty( this.uow, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultCaseSensitiveAfterConstruction() throws Exception {
        assertThat( this.column.isCaseSensitive( this.uow ), is( Column.DEFAULT_CASE_SENSITIVE ) );
    }

    @Test
    public void shouldHaveDefaultCharOctetLengthAfterConstruction() throws Exception {
        assertThat( this.column.getCharOctetLength( this.uow ), is( Column.DEFAULT_CHAR_OCTET_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultCurrencyAfterConstruction() throws Exception {
        assertThat( this.column.isCurrency( this.uow ), is( Column.DEFAULT_CURRENCY ) );
    }

    @Test
    public void shouldHaveDefaultDistinctValuesAfterConstruction() throws Exception {
        assertThat( this.column.getDistinctValues( this.uow ), is( Column.DEFAULT_DISTINCT_VALUES ) );
    }

    @Test
    public void shouldHaveDefaultFixedLengthAfterConstruction() throws Exception {
        assertThat( this.column.isFixedLength( this.uow ), is( Column.DEFAULT_FIXED_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultNullValueCountAfterConstruction() throws Exception {
        assertThat( this.column.getNullValueCount( this.uow ), is( Column.DEFAULT_NULL_VALUE_COUNT ) );
    }

    @Test
    public void shouldHaveDefaultRadixAfterConstruction() throws Exception {
        assertThat( this.column.getRadix( this.uow ), is( Column.DEFAULT_RADIX ) );
    }

    @Test
    public void shouldHaveDefaultSearchableAfterConstruction() throws Exception {
        assertThat( this.column.getSearchable( this.uow ), is( Searchable.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveDefaultSelectableAfterConstruction() throws Exception {
        assertThat( this.column.isSelectable( this.uow ), is( Column.DEFAULT_SELECTABLE ) );
    }

    @Test
    public void shouldHaveDefaultSignedAfterConstruction() throws Exception {
        assertThat( this.column.isSigned( this.uow ), is( Column.DEFAULT_SIGNED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.column.isUpdatable( this.uow ), is( Column.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( this.uow );
        final String[] rawProps = this.column.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.column.getNullable( this.uow ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.NULLABLE ).getStringValue( this.uow ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentTable() throws Exception {
        assertThat( this.column.getParent( this.uow ), is( instanceOf( Table.class ) ) );
        assertThat( this.column.getParent( this.uow ), is( ( KomodoObject )this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.column.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( this.uow );
        final Filter[] filters = this.column.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.column.setCaseSensitive( this.uow, true );
        this.column.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.column.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveCollationNameAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
        assertThat( this.column.getCollationName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithEmptyString() throws Exception {
        this.column.setCollationName( this.uow, "collationName" );
        this.column.setCollationName( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getCollationName( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithNull() throws Exception {
        this.column.setCollationName( this.uow, "collationName" );
        this.column.setCollationName( this.uow, null );
        assertThat( this.column.getCollationName( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue( this.uow, "defaultValue" );
        this.column.setDefaultValue( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDefaultValue( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue( this.uow, "defaultValue" );
        this.column.setDefaultValue( this.uow, null );
        assertThat( this.column.getDefaultValue( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.column.setAutoIncremented( this.uow, value );
        assertThat( this.column.isAutoIncremented( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ).getBooleanValue( this.uow ),
                    is( value ) );
    }

    @Test
    public void shouldSetCaseSensitive() throws Exception {
        final boolean value = !Column.DEFAULT_CASE_SENSITIVE;
        this.column.setCaseSensitive( this.uow, value );
        assertThat( this.column.isCaseSensitive( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetCharOctetLength() throws Exception {
        final int value = 10;
        this.column.setCharOctetLength( this.uow, value );
        assertThat( this.column.getCharOctetLength( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.column.setCollationName( this.uow, value );
        assertThat( this.column.getCollationName( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.COLLATION_NAME ).getStringValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetCurrency() throws Exception {
        final boolean value = !Column.DEFAULT_CURRENCY;
        this.column.setCurrency( this.uow, value );
        assertThat( this.column.isCurrency( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.column.setLength( this.uow, value );
        assertThat( this.column.getLength( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName( this.uow, value );
        assertThat( this.column.getDatatypeName( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.DATATYPE_NAME ).getStringValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.column.setPrecision( this.uow, value );
        assertThat( this.column.getPrecision( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( this.uow ),
                    is( ( long )value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.column.setScale( this.uow, value );
        assertThat( this.column.getScale( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( this.uow ),
                    is( ( long )value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue( this.uow, value );
        assertThat( this.column.getDefaultValue( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.column.setDescription( this.uow, value );
        assertThat( this.column.getDescription( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDistinctValues() throws Exception {
        final long value = ( Column.DEFAULT_DISTINCT_VALUES + 5 );
        this.column.setDistinctValues( this.uow, value );
        assertThat( this.column.getDistinctValues( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetFixedLength() throws Exception {
        final boolean value = !Column.DEFAULT_FIXED_LENGTH;
        this.column.setFixedLength( this.uow, value );
        assertThat( this.column.isFixedLength( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetMaxValue() throws Exception {
        final String value = "maxValue";
        this.column.setMaxValue( this.uow, value );
        assertThat( this.column.getMaxValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetMinValue() throws Exception {
        final String value = "minValue";
        this.column.setMinValue( this.uow, value );
        assertThat( this.column.getMinValue( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( this.uow, value );
        assertThat( this.column.getNameInSource( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNativeType() throws Exception {
        final String value = "nativeType";
        this.column.setNativeType( this.uow, value );
        assertThat( this.column.getNativeType( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable( this.uow, value );
        assertThat( this.column.getNullable( this.uow ), is( value ) );
        assertThat( this.column.getProperty( this.uow, StandardDdlLexicon.NULLABLE ).getStringValue( this.uow ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullValueCount() throws Exception {
        final long value = 10;
        this.column.setNullValueCount( this.uow, value );
        assertThat( this.column.getNullValueCount( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetRadix() throws Exception {
        final int value = 10;
        this.column.setRadix( this.uow, value );
        assertThat( this.column.getRadix( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetSearchable() throws Exception {
        final Searchable value = Searchable.ALL_EXCEPT_LIKE;
        this.column.setSearchable( this.uow, value );
        assertThat( this.column.getSearchable( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetSelectable() throws Exception {
        final boolean value = !Column.DEFAULT_SELECTABLE;
        this.column.setSelectable( this.uow, value );
        assertThat( this.column.isSelectable( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetSigned() throws Exception {
        final boolean value = !Column.DEFAULT_SIGNED;
        this.column.setSigned( this.uow, value );
        assertThat( this.column.isSigned( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Column.DEFAULT_UPDATABLE;
        this.column.setUpdatable( this.uow, value );
        assertThat( this.column.isUpdatable( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.column.setUuid( this.uow, value );
        assertThat( this.column.getUuid( this.uow ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.column.setStatementOption( this.uow, customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( this.uow );
        boolean found = false;

        for ( final PropertyDescriptor descriptor : propDescriptors ) {
            if ( customName.equals( descriptor.getName() ) ) {
                found = true;
                break;
            }
        }

        if ( !found ) {
            fail( "Custom option '" + customName + "'was not included in the property descriptors" );
        }
    }

    @Test
    public void shouldIncludeOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( this.uow, custom, "sledge" );
        boolean customFound = false;

        final String standard = this.column.getStandardOptionNames()[0];
        this.column.setStatementOption( this.uow, standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.column.getPropertyNames( this.uow ) ) {
            if ( custom.equals( prop ) ) {
                if ( customFound ) {
                    fail( "Custom option included multiple times in property names" );
                }

                customFound = true;
            } else if ( standard.equals( prop ) ) {
                if ( standardFound ) {
                    fail( "Standard option included multiple times in property names" );
                }

                standardFound = true;
            }

            if ( customFound && standardFound ) {
                break;
            }
        }

        if ( !customFound ) {
            fail( "Custom option not included in property names" );
        }

        if ( !standardFound ) {
            fail( "Standard option not included in property names" );
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPrimaryTypePropertyDescriptors() throws Exception {
        final String[] optionNames = this.column.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.column.getPrimaryType( this.uow ).getPropertyDescriptors( this.uow );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the primary type property descriptors" );
            }
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPropertyDescriptors() throws Exception {
        final String[] optionNames = this.column.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( this.uow );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the property descriptors" );
            }
        }
    }

    @Test
    public void shouldObtainCustomOptions() throws Exception {
        final String sledge = "sledge";
        this.column.setStatementOption( this.uow, sledge, "hammer" );

        final String elvis = "elvis";
        this.column.setStatementOption( this.uow, elvis, "presley" );

        assertThat( this.column.getCustomOptions( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( this.uow ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.column.setStatementOption( this.uow, custom, "hammer" );

        assertThat( this.column.getPropertyDescriptor( this.uow, custom ), is( notNullValue() ) );
        assertThat( this.column.getPropertyDescriptor( this.uow, custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.column.getStandardOptionNames()[0];
        this.column.setStatementOption( this.uow, standard, "blah" );

        assertThat( this.column.getPropertyDescriptor( this.uow, standard ), is( notNullValue() ) );
        assertThat( this.column.getPropertyDescriptor( this.uow, standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( this.uow, custom, "sledge" );

        final String standard = this.column.getStandardOptionNames()[0];
        this.column.setStatementOption( this.uow, standard, "hammer" );

        assertThat( this.column.getStatementOptionNames( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( this.uow ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptionNames()[0];
        final String value = "newValue";
        this.column.setProperty( this.uow, option, value ); // add
        this.column.setProperty( this.uow, option, (Object)null ); // remove
        assertThat( this.column.hasProperty( this.uow, option ), is( false ) );
        assertThat( this.column.hasChild( this.uow, option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.column.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( this.uow, option, value );

        assertThat( this.column.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.column.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.getStatementOptions( this.uow ).length, is( 1 ) );
        assertThat( this.column.isCustomOption( this.uow, option ), is( true ) );

        final StatementOption statementOption = this.column.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptionNames()[0];
        this.column.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( this.uow, option, value );

        assertThat( this.column.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.column.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.isCustomOption( this.uow, option ), is( false ) );
        assertThat( this.column.getStatementOptions( this.uow ).length, is( 1 ) );

        final StatementOption statementOption = this.column.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

}
