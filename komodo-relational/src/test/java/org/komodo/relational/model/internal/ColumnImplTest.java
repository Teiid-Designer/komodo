/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private Column column;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.column = this.table.addColumn( getTransaction(), NAME );
//        commit();
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( getTransaction(), "blah" );
        this.column.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMaxValue() throws Exception {
        this.column.setMaxValue( getTransaction(), "blah" );
        this.column.setMaxValue( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getMaxValue( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMinValue() throws Exception {
        this.column.setMinValue( getTransaction(), "blah" );
        this.column.setMinValue( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getMinValue( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( getTransaction(), "blah" );
        this.column.setNameInSource( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNativeType() throws Exception {
        this.column.setNativeType( getTransaction(), "blah" );
        this.column.setNativeType( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getNativeType( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( getTransaction(), "blah" );
        this.column.setDescription( getTransaction(), null );
        assertThat( this.column.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMaxValue() throws Exception {
        this.column.setMaxValue( getTransaction(), "blah" );
        this.column.setMaxValue( getTransaction(), null );
        assertThat( this.column.getMaxValue( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMinValue() throws Exception {
        this.column.setMinValue( getTransaction(), "blah" );
        this.column.setMinValue( getTransaction(), null );
        assertThat( this.column.getMinValue( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( getTransaction(), "blah" );
        this.column.setNameInSource( getTransaction(), null );
        assertThat( this.column.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNativeType() throws Exception {
        this.column.setNativeType( getTransaction(), "blah" );
        this.column.setNativeType( getTransaction(), null );
        assertThat( this.column.getNativeType( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotColumn() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ColumnImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( getTransaction(), null );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.column.getName( getTransaction() ), is( NAME ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.column.getTypeIdentifier( getTransaction() ), is(KomodoType.COLUMN));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getLength( getTransaction() ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDatatypeName( getTransaction() ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getPrecision( getTransaction() ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getScale( getTransaction() ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat( this.column.isAutoIncremented( getTransaction() ), is( Column.DEFAULT_AUTO_INCREMENTED ) );
        assertThat( this.column.hasProperty( getTransaction(), TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultCaseSensitiveAfterConstruction() throws Exception {
        assertThat( this.column.isCaseSensitive( getTransaction() ), is( Column.DEFAULT_CASE_SENSITIVE ) );
    }

    @Test
    public void shouldHaveDefaultCharOctetLengthAfterConstruction() throws Exception {
        assertThat( this.column.getCharOctetLength( getTransaction() ), is( Column.DEFAULT_CHAR_OCTET_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultCurrencyAfterConstruction() throws Exception {
        assertThat( this.column.isCurrency( getTransaction() ), is( Column.DEFAULT_CURRENCY ) );
    }

    @Test
    public void shouldHaveDefaultDistinctValuesAfterConstruction() throws Exception {
        assertThat( this.column.getDistinctValues( getTransaction() ), is( Column.DEFAULT_DISTINCT_VALUES ) );
    }

    @Test
    public void shouldHaveDefaultFixedLengthAfterConstruction() throws Exception {
        assertThat( this.column.isFixedLength( getTransaction() ), is( Column.DEFAULT_FIXED_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultNullValueCountAfterConstruction() throws Exception {
        assertThat( this.column.getNullValueCount( getTransaction() ), is( Column.DEFAULT_NULL_VALUE_COUNT ) );
    }

    @Test
    public void shouldHaveDefaultRadixAfterConstruction() throws Exception {
        assertThat( this.column.getRadix( getTransaction() ), is( Column.DEFAULT_RADIX ) );
    }

    @Test
    public void shouldHaveDefaultSearchableAfterConstruction() throws Exception {
        assertThat( this.column.getSearchable( getTransaction() ), is( Searchable.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveDefaultSelectableAfterConstruction() throws Exception {
        assertThat( this.column.isSelectable( getTransaction() ), is( Column.DEFAULT_SELECTABLE ) );
    }

    @Test
    public void shouldHaveDefaultSignedAfterConstruction() throws Exception {
        assertThat( this.column.isSigned( getTransaction() ), is( Column.DEFAULT_SIGNED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.column.isUpdatable( getTransaction() ), is( Column.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( getTransaction() );
        final String[] rawProps = this.column.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.column.getNullable( getTransaction() ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentTable() throws Exception {
        assertThat( this.column.getParent( getTransaction() ), is( instanceOf( Table.class ) ) );
        assertThat( this.column.getParent( getTransaction() ), is( ( KomodoObject )this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.column.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( getTransaction() );
        final Filter[] filters = this.column.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.column.setCaseSensitive( getTransaction(), true );
        this.column.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.column.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveCollationNameAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.COLLATION_NAME ), is( false ) );
        assertThat( this.column.getCollationName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithEmptyString() throws Exception {
        this.column.setCollationName( getTransaction(), "collationName" );
        this.column.setCollationName( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getCollationName( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithNull() throws Exception {
        this.column.setCollationName( getTransaction(), "collationName" );
        this.column.setCollationName( getTransaction(), null );
        assertThat( this.column.getCollationName( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue( getTransaction(), "defaultValue" );
        this.column.setDefaultValue( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getDefaultValue( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue( getTransaction(), "defaultValue" );
        this.column.setDefaultValue( getTransaction(), null );
        assertThat( this.column.getDefaultValue( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.column.rename( getTransaction(), newName );
        assertThat( this.column.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.column.setAutoIncremented( getTransaction(), value );
        assertThat( this.column.isAutoIncremented( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ).getBooleanValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetCaseSensitive() throws Exception {
        final boolean value = !Column.DEFAULT_CASE_SENSITIVE;
        this.column.setCaseSensitive( getTransaction(), value );
        assertThat( this.column.isCaseSensitive( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetCharOctetLength() throws Exception {
        final long value = 10;
        this.column.setCharOctetLength( getTransaction(), value );
        assertThat( this.column.getCharOctetLength( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.column.setCollationName( getTransaction(), value );
        assertThat( this.column.getCollationName( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.COLLATION_NAME ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetCurrency() throws Exception {
        final boolean value = !Column.DEFAULT_CURRENCY;
        this.column.setCurrency( getTransaction(), value );
        assertThat( this.column.isCurrency( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.column.setLength( getTransaction(), value );
        assertThat( this.column.getLength( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName( getTransaction(), value );
        assertThat( this.column.getDatatypeName( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_NAME ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final long value = 10;
        this.column.setPrecision( getTransaction(), value );
        assertThat( this.column.getPrecision( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final long value = 10;
        this.column.setScale( getTransaction(), value );
        assertThat( this.column.getScale( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue( getTransaction(), value );
        assertThat( this.column.getDefaultValue( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.column.setDescription( getTransaction(), value );
        assertThat( this.column.getDescription( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDistinctValues() throws Exception {
        final long value = ( Column.DEFAULT_DISTINCT_VALUES + 5 );
        this.column.setDistinctValues( getTransaction(), value );
        assertThat( this.column.getDistinctValues( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetFixedLength() throws Exception {
        final boolean value = !Column.DEFAULT_FIXED_LENGTH;
        this.column.setFixedLength( getTransaction(), value );
        assertThat( this.column.isFixedLength( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMaxValue() throws Exception {
        final String value = "maxValue";
        this.column.setMaxValue( getTransaction(), value );
        assertThat( this.column.getMaxValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMinValue() throws Exception {
        final String value = "minValue";
        this.column.setMinValue( getTransaction(), value );
        assertThat( this.column.getMinValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( getTransaction(), value );
        assertThat( this.column.getNameInSource( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNativeType() throws Exception {
        final String value = "nativeType";
        this.column.setNativeType( getTransaction(), value );
        assertThat( this.column.getNativeType( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable( getTransaction(), value );
        assertThat( this.column.getNullable( getTransaction() ), is( value ) );
        assertThat( this.column.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullValueCount() throws Exception {
        final long value = 10;
        this.column.setNullValueCount( getTransaction(), value );
        assertThat( this.column.getNullValueCount( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetRadix() throws Exception {
        final long value = 10;
        this.column.setRadix( getTransaction(), value );
        assertThat( this.column.getRadix( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetSearchable() throws Exception {
        final Searchable value = Searchable.ALL_EXCEPT_LIKE;
        this.column.setSearchable( getTransaction(), value );
        assertThat( this.column.getSearchable( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetSelectable() throws Exception {
        final boolean value = !Column.DEFAULT_SELECTABLE;
        this.column.setSelectable( getTransaction(), value );
        assertThat( this.column.isSelectable( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetSigned() throws Exception {
        final boolean value = !Column.DEFAULT_SIGNED;
        this.column.setSigned( getTransaction(), value );
        assertThat( this.column.isSigned( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Column.DEFAULT_UPDATABLE;
        this.column.setUpdatable( getTransaction(), value );
        assertThat( this.column.isUpdatable( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.column.setUuid( getTransaction(), value );
        assertThat( this.column.getUuid( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.column.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( getTransaction() );
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
    public void shouldIncludeStandardOptionDefaultValuesWithPropertyDescriptors() throws Exception {
        final Map< String, String > options = this.column.getStandardOptions();
        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( getTransaction() );

        for ( final Entry< String, String > entry : options.entrySet() ) {
            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( entry.getKey().equals( descriptor.getName() ) ) {
                    final String value = entry.getValue();
                    final Object[] defaultValues = descriptor.getDefaultValues();

                    if ( StringUtils.isBlank( value ) ) {
                        assertThat( defaultValues.length, is( 0 ) );
                    } else {
                        assertThat( defaultValues.length, is( 1 ) );
                        assertThat( value, is( defaultValues[0] ) );
                    }
                }
            }
        }
    }

    @Test
    public void shouldIncludeOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        final String standard = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( getTransaction(), standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.column.getPropertyNames( getTransaction() ) ) {
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
        final Set< String > optionNames = this.column.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.column.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );

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
        final Set< String > optionNames = this.column.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( getTransaction() );

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
        this.column.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.column.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.column.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.column.setStatementOption( getTransaction(), custom, "hammer" );

        assertThat( this.column.getPropertyDescriptor( getTransaction(), custom ), is( notNullValue() ) );
        assertThat( this.column.getPropertyDescriptor( getTransaction(), custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( getTransaction(), standard, "blah" );

        assertThat( this.column.getPropertyDescriptor( getTransaction(), standard ), is( notNullValue() ) );
        assertThat( this.column.getPropertyDescriptor( getTransaction(), standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( getTransaction(), custom, "sledge" );

        final String standard = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( getTransaction(), standard, "hammer" );

        assertThat( this.column.getStatementOptionNames( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( getTransaction() ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.column.setProperty( getTransaction(), option, value ); // add
        this.column.setProperty( getTransaction(), option, (Object)null ); // remove
        assertThat( this.column.hasProperty( getTransaction(), option ), is( false ) );
        assertThat( this.column.hasChild( getTransaction(), option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.column.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( getTransaction(), option, value );

        assertThat( this.column.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.column.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.column.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.column.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( getTransaction(), option, value );

        assertThat( this.column.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.column.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.isCustomOption( getTransaction(), option ), is( false ) );
        assertThat( this.column.getStatementOptions( getTransaction() ).length, is( 1 ) );

        final StatementOption statementOption = this.column.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

}
