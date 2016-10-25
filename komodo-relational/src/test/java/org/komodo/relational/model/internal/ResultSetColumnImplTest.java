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
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ResultSetColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private ResultSetColumn column;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        final StoredProcedure procedure = model.addStoredProcedure( getTransaction(), "procedure" );
        this.resultSet = procedure.setResultSet( getTransaction(), TabularResultSet.class );
        this.column = this.resultSet.addColumn( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( getTransaction(), "blah" );
        this.column.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( getTransaction(), "blah" );
        this.column.setNameInSource( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( getTransaction(), "blah" );
        this.column.setDescription( getTransaction(), null );
        assertThat( this.column.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( getTransaction(), "blah" );
        this.column.setNameInSource( getTransaction(), null );
        assertThat( this.column.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotResultSetColumn() {
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
        assertThat(this.column.getTypeIdentifier( getTransaction() ), is(KomodoType.RESULT_SET_COLUMN));
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
    public void shouldHaveParentResultSet() throws Exception {
        assertThat( this.column.getParent( getTransaction() ), is( instanceOf( TabularResultSet.class ) ) );
        assertThat( this.column.getParent( getTransaction() ), is( ( KomodoObject )this.resultSet ) );
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
        this.column.setUuid( getTransaction(), "elvis" );
        this.column.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.column.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( getTransaction() ), is( nullValue() ) );
        assertThat( this.column.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
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
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( getTransaction(), value );
        assertThat( this.column.getNameInSource( getTransaction() ), is( value ) );
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
                    // standard options don't have any default values
                    assertThat( this.column.getStandardOptions().get( optionName ), is( nullValue() ) );
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
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.column.rename( getTransaction(), newName );
        assertThat( this.column.getName( getTransaction() ), is( newName ) );
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
