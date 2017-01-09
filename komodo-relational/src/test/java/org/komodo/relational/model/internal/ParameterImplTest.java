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
import static org.hamcrest.core.IsCollectionContaining.hasItem;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ParameterImplTest extends RelationalModelTest {

    private static final String NAME = "parameter";

    private Parameter parameter;
    private AbstractProcedure procedure;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addVirtualProcedure( getTransaction(), "procedure" );
        this.parameter = this.procedure.addParameter( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.parameter.setStatementOption( getTransaction(), name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( getTransaction() ), is( name ) );
        assertThat( statementOption.getOption( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldAllowEmptyDatatypeName() throws Exception {
        this.parameter.setDatatypeName( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowEmptyDefaultValue() throws Exception {
        this.parameter.setDefaultValue( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowNullDatatypeName() throws Exception {
        this.parameter.setDatatypeName( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullDefaultValue() throws Exception {
        this.parameter.setDefaultValue( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullDirection() throws Exception {
        this.parameter.setDirection( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullNullable() throws Exception {
        this.parameter.setNullable( getTransaction(), null );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.parameter.isChildRestricted(), is( true ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.parameter.setStatementOption( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.parameter.setStatementOption( getTransaction(), null, "blah" );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValueWhenNeverAdded() throws Exception {
        this.parameter.setStatementOption( getTransaction(), "blah", null );
    }

    @Test
    public void shouldFailConstructionIfNotParameter() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ParameterImpl( getTransaction(), _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.parameter.setStatementOption( getTransaction(), "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.parameter.removeStatementOption( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.parameter.removeStatementOption( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.parameter.removeStatementOption( getTransaction(), "unknown" );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.parameter.setStatementOption( getTransaction(), "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.parameter.getStatementOptions( getTransaction() ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.parameter.hasDescriptor( getTransaction(), CreateProcedure.PARAMETER ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.parameter.getTypeIdentifier( getTransaction() ), is(KomodoType.PARAMETER));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getLength( getTransaction() ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDatatypeName( getTransaction() ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getPrecision( getTransaction() ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getScale( getTransaction() ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultResultAfterConstruction() throws Exception {
        assertThat( this.parameter.isResult( getTransaction() ), is( Parameter.DEFAULT_RESULT ) );
    }

    @Test
    public void shouldHaveDirectionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDirection( getTransaction() ), is( Direction.DEFAULT_VALUE ) );
        assertThat( this.parameter.hasProperty( getTransaction(), TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ), is( true ) );
        assertThat( this.parameter.getProperty( getTransaction(), TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ).getStringValue( getTransaction() ),
                    is( Direction.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.parameter.getPropertyNames( getTransaction() );
        final String[] rawProps = this.parameter.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getNullable( getTransaction() ), is( Nullable.DEFAULT_VALUE ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat( this.parameter.getParent( getTransaction() ), is( this.procedure ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.parameter.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.parameter.getPropertyNames( getTransaction() );
        final Filter[] filters = this.parameter.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.parameter.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.parameter.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDefaultValue( getTransaction() ), is( nullValue() ) );
        assertThat( this.parameter.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    public void shouldRemoveOptionWithEmptyStatementOptionValue() throws Exception {
        final String name = "blah";
        this.parameter.setStatementOption( getTransaction(), name, "blah" );
        this.parameter.setStatementOption( getTransaction(), name, StringConstants.EMPTY_STRING );
        assertThat( this.parameter.getProperty( getTransaction(), name ), is( nullValue() ) );
    }

    public void shouldRemoveOptionWithNullStatementOptionValue() throws Exception {
        final String name = "blah";
        this.parameter.setStatementOption( getTransaction(), name, "blah" );
        this.parameter.setStatementOption( getTransaction(), name, null );
        assertThat( this.parameter.getProperty( getTransaction(), name ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.parameter.setStatementOption( getTransaction(), name, "blah" );
        this.parameter.removeStatementOption( getTransaction(), name );
        assertThat( this.parameter.getStatementOptions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = 10;
        this.parameter.setLength( getTransaction(), value );
        assertThat( this.parameter.getLength( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.parameter.setDatatypeName( getTransaction(), value );
        assertThat( this.parameter.getDatatypeName( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_NAME ).getStringValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final long value = 10;
        this.parameter.setPrecision( getTransaction(), value );
        assertThat( this.parameter.getPrecision( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final long value = 10;
        this.parameter.setScale( getTransaction(), value );
        assertThat( this.parameter.getScale( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.parameter.setDefaultValue( getTransaction(), value );
        assertThat( this.parameter.getDefaultValue( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDirectionProperty() throws Exception {
        final Direction value = Direction.IN_OUT;
        this.parameter.setDirection( getTransaction(), value );
        assertThat( this.parameter.getDirection( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.parameter.setNullable( getTransaction(), value );
        assertThat( this.parameter.getNullable( getTransaction() ), is( value ) );
        assertThat( this.parameter.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetResult() throws Exception {
        final boolean value = !Parameter.DEFAULT_RESULT;
        this.parameter.setResult( getTransaction(), value );
        assertThat( this.parameter.isResult( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.parameter.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.parameter.getPropertyDescriptors( getTransaction() );
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
    public void shouldIncludeCustomOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.parameter.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        for ( final String prop : this.parameter.getPropertyNames( getTransaction() ) ) {
            if ( custom.equals( prop ) ) {
                if ( customFound ) {
                    fail( "Custom option included multiple times in property names" );
                }

                customFound = true;
                break;
            }
        }

        if ( !customFound ) {
            fail( "Custom option not included in property names" );
        }
    }

    @Test
    public void shouldObtainCustomOptions() throws Exception {
        final String sledge = "sledge";
        this.parameter.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.parameter.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.parameter.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.parameter.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.parameter.setStatementOption( getTransaction(), custom, "hammer" );

        assertThat( this.parameter.getPropertyDescriptor( getTransaction(), custom ), is( notNullValue() ) );
        assertThat( this.parameter.getPropertyDescriptor( getTransaction(), custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.parameter.setStatementOption( getTransaction(), custom, "sledge" );

        assertThat( this.parameter.getStatementOptionNames( getTransaction() ).length, is( 1 ) );
        assertThat( Arrays.asList( this.parameter.getStatementOptionNames( getTransaction() ) ), hasItem( custom ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.parameter.rename( getTransaction(), newName );
        assertThat( this.parameter.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.parameter.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.parameter.setProperty( getTransaction(), option, value );

        assertThat( this.parameter.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.parameter.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.parameter.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.parameter.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.parameter.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldNotHaveStandardOptionNames() throws Exception {
        assertThat( this.parameter.getStandardOptions().isEmpty(), is( true ) );
    }

}
