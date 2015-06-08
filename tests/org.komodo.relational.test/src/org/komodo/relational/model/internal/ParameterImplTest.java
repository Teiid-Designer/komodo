/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ParameterImplTest extends RelationalModelTest {

    private static final String NAME = "parameter";

    private Parameter parameter;
    private AbstractProcedure procedure;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createVirtualProcedure( this.uow, _repo, mock( Model.class ), "procedure" );
        this.parameter = RelationalModelFactory.createParameter( this.uow, _repo, this.procedure, NAME );
        commit();
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.parameter.setStatementOption( this.uow, name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( this.uow ), is( name ) );
        assertThat( statementOption.getOption( this.uow ), is( value ) );
    }

    @Test
    public void shouldAllowEmptyDatatypeName() throws Exception {
        this.parameter.setDatatypeName( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowEmptyDefaultValue() throws Exception {
        this.parameter.setDefaultValue( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowNullDatatypeName() throws Exception {
        this.parameter.setDatatypeName( this.uow, null );
    }

    @Test
    public void shouldAllowNullDefaultValue() throws Exception {
        this.parameter.setDefaultValue( this.uow, null );
    }

    @Test
    public void shouldAllowNullDirection() throws Exception {
        this.parameter.setDirection( this.uow, null );
    }

    @Test
    public void shouldAllowNullNullable() throws Exception {
        this.parameter.setNullable( this.uow, null );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.parameter.isChildRestricted(), is( true ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.parameter.setStatementOption( this.uow, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.parameter.setStatementOption( this.uow, null, "blah" );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValueWhenNeverAdded() throws Exception {
        this.parameter.setStatementOption( this.uow, "blah", null );
    }

    @Test
    public void shouldFailConstructionIfNotParameter() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ParameterImpl( this.uow, _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.parameter.setStatementOption( this.uow, "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.parameter.removeStatementOption( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.parameter.removeStatementOption( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.parameter.removeStatementOption( this.uow, "unknown" );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.parameter.setStatementOption( this.uow, "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.parameter.getStatementOptions( this.uow ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.parameter.hasDescriptor( this.uow, CreateProcedure.PARAMETER ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.parameter.getTypeIdentifier( this.uow ), is(KomodoType.PARAMETER));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getLength( this.uow ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDatatypeName( this.uow ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getPrecision( this.uow ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getScale( this.uow ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultResultAfterConstruction() throws Exception {
        assertThat( this.parameter.isResult( this.uow ), is( Parameter.DEFAULT_RESULT ) );
    }

    @Test
    public void shouldHaveDirectionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDirection( this.uow ), is( Direction.DEFAULT_VALUE ) );
        assertThat( this.parameter.hasProperty( this.uow, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ), is( true ) );
        assertThat( this.parameter.getProperty( this.uow, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ).getStringValue( this.uow ),
                    is( Direction.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.parameter.getPropertyNames( this.uow );
        final String[] rawProps = this.parameter.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getNullable( this.uow ), is( Nullable.DEFAULT_VALUE ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.NULLABLE ).getStringValue( this.uow ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat( ( AbstractProcedure )this.parameter.getParent( this.uow ), is( this.procedure ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.parameter.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.parameter.getPropertyNames( this.uow );
        final Filter[] filters = this.parameter.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.parameter.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.parameter.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.parameter.getDefaultValue( this.uow ), is( nullValue() ) );
        assertThat( this.parameter.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    public void shouldRemoveOptionWithEmptyStatementOptionValue() throws Exception {
        final String name = "blah";
        this.parameter.setStatementOption( this.uow, name, "blah" );
        this.parameter.setStatementOption( this.uow, name, StringConstants.EMPTY_STRING );
        assertThat( this.parameter.getProperty( this.uow, name ), is( nullValue() ) );
    }

    public void shouldRemoveOptionWithNullStatementOptionValue() throws Exception {
        final String name = "blah";
        this.parameter.setStatementOption( this.uow, name, "blah" );
        this.parameter.setStatementOption( this.uow, name, null );
        assertThat( this.parameter.getProperty( this.uow, name ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.parameter.setStatementOption( this.uow, name, "blah" );
        this.parameter.removeStatementOption( this.uow, name );
        assertThat( this.parameter.getStatementOptions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = 10;
        this.parameter.setLength( this.uow, value );
        assertThat( this.parameter.getLength( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( this.uow ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.parameter.setDatatypeName( this.uow, value );
        assertThat( this.parameter.getDatatypeName( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.DATATYPE_NAME ).getStringValue( this.uow ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.parameter.setPrecision( this.uow, value );
        assertThat( this.parameter.getPrecision( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( this.uow ),
                    is( ( long )value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.parameter.setScale( this.uow, value );
        assertThat( this.parameter.getScale( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( this.uow ),
                    is( ( long )value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.parameter.setDefaultValue( this.uow, value );
        assertThat( this.parameter.getDefaultValue( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( this.uow ),
                    is( value ) );
    }

    @Test
    public void shouldSetDirectionProperty() throws Exception {
        final Direction value = Direction.IN_OUT;
        this.parameter.setDirection( this.uow, value );
        assertThat( this.parameter.getDirection( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE ).getStringValue( this.uow ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.parameter.setNullable( this.uow, value );
        assertThat( this.parameter.getNullable( this.uow ), is( value ) );
        assertThat( this.parameter.getProperty( this.uow, StandardDdlLexicon.NULLABLE ).getStringValue( this.uow ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetResult() throws Exception {
        final boolean value = !Parameter.DEFAULT_RESULT;
        this.parameter.setResult( this.uow, value );
        assertThat( this.parameter.isResult( this.uow ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.parameter.setStatementOption( this.uow, customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.parameter.getPropertyDescriptors( this.uow );
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
        this.parameter.setStatementOption( this.uow, custom, "sledge" );
        boolean customFound = false;

        for ( final String prop : this.parameter.getPropertyNames( this.uow ) ) {
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
        this.parameter.setStatementOption( this.uow, sledge, "hammer" );

        final String elvis = "elvis";
        this.parameter.setStatementOption( this.uow, elvis, "presley" );

        assertThat( this.parameter.getCustomOptions( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.parameter.getStatementOptionNames( this.uow ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.parameter.setStatementOption( this.uow, custom, "hammer" );

        assertThat( this.parameter.getPropertyDescriptor( this.uow, custom ), is( notNullValue() ) );
        assertThat( this.parameter.getPropertyDescriptor( this.uow, custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.parameter.setStatementOption( this.uow, custom, "sledge" );

        assertThat( this.parameter.getStatementOptionNames( this.uow ).length, is( 1 ) );
        assertThat( Arrays.asList( this.parameter.getStatementOptionNames( this.uow ) ), hasItem( custom ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.parameter.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.parameter.setProperty( this.uow, option, value );

        assertThat( this.parameter.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.parameter.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.parameter.getStatementOptions( this.uow ).length, is( 1 ) );
        assertThat( this.parameter.isCustomOption( this.uow, option ), is( true ) );

        final StatementOption statementOption = this.parameter.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

    @Test
    public void shouldNotHaveStandardOptionNames() throws Exception {
        assertThat( this.parameter.getStandardOptionNames().length, is( 0 ) );
    }

}
