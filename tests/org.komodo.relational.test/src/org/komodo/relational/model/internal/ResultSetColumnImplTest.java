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
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ResultSetColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private ResultSetColumn column;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "path" );
        final Model model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        final StoredProcedure procedure = RelationalModelFactory.createStoredProcedure( this.uow, _repo, model, "procedure" );
        this.resultSet = RelationalModelFactory.createTabularResultSet( this.uow, _repo, procedure );
        this.column = RelationalModelFactory.createResultSetColumn( this.uow, _repo, this.resultSet, NAME );
        commit();
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( this.uow, "blah" );
        this.column.setDescription( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( this.uow, "blah" );
        this.column.setNameInSource( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( this.uow, "blah" );
        this.column.setDescription( this.uow, null );
        assertThat( this.column.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( this.uow, "blah" );
        this.column.setNameInSource( this.uow, null );
        assertThat( this.column.getNameInSource( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotResultSetColumn() {
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
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.column.getTypeIdentifier( this.uow ), is(KomodoType.RESULT_SET_COLUMN));
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
    public void shouldHaveParentResultSet() throws Exception {
        assertThat( this.column.getParent( this.uow ), is( instanceOf( TabularResultSet.class ) ) );
        assertThat( this.column.getParent( this.uow ), is( ( KomodoObject )this.resultSet ) );
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
        this.column.setUuid( this.uow, "elvis" );
        this.column.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.column.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( this.uow ), is( nullValue() ) );
        assertThat( this.column.hasProperty( this.uow, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
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
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( this.uow, value );
        assertThat( this.column.getNameInSource( this.uow ), is( value ) );
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
