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
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;

@SuppressWarnings( { "javadoc", "nls" } )
public final class StoredProcedureImplTest extends RelationalModelTest {

    private StoredProcedure procedure;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addStoredProcedure( this.uow, "procedure" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotStoredProcedure() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new StoredProcedureImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.procedure.removeResultSet( this.uow );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNativeQueryWithEmptyValueWhenItWasNeverAdded() throws Exception {
        this.procedure.setNativeQuery( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNativeQueryWhenNeverAdded() throws Exception {
        this.procedure.setNativeQuery( this.uow, null );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.procedure.setResultSet( this.uow, TabularResultSet.class );
        assertThat( this.procedure.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.procedure.getChildren( this.uow )[0], is( ( KomodoObject )resultSet ) );
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.procedure.addParameter( this.uow, "param" );
        this.procedure.setResultSet( this.uow, DataTypeResultSet.class );
        assertThat( this.procedure.getChildren( this.uow ).length, is( 2 ) );
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( this.uow ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.procedure.getTypeIdentifier( this.uow ), is(KomodoType.STORED_PROCEDURE));
    }

    @Test
    public void shouldHaveDefaultNonPreparedAfterConstruction() throws Exception {
        assertThat( this.procedure.isNonPrepared( this.uow ), is( StoredProcedure.DEFAULT_NON_PREPARED ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( this.uow );
        final String[] rawProps = this.procedure.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( this.uow );
        final Filter[] filters = this.procedure.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.procedure.setNativeQuery( this.uow, "blah" );
        this.procedure.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.procedure.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.procedure.getResultSet( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        this.procedure.setResultSet( this.uow, TabularResultSet.class );
        this.procedure.removeResultSet( this.uow );
        assertThat( this.procedure.getResultSet( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        assertThat( this.procedure.setResultSet( this.uow, DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( this.uow ), is( instanceOf( DataTypeResultSet.class ) ) );
    }

    @Test
    public void shouldSetNativeQuery() throws Exception {
        final String value = "nativeQuery";
        this.procedure.setNativeQuery( this.uow, value );
        assertThat( this.procedure.getNativeQuery( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNonPrepared() throws Exception {
        final boolean value = !StoredProcedure.DEFAULT_NON_PREPARED;
        this.procedure.setNonPrepared( this.uow, value );
        assertThat( this.procedure.isNonPrepared( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        assertThat( this.procedure.setResultSet( this.uow, TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( this.uow ), is( instanceOf( TabularResultSet.class ) ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.procedure.setStatementOption( this.uow, customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.procedure.getPropertyDescriptors( this.uow );
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
        this.procedure.setStatementOption( this.uow, custom, "sledge" );
        boolean customFound = false;

        final String standard = this.procedure.getStandardOptionNames()[0];
        this.procedure.setStatementOption( this.uow, standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.procedure.getPropertyNames( this.uow ) ) {
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
        final String[] optionNames = this.procedure.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.procedure.getPrimaryType( this.uow ).getPropertyDescriptors( this.uow );

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
        final String[] optionNames = this.procedure.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.procedure.getPropertyDescriptors( this.uow );

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
        this.procedure.setStatementOption( this.uow, sledge, "hammer" );

        final String elvis = "elvis";
        this.procedure.setStatementOption( this.uow, elvis, "presley" );

        assertThat( this.procedure.getCustomOptions( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.procedure.getStatementOptionNames( this.uow ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.procedure.setStatementOption( this.uow, custom, "hammer" );

        assertThat( this.procedure.getPropertyDescriptor( this.uow, custom ), is( notNullValue() ) );
        assertThat( this.procedure.getPropertyDescriptor( this.uow, custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.procedure.getStandardOptionNames()[0];
        this.procedure.setStatementOption( this.uow, standard, "blah" );

        assertThat( this.procedure.getPropertyDescriptor( this.uow, standard ), is( notNullValue() ) );
        assertThat( this.procedure.getPropertyDescriptor( this.uow, standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.procedure.setStatementOption( this.uow, custom, "sledge" );

        final String standard = this.procedure.getStandardOptionNames()[0];
        this.procedure.setStatementOption( this.uow, standard, "hammer" );

        assertThat( this.procedure.getStatementOptionNames( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.procedure.getStatementOptionNames( this.uow ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.procedure.getStandardOptionNames()[0];
        final String value = "newValue";
        this.procedure.setProperty( this.uow, option, value ); // add
        this.procedure.setProperty( this.uow, option, (Object)null ); // remove
        assertThat( this.procedure.hasProperty( this.uow, option ), is( false ) );
        assertThat( this.procedure.hasChild( this.uow, option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.procedure.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.procedure.setProperty( this.uow, option, value );

        assertThat( this.procedure.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.procedure.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.procedure.getStatementOptions( this.uow ).length, is( 1 ) );
        assertThat( this.procedure.isCustomOption( this.uow, option ), is( true ) );

        final StatementOption statementOption = this.procedure.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.procedure.getStandardOptionNames()[0];
        this.procedure.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.procedure.setProperty( this.uow, option, value );

        assertThat( this.procedure.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.procedure.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.procedure.isCustomOption( this.uow, option ), is( false ) );
        assertThat( this.procedure.getStatementOptions( this.uow ).length, is( 1 ) );

        final StatementOption statementOption = this.procedure.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = StoredProcedureImpl.RESOLVER.create( this.uow,
                                                                          _repo,
                                                                          this.procedure.getParent( this.uow ),
                                                                          name,
                                                                          null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( StoredProcedure.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        StoredProcedureImpl.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
