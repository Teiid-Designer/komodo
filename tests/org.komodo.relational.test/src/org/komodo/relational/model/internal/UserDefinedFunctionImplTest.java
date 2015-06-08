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
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;

@SuppressWarnings( { "javadoc", "nls" } )
public final class UserDefinedFunctionImplTest extends RelationalModelTest {

    private UserDefinedFunction function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createUserDefinedFunction( this.uow, _repo, mock( Model.class ), "function" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotUserDefinedFunction() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new UserDefinedFunctionImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( this.uow ), is( SchemaElementType.VIRTUAL ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.function.getTypeIdentifier( this.uow ), is(KomodoType.USER_DEFINED_FUNCTION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( this.uow );
        final String[] rawProps = this.function.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( this.uow );
        final Filter[] filters = this.function.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    /////////////////////////////////////////////////
    // CATEGORY Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyCategoryWhenRemoving() throws Exception {
        this.function.setCategory( this.uow, "blah" );
        this.function.setCategory( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.function.getCategory( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullCategoryWhenRemoving() throws Exception {
        this.function.setCategory( this.uow, "blah" );
        this.function.setCategory( this.uow, null );
        assertThat( this.function.getCategory( this.uow ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyCategoryIfNotSet() throws Exception {
        this.function.setCategory( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullCategoryIfNotSet() throws Exception {
        this.function.setCategory( this.uow, null );
    }

    @Test
    public void shouldSetCategory() throws Exception {
        final String value = "category";
        this.function.setCategory( this.uow, value );
        assertThat( this.function.getCategory( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // JAVA_CLASS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass( this.uow, "blah" );
        this.function.setJavaClass( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.function.getJavaClass( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass( this.uow, "blah" );
        this.function.setJavaClass( this.uow, null );
        assertThat( this.function.getJavaClass( this.uow ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass( this.uow, null );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setCategory( this.uow, "blah" );
        this.function.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.function.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetJavaClass() throws Exception {
        final String value = "javaClass";
        this.function.setJavaClass( this.uow, value );
        assertThat( this.function.getJavaClass( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // JAVA_METHOD Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod( this.uow, "blah" );
        this.function.setJavaMethod( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.function.getJavaMethod( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod( this.uow, "blah" );
        this.function.setJavaMethod( this.uow, null );
        assertThat( this.function.getJavaMethod( this.uow ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod( this.uow, null );
    }

    @Test
    public void shouldSetJavaMethod() throws Exception {
        final String value = "javaMethod";
        this.function.setJavaMethod( this.uow, value );
        assertThat( this.function.getJavaMethod( this.uow ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.function.setStatementOption( this.uow, customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( this.uow );
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
        this.function.setStatementOption( this.uow, custom, "sledge" );
        boolean customFound = false;

        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( this.uow, standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.function.getPropertyNames( this.uow ) ) {
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
        final String[] optionNames = this.function.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.function.getPrimaryType( this.uow ).getPropertyDescriptors( this.uow );

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
        final String[] optionNames = this.function.getStandardOptionNames();
        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( this.uow );

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
        this.function.setStatementOption( this.uow, sledge, "hammer" );

        final String elvis = "elvis";
        this.function.setStatementOption( this.uow, elvis, "presley" );

        assertThat( this.function.getCustomOptions( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( this.uow ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.function.setStatementOption( this.uow, custom, "hammer" );

        assertThat( this.function.getPropertyDescriptor( this.uow, custom ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( this.uow, custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( this.uow, standard, "blah" );

        assertThat( this.function.getPropertyDescriptor( this.uow, standard ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( this.uow, standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.function.setStatementOption( this.uow, custom, "sledge" );

        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( this.uow, standard, "hammer" );

        assertThat( this.function.getStatementOptionNames( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( this.uow ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptionNames()[0];
        final String value = "newValue";
        this.function.setProperty( this.uow, option, value ); // add
        this.function.setProperty( this.uow, option, (Object)null ); // remove
        assertThat( this.function.hasProperty( this.uow, option ), is( false ) );
        assertThat( this.function.hasChild( this.uow, option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.function.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( this.uow, option, value );

        assertThat( this.function.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.function.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.getStatementOptions( this.uow ).length, is( 1 ) );
        assertThat( this.function.isCustomOption( this.uow, option ), is( true ) );

        final StatementOption statementOption = this.function.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( this.uow, option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( this.uow, option, value );

        assertThat( this.function.hasProperty( this.uow, option ), is( true ) );
        assertThat( this.function.getProperty( this.uow, option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.isCustomOption( this.uow, option ), is( false ) );
        assertThat( this.function.getStatementOptions( this.uow ).length, is( 1 ) );

        final StatementOption statementOption = this.function.getStatementOptions( this.uow )[0];
        assertThat( statementOption.getName( this.uow ), is( option ) );
        assertThat( statementOption.getValue( this.uow ), is( ( Object )value ) );
    }

}
