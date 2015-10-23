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
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;

@SuppressWarnings( { "javadoc", "nls" } )
public final class UserDefinedFunctionImplTest extends RelationalModelTest {

    private UserDefinedFunction function;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.function = model.addUserDefinedFunction( getTransaction(), "function" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotUserDefinedFunction() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new UserDefinedFunctionImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( getTransaction() ), is( SchemaElementType.VIRTUAL ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.function.getTypeIdentifier( getTransaction() ), is(KomodoType.USER_DEFINED_FUNCTION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
        final String[] rawProps = this.function.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
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
        this.function.setCategory( getTransaction(), "blah" );
        this.function.setCategory( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.function.getCategory( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullCategoryWhenRemoving() throws Exception {
        this.function.setCategory( getTransaction(), "blah" );
        this.function.setCategory( getTransaction(), null );
        assertThat( this.function.getCategory( getTransaction() ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyCategoryIfNotSet() throws Exception {
        this.function.setCategory( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullCategoryIfNotSet() throws Exception {
        this.function.setCategory( getTransaction(), null );
    }

    @Test
    public void shouldSetCategory() throws Exception {
        final String value = "category";
        this.function.setCategory( getTransaction(), value );
        assertThat( this.function.getCategory( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // JAVA_CLASS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass( getTransaction(), "blah" );
        this.function.setJavaClass( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.function.getJavaClass( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass( getTransaction(), "blah" );
        this.function.setJavaClass( getTransaction(), null );
        assertThat( this.function.getJavaClass( getTransaction() ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass( getTransaction(), null );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setCategory( getTransaction(), "blah" );
        this.function.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.function.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldSetJavaClass() throws Exception {
        final String value = "javaClass";
        this.function.setJavaClass( getTransaction(), value );
        assertThat( this.function.getJavaClass( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // JAVA_METHOD Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod( getTransaction(), "blah" );
        this.function.setJavaMethod( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.function.getJavaMethod( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod( getTransaction(), "blah" );
        this.function.setJavaMethod( getTransaction(), null );
        assertThat( this.function.getJavaMethod( getTransaction() ), is( nullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod( getTransaction(), null );
    }

    @Test
    public void shouldSetJavaMethod() throws Exception {
        final String value = "javaMethod";
        this.function.setJavaMethod( getTransaction(), value );
        assertThat( this.function.getJavaMethod( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.function.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( getTransaction() );
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
        this.function.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( getTransaction(), standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.function.getPropertyNames( getTransaction() ) ) {
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
        final PropertyDescriptor[] propDescriptors = this.function.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );

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
        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( getTransaction() );

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
        this.function.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.function.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.function.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.function.setStatementOption( getTransaction(), custom, "hammer" );

        assertThat( this.function.getPropertyDescriptor( getTransaction(), custom ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( getTransaction(), custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( getTransaction(), standard, "blah" );

        assertThat( this.function.getPropertyDescriptor( getTransaction(), standard ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( getTransaction(), standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.function.setStatementOption( getTransaction(), custom, "sledge" );

        final String standard = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( getTransaction(), standard, "hammer" );

        assertThat( this.function.getStatementOptionNames( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( getTransaction() ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptionNames()[0];
        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value ); // add
        this.function.setProperty( getTransaction(), option, (Object)null ); // remove
        assertThat( this.function.hasProperty( getTransaction(), option ), is( false ) );
        assertThat( this.function.hasChild( getTransaction(), option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.function.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value );

        assertThat( this.function.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.function.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.function.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.function.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptionNames()[0];
        this.function.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value );

        assertThat( this.function.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.function.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.isCustomOption( getTransaction(), option ), is( false ) );
        assertThat( this.function.getStatementOptions( getTransaction() ).length, is( 1 ) );

        final StatementOption statementOption = this.function.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = UserDefinedFunction.RESOLVER.create( getTransaction(),
                                                                              _repo,
                                                                              this.function.getParent( getTransaction() ),
                                                                              name,
                                                                              null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( UserDefinedFunction.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( getTransaction(), null, "bogus", null );
        UserDefinedFunction.RESOLVER.create( getTransaction(), _repo, bogusParent, "blah", null );
    }

}
