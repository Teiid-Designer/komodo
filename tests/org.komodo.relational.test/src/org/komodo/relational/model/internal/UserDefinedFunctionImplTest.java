/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;

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

}
