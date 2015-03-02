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
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings( {"javadoc", "nls"} )
public final class UserDefinedFunctionImplTest extends RelationalModelTest {

    private UserDefinedFunction function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createUserDefinedFunction(null, _repo, mock(Model.class), "function");
    }

    @Test
    public void shouldFailConstructionIfNotUserDefinedFunction() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new UserDefinedFunctionImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( null ), is( SchemaElementType.VIRTUAL ) );
    }

    /////////////////////////////////////////////////
    // CATEGORY Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyCategoryWhenRemoving() throws Exception {
        this.function.setCategory(null, "blah");
        this.function.setCategory(null, StringConstants.EMPTY_STRING);
        assertThat(this.function.getCategory(null), is(nullValue()));
    }

    @Test
    public void shouldAllowNullCategoryWhenRemoving() throws Exception {
        this.function.setCategory(null, "blah");
        this.function.setCategory(null, null);
        assertThat(this.function.getCategory(null), is(nullValue()));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyCategoryIfNotSet() throws Exception {
        this.function.setCategory(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullCategoryIfNotSet() throws Exception {
        this.function.setCategory(null, null);
    }

    @Test
    public void shouldSetCategory() throws Exception {
        final String value = "category";
        this.function.setCategory(null, value);
        assertThat(this.function.getCategory(null), is(value));
    }

    /////////////////////////////////////////////////
    // JAVA_CLASS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass(null, "blah");
        this.function.setJavaClass(null, StringConstants.EMPTY_STRING);
        assertThat(this.function.getJavaClass(null), is(nullValue()));
    }

    @Test
    public void shouldAllowNullJavaClassWhenRemoving() throws Exception {
        this.function.setJavaClass(null, "blah");
        this.function.setJavaClass(null, null);
        assertThat(this.function.getJavaClass(null), is(nullValue()));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaClassIfNotSet() throws Exception {
        this.function.setJavaClass(null, null);
    }

    @Test
    public void shouldSetJavaClass() throws Exception {
        final String value = "javaClass";
        this.function.setJavaClass(null, value);
        assertThat(this.function.getJavaClass(null), is(value));
    }

    /////////////////////////////////////////////////
    // JAVA_METHOD Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldAllowEmptyJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod(null, "blah");
        this.function.setJavaMethod(null, StringConstants.EMPTY_STRING);
        assertThat(this.function.getJavaMethod(null), is(nullValue()));
    }

    @Test
    public void shouldAllowNullJavaMethodWhenRemoving() throws Exception {
        this.function.setJavaMethod(null, "blah");
        this.function.setJavaMethod(null, null);
        assertThat(this.function.getJavaMethod(null), is(nullValue()));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullJavaMethodIfNotSet() throws Exception {
        this.function.setJavaMethod(null, null);
    }

    @Test
    public void shouldSetJavaMethod() throws Exception {
        final String value = "javaMethod";
        this.function.setJavaMethod(null, value);
        assertThat(this.function.getJavaMethod(null), is(value));
    }

}
