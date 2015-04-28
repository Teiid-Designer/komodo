/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.RelationalObject.Filter;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

@SuppressWarnings( {"javadoc", "nls"} )
public final class PushdownFunctionImplTest extends RelationalModelTest {

    private PushdownFunction function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createPushdownFunction( null, _repo, mock( Model.class ), "function" );
    }

    @Test
    public void shouldFailConstructionIfNotPushdownFunction() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new PushdownFunctionImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.function.removeResultSet( null );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.function.setResultSet( null, TabularResultSet.class );
        assertThat(this.function.getChildren( null ).length, is(1));
        assertThat(this.function.getChildren( null )[0], is((KomodoObject)resultSet));
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.function.addParameter( null, "param" );
        this.function.setResultSet( null, DataTypeResultSet.class );
        assertThat(this.function.getChildren( null ).length, is(2));
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( null ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( null );
        final String[] rawProps = this.function.getRawPropertyNames( null );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( null );
        final Filter[] filters = this.function.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setAggregate( null, true );
        this.function.setStatementOption( null, "sledge", "hammer" );
        assertThat( this.function.getChildren( null ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.function.getResultSet( null ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        this.function.setResultSet( uow, TabularResultSet.class );
        this.function.removeResultSet( uow );
        uow.commit();

        assertThat( this.function.getResultSet( null ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        assertThat( this.function.setResultSet( uow, DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( uow ), is( instanceOf( DataTypeResultSet.class ) ) );
        uow.commit();
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        assertThat( this.function.setResultSet( uow, TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( uow ), is( instanceOf( TabularResultSet.class ) ) );
        uow.commit();
    }

}
