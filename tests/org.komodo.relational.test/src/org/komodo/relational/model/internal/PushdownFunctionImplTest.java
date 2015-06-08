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
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class PushdownFunctionImplTest extends RelationalModelTest {

    private PushdownFunction function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createPushdownFunction( this.uow, _repo, mock( Model.class ), "function" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotPushdownFunction() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new PushdownFunctionImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.function.removeResultSet( this.uow );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.function.setResultSet( this.uow, TabularResultSet.class );
        assertThat( this.function.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.function.getChildren( this.uow )[0], is( ( KomodoObject )resultSet ) );
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.function.addParameter( this.uow, "param" );
        this.function.setResultSet( this.uow, DataTypeResultSet.class );
        assertThat( this.function.getChildren( this.uow ).length, is( 2 ) );
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( this.uow ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.function.getTypeIdentifier( this.uow ), is(KomodoType.PUSHDOWN_FUNCTION));
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

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setAggregate( this.uow, true );
        this.function.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.function.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.function.getResultSet( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        this.function.setResultSet( this.uow, TabularResultSet.class );
        this.function.removeResultSet( this.uow );
        assertThat( this.function.getResultSet( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        assertThat( this.function.setResultSet( this.uow, DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( this.uow ), is( instanceOf( DataTypeResultSet.class ) ) );
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        assertThat( this.function.setResultSet( this.uow, TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( this.uow ), is( instanceOf( TabularResultSet.class ) ) );
    }

}
