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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;

@SuppressWarnings( { "javadoc", "nls" } )
public final class StoredProcedureImplTest extends RelationalModelTest {

    private StoredProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createStoredProcedure( this.uow, _repo, model, "procedure" );
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

}
