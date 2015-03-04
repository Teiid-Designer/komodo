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
import org.komodo.spi.repository.Repository;

@SuppressWarnings( {"javadoc", "nls"} )
public final class StoredProcedureImplTest extends RelationalModelTest {

    private StoredProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( null, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( null, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createStoredProcedure( null, _repo, model, "procedure" );
    }

    @Test
    public void shouldFailConstructionIfNotStoredProcedure() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new StoredProcedureImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.procedure.removeResultSet( null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNativeQueryWithEmptyValueWhenItWasNeverAdded() throws Exception {
        this.procedure.setNativeQuery( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNativeQueryWhenNeverAdded() throws Exception {
        this.procedure.setNativeQuery( null, null );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.procedure.setResultSet( null, TabularResultSet.class );
        assertThat(this.procedure.getChildren( null ).length, is(1));
        assertThat(this.procedure.getChildren( null )[0], is((KomodoObject)resultSet));
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.procedure.addParameter( null, "param" );
        this.procedure.setResultSet( null, DataTypeResultSet.class );
        assertThat(this.procedure.getChildren( null ).length, is(2));
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( null ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveDefaultNonPreparedAfterConstruction() throws Exception {
        assertThat( this.procedure.isNonPrepared( null ), is( StoredProcedure.DEFAULT_NON_PREPARED ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.procedure.getResultSet( null ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        this.procedure.setResultSet( uow, TabularResultSet.class );
        this.procedure.removeResultSet( uow );
        uow.commit();

        assertThat( this.procedure.getResultSet( null ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        assertThat( this.procedure.setResultSet( uow, DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( uow ), is( instanceOf( DataTypeResultSet.class ) ) );
        uow.commit();
    }

    @Test
    public void shouldSetNativeQuery() throws Exception {
        final String value = "nativeQuery";
        this.procedure.setNativeQuery( null, value );
        assertThat( this.procedure.getNativeQuery( null ), is( value ) );
    }

    @Test
    public void shouldSetNonPrepared() throws Exception {
        final boolean value = !StoredProcedure.DEFAULT_NON_PREPARED;
        this.procedure.setNonPrepared( null, value );
        assertThat( this.procedure.isNonPrepared( null ), is( value ) );
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        assertThat( this.procedure.setResultSet( uow, TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( uow ), is( instanceOf( TabularResultSet.class ) ) );
        uow.commit();
    }

}
