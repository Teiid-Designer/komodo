/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;

@SuppressWarnings( {"javadoc", "nls"} )
public class TabularResultSetImplTest extends RelationalModelTest {

    private StoredProcedure procedure;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( null, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( null, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createStoredProcedure( null, _repo, model, "procedure" );
        this.resultSet = RelationalModelFactory.createTabularResultSet( null, _repo, this.procedure );
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String resultSetColumnName = "resultSetColumn";
        final Repository.UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );
        final ResultSetColumn resultSetColumn = this.resultSet.addColumn( uow, resultSetColumnName );
        assertThat( this.resultSet.getColumns( uow ).length, is( 1 ) );
        assertThat( resultSetColumn.getName( uow ), is( resultSetColumnName ) );
        uow.commit();

    }

    @Test
    public void shouldFailConstructionIfNotTabularResultSet() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new TabularResultSetImpl( null, _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

}
