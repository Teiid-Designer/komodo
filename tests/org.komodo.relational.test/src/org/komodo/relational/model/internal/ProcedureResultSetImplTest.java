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
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( {"javadoc", "nls"} )
public class ProcedureResultSetImplTest extends RelationalModelTest {

    private Procedure procedure;
    private ProcedureResultSet procedureResultSet;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createProcedure(null, _repo, mock(Model.class), "procedure");
        this.procedureResultSet = RelationalModelFactory.createProcedureResultSet(null, _repo, this.procedure);
    }

    @Test
    public void shouldFailConstructionIfNotProcedureResultSet() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ProcedureResultSetImpl(null, _repo, this.procedure.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingAccessPattern() throws KException {
        this.procedureResultSet.addAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingForeignKey() throws KException {
        this.procedureResultSet.addForeignKey(null, "blah", mock(Table.class));
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingUniqueConstraint() throws KException {
        this.procedureResultSet.addUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingAccessPattern() throws KException {
        this.procedureResultSet.removeAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingForeignKey() throws KException {
        this.procedureResultSet.removeForeignKey(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingUniqueConstraint() throws KException {
        this.procedureResultSet.removeUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenSettingPrimaryKey() throws KException {
        this.procedureResultSet.setPrimaryKey(null, "blah");
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.procedureResultSet.hasDescriptor(null, CreateProcedure.RESULT_DATA_TYPE), is(true));
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat(this.procedureResultSet.getProcedure(null), is(this.procedure));
    }

}
