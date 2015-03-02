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
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( {"javadoc", "nls"} )
public class TabularResultSetImplTest extends RelationalModelTest {

    private StoredProcedure procedure;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createStoredProcedure(null, _repo, mock(Model.class), "procedure");
        this.resultSet = RelationalModelFactory.createTabularResultSet(null, _repo, this.procedure);
    }

    @Test
    public void shouldFailConstructionIfNotDataTypeResultSet() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new TabularResultSetImpl(null, _repo, this.procedure.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingAccessPattern() throws KException {
        this.resultSet.addAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingForeignKey() throws KException {
        this.resultSet.addForeignKey(null, "blah", mock(Table.class));
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingUniqueConstraint() throws KException {
        this.resultSet.addUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingAccessPattern() throws KException {
        this.resultSet.removeAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingForeignKey() throws KException {
        this.resultSet.removeForeignKey(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingUniqueConstraint() throws KException {
        this.resultSet.removeUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenSettingPrimaryKey() throws KException {
        this.resultSet.setPrimaryKey(null, "blah");
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat(this.resultSet.getName(null), is(CreateProcedure.RESULT_SET));
    }

}
