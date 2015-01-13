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
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;

@SuppressWarnings( {"javadoc", "nls"} )
public class ProcedureResultSetImplTest extends RelationalModelTest {

    private ProcedureResultSet modelObject;
    private Procedure parent;

    @Before
    public void init() throws Exception {
        this.parent = RelationalModelFactory.createProcedure(null, _repo, null, "procedure");
        this.modelObject = RelationalModelFactory.createProcedureResultSet(null, _repo, this.parent);
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingAccessPattern() throws KException {
        this.modelObject.addAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingForeignKey() throws KException {
        this.modelObject.addForeignKey(null, "blah", mock(Table.class));
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingUniqueConstraint() throws KException {
        this.modelObject.addUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingAccessPattern() throws KException {
        this.modelObject.removeAccessPattern(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingForeignKey() throws KException {
        this.modelObject.removeForeignKey(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingUniqueConstraint() throws KException {
        this.modelObject.removeUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenSettingPrimaryKey() throws KException {
        this.modelObject.setPrimaryKey(null, "blah");
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat(this.modelObject.getProcedure(null), is(this.parent));
    }

}
