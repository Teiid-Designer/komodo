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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.RelationalModelFactory;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;

@SuppressWarnings( {"javadoc", "nls"} )
public class UniqueConstraintImplTest extends RelationalModelTest {

    private static final String NAME = "uniqueconstraint";

    private Table parentTable;
    private UniqueConstraint modelObject;

    @Before
    public void init() throws Exception {
        this.parentTable = RelationalModelFactory.createTable(null, _repo, null, "parentTable");
        this.modelObject = RelationalModelFactory.createUniqueConstraint(null, _repo, this.parentTable, NAME);
    }

    @Test
    public void shouldHaveCorrectConstraintType() {
        assertThat(this.modelObject.getConstraintType(), is(TableConstraint.ConstraintType.UNIQUE));
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat(this.modelObject.getTable(null), is(this.parentTable));
    }

}
