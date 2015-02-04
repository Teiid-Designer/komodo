/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;

@SuppressWarnings( {"javadoc", "nls"} )
public class ViewImplTest extends RelationalModelTest {

    private View view;

    @Before
    public void init() throws Exception {
        this.view = RelationalModelFactory.createView(null, _repo, mock(Model.class), "view");
    }

    @Test
    public void shouldFailConstructionIfNotView() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ViewImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingForeignKey() throws KException {
        this.view.addForeignKey(null, "blah", mock(Table.class));
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingUniqueConstraint() throws KException {
        this.view.addUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingForeignKey() throws KException {
        this.view.removeForeignKey(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingUniqueConstraint() throws KException {
        this.view.removeUniqueConstraint(null, "blah");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenSettingPrimaryKey() throws KException {
        this.view.setPrimaryKey(null, "blah");
    }

}
