/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.model;

import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.internal.model.ViewImpl;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;

@SuppressWarnings( {"javadoc", "nls"} )
public class ViewImplTest {

    private static Repository _repo = null;

    @BeforeClass
    public static void attachToLocalRepository() {
        _repo = mock(Repository.class);
    }

    private View modelObject;

    @Before
    public void init() throws Exception {
        this.modelObject = new ViewImpl(_repo, "view");
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

}
