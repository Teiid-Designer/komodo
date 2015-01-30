/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class MaskImplTest extends RelationalModelTest {

    private DataRole dataRole;
    private Mask mask;
    private Permission permission;
    private Vdb vdb;

    @Before
    public void init() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(MaskImplTest.class.getSimpleName(), false, null);

        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb");
        this.dataRole = RelationalModelFactory.createDataRole(transaction, _repo, this.vdb, "dataRole");
        this.permission = RelationalModelFactory.createPermission(transaction, _repo, this.dataRole, "permission");
        this.mask = RelationalModelFactory.createMask(transaction, _repo, this.permission, "mask");

        transaction.commit();
    }

    @Test
    public void shouldFailConstructionIfNotMask() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new MaskImpl(null, _repo, this.vdb.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat(this.mask.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.Permission.Mask.MASK));
    }

    @Test
    public void shouldNotHaveOrderAfterConstruction() throws Exception {
        assertThat(this.mask.getOrder(null), is(nullValue()));
    }

    @Test
    public void shouldSetOrder() throws Exception {
        final String newValue = "newOrder";
        this.mask.setOrder(null, newValue);
        assertThat(this.mask.getOrder(null), is(newValue));
    }

}
