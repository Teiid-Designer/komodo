/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class VdbImportImplTest extends RelationalModelTest {

    private Vdb vdb;
    private VdbImport vdbImport;

    @Before
    public void init() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(VdbImportImplTest.class.getSimpleName(), false, null);

        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb");
        this.vdbImport = RelationalModelFactory.createVdbImport(transaction, _repo, this.vdb, "entry");

        transaction.commit();
    }

    @Test
    public void shouldFailConstructionIfNotVdbImport() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new VdbImportImpl(null, _repo, this.vdb.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat(this.vdbImport.getPrimaryType(null).getName(), is(VdbLexicon.ImportVdb.IMPORT_VDB));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesAfterConstruction() throws Exception {
        assertThat(this.vdbImport.isImportDataPolicies(null), is(VdbImport.DEFAULT_IMPORT_DATA_POLICIES));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesValueAfterConstruction() throws Exception {
        assertThat(this.vdbImport.isImportDataPolicies(null), is(VdbImport.DEFAULT_IMPORT_DATA_POLICIES));
    }

    @Test
    public void shouldSetImportDataPoliciesValue() throws Exception {
        final boolean newValue = !VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
        this.vdbImport.setImportDataPolicies(null, newValue);
        assertThat(this.vdbImport.isImportDataPolicies(null), is(newValue));
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = (Vdb.DEFAULT_VERSION + 10);
        this.vdbImport.setVersion(null, newValue);
        assertThat(this.vdbImport.getVersion(null), is(newValue));
    }

}
