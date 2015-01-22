/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class VdbImportImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private Vdb vdb;
    private VdbImport vdbImport;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.vdbImport = RelationalModelFactory.createVdbImport(transaction, _repo, this.vdb, ("entry" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.vdbImport.getPrimaryType(null).getName(), is(VdbLexicon.ImportVdb.IMPORT_VDB));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesAfterConstruction() throws Exception {
        create();
        assertThat(this.vdbImport.isImportDataPolicies(null), is(VdbImport.DEFAULT_IMPORT_DATA_POLICIES));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesValueAfterConstruction() throws Exception {
        create();
        assertThat(this.vdbImport.isImportDataPolicies(null), is(VdbImport.DEFAULT_IMPORT_DATA_POLICIES));
    }

    @Test
    public void shouldSetImportDataPoliciesValue() throws Exception {
        create();
        final boolean newValue = !VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
        this.vdbImport.setImportDataPolicies(null, newValue);
        assertThat(this.vdbImport.isImportDataPolicies(null), is(newValue));
    }

    @Test
    public void shouldSetVersion() throws Exception {
        create();
        final int newValue = (Vdb.DEFAULT_VERSION + 10);
        this.vdbImport.setVersion(null, newValue);
        assertThat(this.vdbImport.getVersion(null), is(newValue));
    }

}
