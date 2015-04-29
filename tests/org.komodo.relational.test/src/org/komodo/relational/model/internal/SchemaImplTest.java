/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

@SuppressWarnings( {"javadoc", "nls"} )
public class SchemaImplTest extends RelationalModelTest {

    private static final String DDL_VIEW = "CREATE VIEW G1 (" + NEW_LINE +
                                                                        TAB + "e1 integer," + NEW_LINE +
                                                                        TAB + "e2 varchar" + NEW_LINE +
                                                                        ") OPTIONS (CARDINALITY '1234567954432')" + NEW_LINE +
                                                                        "AS" + NEW_LINE +
                                                                        "SELECT e1, e2 FROM foo.bar;\n";

    private static final String SEQUENCE_DDL_PROBLEM = ".*\\/ddl.+problem";

    private static final String SEQUENCE_TEIID_SQL_PATH = ".*\\/G1\\/tsql:query";

    private static final String NAME = "schema";

    private Schema schema;

    private KomodoObject workspace;

    @Before
    public void init() throws Exception {
        WorkspaceManager manager = WorkspaceManager.getInstance(_repo);
        workspace = _repo.komodoWorkspace(null);
        this.schema = manager.createSchema(null, workspace, NAME);
    }

    private void setRenditionValueAwaitSequencing(String value) throws Exception {
        final CountDownLatch updateLatch = new CountDownLatch(1);
        final Throwable[] errorHolder = new Throwable[1];

        UnitOfWork transaction = _repo.createTransaction("schematests-setrendition-value", false, new UnitOfWorkListener() {

            @Override
            public boolean awaitSequencerCompletion() {
                return true;
            }

            @Override
            public void respond(Object results) {
                updateLatch.countDown();
            }

            @Override
            public void errorOccurred(Throwable error) {
                updateLatch.countDown();
                errorHolder[0] = error;
            }
        });

        assertNotNull(transaction);

        this.schema.setRendition(transaction, value);

        //
        // Commit the transaction and await the response of the callback
        //
        transaction.commit();

        // Wait for the sequencing of the repository or timeout of 3 minutes
        assertTrue(updateLatch.await(3, TimeUnit.MINUTES));
        assertNull(errorHolder[0]);

        traverse(schema);
    }

    @Test
    public void shouldFailConstructionIfNotSchema() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new SchemaImpl(null, _repo, workspace.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldAllowEmptyRendition() throws Exception {
        this.schema.setRendition(null, EMPTY_STRING);
        String rendition = this.schema.getRendition(null);
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldAllowNullRendition() throws Exception {
        this.schema.setRendition(null, null);
        String rendition = this.schema.getRendition(null);
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldsetRendition() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);
        assertThat(this.schema.getRendition(null), is(DDL_VIEW));
    }

    @Test
    public void shouldExportEmptyDdl() throws Exception {
        final String fragment = this.schema.export(null, new Properties());
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportInvalidDdl() throws Exception {
        setRenditionValueAwaitSequencing("This is not ddl syntax");

        final String fragment = this.schema.export(null, new Properties());
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportDdl() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);

        // test
        final String fragment = this.schema.export(null, new Properties());
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(false));
        assertEquals(DDL_VIEW, fragment);
    }

}
