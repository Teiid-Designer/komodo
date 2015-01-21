/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class EntryImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private Entry entry;
    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.entry = RelationalModelFactory.createEntry(transaction, _repo, this.vdb, ("entry" + suffix), ("path" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.entry.getPrimaryType(null).getName(), is(VdbLexicon.Entry.ENTRY));
    }

    @Test
    public void shouldHavePathAfterConstruction() throws Exception {
        create();
        assertThat(this.entry.getPath(null), is(notNullValue()));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyPPath() throws Exception {
        create();
        this.entry.setPath(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullPath() throws Exception {
        create();
        this.entry.setPath(null, null);
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        create();
        assertThat(this.entry.getDescription(null), is(nullValue()));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        create();
        final String newValue = "newDescription";
        this.entry.setDescription(null, newValue);
        assertThat(this.entry.getDescription(null), is(newValue));
    }

    @Test
    public void shouldSetPath() throws Exception {
        create();
        final String newValue = "newPath";
        this.entry.setPath(null, newValue);
        assertThat(this.entry.getPath(null), is(newValue));
    }

}
