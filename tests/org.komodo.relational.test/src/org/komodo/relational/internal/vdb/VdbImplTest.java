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
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class VdbImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        final String path = "/Users/sledge/hammer/MyVdb.vdb";
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), path);

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.vdb.getOriginalFilePath(null), is(path));
    }

    @Test
    public void shouldAddDataRole() throws Exception {
        create();

        final String name = ("dataRole" + _counter);
        final DataRole dataRole = this.vdb.addDataRole(null, name);
        assertThat(dataRole, is(notNullValue()));
        assertThat(this.vdb.getDataRoles(null).length, is(1));

        final DataRole added = this.vdb.getDataRoles(null)[0];
        assertThat(added, is(dataRole));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.DataRole.DATA_ROLE));
    }

    @Test
    public void shouldAddEntry() throws Exception {
        create();

        final String name = ("entry" + _counter);
        final String path = "/my/path";
        final Entry entry = this.vdb.addEntry(null, name, path);
        assertThat(entry, is(notNullValue()));
        assertThat(this.vdb.getEntries(null).length, is(1));

        final Entry added = this.vdb.getEntries(null)[0];
        assertThat(added, is(entry));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.Entry.ENTRY));
        assertThat(added.getPath(null), is(path));
    }

    @Test
    public void shouldAddImport() throws Exception {
        create();

        final String name = ("vdbImport" + _counter);
        final VdbImport vdbImport = this.vdb.addImport(null, name);
        assertThat(vdbImport, is(notNullValue()));
        assertThat(this.vdb.getImports(null).length, is(1));

        final VdbImport added = this.vdb.getImports(null)[0];
        assertThat(added, is(vdbImport));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.ImportVdb.IMPORT_VDB));
    }

    @Test
    public void shouldAddTranslator() throws Exception {
        create();

        final String name = ("translator" + _counter);
        final String type = "oracle";
        final Translator translator = this.vdb.addTranslator(null, name, type);
        assertThat(translator, is(notNullValue()));
        assertThat(this.vdb.getTranslators(null).length, is(1));

        final Translator added = this.vdb.getTranslators(null)[0];
        assertThat(added, is(translator));
        assertThat(added.getName(null), is(name));
        assertThat(added.getPrimaryType(null).getName(), is(VdbLexicon.Translator.TRANSLATOR));
        assertThat(added.getType(null), is(type));
    }

    @Test
    public void shouldHaveDefaultPreviewValueAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.isPreview(null), is(Vdb.DEFAULT_PREVIEW));
    }

    @Test
    public void shouldHaveDefaultVersionAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getVersion(null), is(Vdb.DEFAULT_VERSION));
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyDataRole() throws Exception {
        create();
        this.vdb.addDataRole(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyEntry() throws Exception {
        create();
        this.vdb.addEntry(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyImport() throws Exception {
        create();
        this.vdb.addImport(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyTranslator() throws Exception {
        create();
        this.vdb.addTranslator(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullDataRole() throws Exception {
        create();
        this.vdb.addDataRole(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullEntry() throws Exception {
        create();
        this.vdb.addEntry(null, null, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullImport() throws Exception {
        create();
        this.vdb.addImport(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullTranslator() throws Exception {
        create();
        this.vdb.addTranslator(null, null, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToSetEmptyOriginalFilePath() throws Exception {
        create();
        this.vdb.setOriginalFilePath(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToSetNullOriginalFilePath() throws Exception {
        create();
        this.vdb.setOriginalFilePath(null, null);
    }

    @Test
    public void shouldNotHaveConnectionTypeAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getConnectionType(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveDataRolesAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getDataRoles(null), is(notNullValue()));
        assertThat(this.vdb.getDataRoles(null).length, is(0));
    }

    @Test
    public void shouldNotHaveEntriesAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getEntries(null), is(notNullValue()));
        assertThat(this.vdb.getEntries(null).length, is(0));
    }

    @Test
    public void shouldNotHaveTranslatorsAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getTranslators(null), is(notNullValue()));
        assertThat(this.vdb.getTranslators(null).length, is(0));
    }

    @Test
    public void shouldNotHaveVdbImportsAfterConstruction() throws Exception {
        create();
        assertThat(this.vdb.getImports(null), is(notNullValue()));
        assertThat(this.vdb.getImports(null).length, is(0));
    }

    @Test
    public void shouldRemoveDataRole() throws Exception {
        create();

        final String name = ("dataRole" + _counter);
        this.vdb.addDataRole(null, name);
        assertThat(this.vdb.getDataRoles(null).length, is(1));

        this.vdb.removeDataRole(null, name);
        assertThat(this.vdb.getDataRoles(null).length, is(0));
    }

    @Test
    public void shouldRemoveEntry() throws Exception {
        create();

        final String name = ("entry" + _counter);
        this.vdb.addEntry(null, name, "path");
        assertThat(this.vdb.getEntries(null).length, is(1));

        this.vdb.removeEntry(null, name);
        assertThat(this.vdb.getEntries(null).length, is(0));
    }

    @Test
    public void shouldRemoveTranslator() throws Exception {
        create();

        final String name = ("translator" + _counter);
        this.vdb.addTranslator(null, name, "oracle");
        assertThat(this.vdb.getTranslators(null).length, is(1));

        this.vdb.removeTranslator(null, name);
        assertThat(this.vdb.getTranslators(null).length, is(0));
    }

    @Test
    public void shouldRemoveVdbImport() throws Exception {
        create();

        final String name = ("vdbImport" + _counter);
        this.vdb.addImport(null, name);
        assertThat(this.vdb.getImports(null).length, is(1));

        this.vdb.removeImport(null, name);
        assertThat(this.vdb.getImports(null).length, is(0));
    }

    @Test
    public void shouldSetConnectionType() throws Exception {
        create();
        final String newValue = "newConnectionType";
        this.vdb.setConnectionType(null, newValue);
        assertThat(this.vdb.getConnectionType(null), is(newValue));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        create();
        final String newValue = "newDescription";
        this.vdb.setDescription(null, newValue);
        assertThat(this.vdb.getDescription(null), is(newValue));
    }

    @Test
    public void shouldSetOriginalFilePath() throws Exception {
        create();
        final String newValue = "newOriginalFilePath";
        this.vdb.setOriginalFilePath(null, newValue);
        assertThat(this.vdb.getOriginalFilePath(null), is(newValue));
    }

    @Test
    public void shouldSetPreviewValue() throws Exception {
        create();
        final boolean newValue = !Vdb.DEFAULT_PREVIEW;
        this.vdb.setPreview(null, newValue);
        assertThat(this.vdb.isPreview(null), is(newValue));
    }

    @Test
    public void shouldSetVdbName() throws Exception {
        create();
        final String newValue = "newName";
        this.vdb.setVdbName(null, newValue);
        assertThat(this.vdb.getVdbName(null), is(newValue));
    }

    @Test
    public void shouldSetVersion() throws Exception {
        create();
        final int newValue = (Vdb.DEFAULT_VERSION + 10);
        this.vdb.setVersion(null, newValue);
        assertThat(this.vdb.getVersion(null), is(newValue));
    }

}
