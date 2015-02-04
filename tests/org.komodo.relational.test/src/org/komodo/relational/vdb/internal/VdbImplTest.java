/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
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

    private static final String PATH = "/Users/sledge/hammer/MyVdb.vdb";

    private Vdb vdb;
    private String vdbName;

    @Before
    public void init() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(VdbImplTest.class.getSimpleName(), false, null);

        this.vdbName = "vdb";
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, this.vdbName, PATH);

        transaction.commit();
    }

    @Test
    public void shouldAddDataRole() throws Exception {
        final String name = "dataRole";
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
        final String name = "entry";
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
        final String name = "vdbImport";
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
        final String name = "translator";
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
    public void shouldFailConstructionIfNotVdb() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new VdbImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat(this.vdb.getName(null), is(this.vdbName));
    }

    @Test
    public void shouldHaveCorrectOriginalFilePathAfterConstruction() throws Exception {
        assertThat(this.vdb.getOriginalFilePath(null), is(PATH));
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
    }

    @Test
    public void shouldHaveDefaultPreviewValueAfterConstruction() throws Exception {
        assertThat(this.vdb.isPreview(null), is(Vdb.DEFAULT_PREVIEW));
    }

    @Test
    public void shouldHaveDefaultVersionAfterConstruction() throws Exception {
        assertThat(this.vdb.getVersion(null), is(Vdb.DEFAULT_VERSION));
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyDataRole() throws Exception {
        this.vdb.addDataRole(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyEntry() throws Exception {
        this.vdb.addEntry(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyImport() throws Exception {
        this.vdb.addImport(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddEmptyTranslator() throws Exception {
        this.vdb.addTranslator(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullDataRole() throws Exception {
        this.vdb.addDataRole(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullEntry() throws Exception {
        this.vdb.addEntry(null, null, "blah");
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullImport() throws Exception {
        this.vdb.addImport(null, null);
    }

    @Test( expected = KException.class )
    public void shouldNotBeAbleToAddNullTranslator() throws Exception {
        this.vdb.addTranslator(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath(null, null);
    }

    @Test
    public void shouldNotHaveConnectionTypeAfterConstruction() throws Exception {
        assertThat(this.vdb.getConnectionType(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveDataRolesAfterConstruction() throws Exception {
        assertThat(this.vdb.getDataRoles(null), is(notNullValue()));
        assertThat(this.vdb.getDataRoles(null).length, is(0));
    }

    @Test
    public void shouldNotHaveEntriesAfterConstruction() throws Exception {
        assertThat(this.vdb.getEntries(null), is(notNullValue()));
        assertThat(this.vdb.getEntries(null).length, is(0));
    }

    @Test
    public void shouldNotHaveTranslatorsAfterConstruction() throws Exception {
        assertThat(this.vdb.getTranslators(null), is(notNullValue()));
        assertThat(this.vdb.getTranslators(null).length, is(0));
    }

    @Test
    public void shouldNotHaveVdbImportsAfterConstruction() throws Exception {
        assertThat(this.vdb.getImports(null), is(notNullValue()));
        assertThat(this.vdb.getImports(null).length, is(0));
    }

    @Test
    public void shouldRemoveDataRole() throws Exception {
        final String name = "dataRole";
        this.vdb.addDataRole(null, name);
        assertThat(this.vdb.getDataRoles(null).length, is(1));

        this.vdb.removeDataRole(null, name);
        assertThat(this.vdb.getDataRoles(null).length, is(0));
    }

    @Test
    public void shouldRemoveEntry() throws Exception {
        final String name = "entry";
        this.vdb.addEntry(null, name, "path");
        assertThat(this.vdb.getEntries(null).length, is(1));

        this.vdb.removeEntry(null, name);
        assertThat(this.vdb.getEntries(null).length, is(0));
    }

    @Test
    public void shouldRemoveTranslator() throws Exception {
        final String name = "translator";
        this.vdb.addTranslator(null, name, "oracle");
        assertThat(this.vdb.getTranslators(null).length, is(1));

        this.vdb.removeTranslator(null, name);
        assertThat(this.vdb.getTranslators(null).length, is(0));
    }

    @Test
    public void shouldRemoveVdbImport() throws Exception {
        final String name = "vdbImport";
        this.vdb.addImport(null, name);
        assertThat(this.vdb.getImports(null).length, is(1));

        this.vdb.removeImport(null, name);
        assertThat(this.vdb.getImports(null).length, is(0));
    }

    @Test
    public void shouldSetConnectionType() throws Exception {
        final String newValue = "newConnectionType";
        this.vdb.setConnectionType(null, newValue);
        assertThat(this.vdb.getConnectionType(null), is(newValue));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.vdb.setDescription(null, newValue);
        assertThat(this.vdb.getDescription(null), is(newValue));
    }

    @Test
    public void shouldSetOriginalFilePath() throws Exception {
        final String newValue = "newOriginalFilePath";
        this.vdb.setOriginalFilePath(null, newValue);
        assertThat(this.vdb.getOriginalFilePath(null), is(newValue));
    }

    @Test
    public void shouldSetPreviewValue() throws Exception {
        final boolean newValue = !Vdb.DEFAULT_PREVIEW;
        this.vdb.setPreview(null, newValue);
        assertThat(this.vdb.isPreview(null), is(newValue));
    }

    @Test
    public void shouldSetVdbName() throws Exception {
        final String newValue = "newName";
        this.vdb.setVdbName(null, newValue);
        assertThat(this.vdb.getVdbName(null), is(newValue));
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = (Vdb.DEFAULT_VERSION + 10);
        this.vdb.setVersion(null, newValue);
        assertThat(this.vdb.getVersion(null), is(newValue));
    }

}
