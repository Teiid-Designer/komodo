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
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class TranslatorImplTest extends RelationalModelTest {

    private static int _counter = 1;

    private Translator translator;
    private Vdb vdb;

    private void create() throws Exception {
        final UnitOfWork transaction = _repo.createTransaction(this.name.getMethodName(), false, null); // won't work inside an @Before

        final int suffix = _counter++;
        this.vdb = RelationalModelFactory.createVdb(transaction, _repo, null, ("vdb" + suffix), "/Users/sledge/hammer/MyVdb.vdb");
        this.translator = RelationalModelFactory.createTranslator(transaction,
                                                                  _repo,
                                                                  this.vdb,
                                                                  ("translator" + suffix),
                                                                  ("type" + suffix));

        transaction.commit();

        assertThat(this.vdb.getPrimaryType(null).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(this.translator.getPrimaryType(null).getName(), is(VdbLexicon.Translator.TRANSLATOR));
    }

    @Test
    public void shouldHaveTypeAfterConstruction() throws Exception {
        create();
        assertThat(this.translator.getType(null), is(notNullValue()));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyType() throws Exception {
        create();
        this.translator.setType(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullType() throws Exception {
        create();
        this.translator.setType(null, null);
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        create();
        assertThat(this.translator.getDescription(null), is(nullValue()));
    }

    @Test
    public void shouldSetCustomProperty() throws Exception {
        create();
        final String propName = "custom";
        final String propValue = "value";
        this.translator.setProperty(null, propName, propValue);

        assertThat(this.translator.getProperty(null, propName), is(notNullValue()));
        assertThat(this.translator.getProperty(null, propName).getStringValue(), is(propValue));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        create();
        final String newValue = "newDescription";
        this.translator.setDescription(null, newValue);
        assertThat(this.translator.getDescription(null), is(newValue));
    }

    @Test
    public void shouldSetType() throws Exception {
        create();
        final String newValue = "newType";
        this.translator.setType(null, newValue);
        assertThat(this.translator.getType(null), is(newValue));
    }

}
