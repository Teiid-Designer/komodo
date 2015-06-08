/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ModelSourceImplTest extends RelationalModelTest {

    private ModelSource source;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb" );
        final Model model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        this.source = RelationalModelFactory.createModelSource( this.uow, _repo, model, "source" );
        commit();
    }

    @Test
    public void shouldBeAbleToSetEmptyJndi() throws Exception {
        this.source.setJndiName( this.uow, "blah" );
        this.source.setJndiName( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.source.getJndiName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetEmptyTranslator() throws Exception {
        this.source.setTranslatorName( this.uow, "blah" );
        this.source.setTranslatorName( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.source.getTranslatorName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetNullJndi() throws Exception {
        this.source.setJndiName( this.uow, "blah" );
        this.source.setJndiName( this.uow, null );
        assertThat( this.source.getJndiName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetNullTranslator() throws Exception {
        this.source.setTranslatorName( this.uow, "blah" );
        this.source.setTranslatorName( this.uow, null );
        assertThat( this.source.getTranslatorName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.source.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotSource() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TranslatorImpl( this.uow, _repo, this.source.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.source.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Source.SOURCE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.source.getTypeIdentifier( this.uow ), is(KomodoType.VDB_MODEL_SOURCE));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.source.getPropertyNames( this.uow );
        final String[] rawProps = this.source.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.source.getParent( this.uow ), is( instanceOf( Model.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.source.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.source.getPropertyNames( this.uow );
        final Filter[] filters = this.source.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveJndiNameAfterConstruction() throws Exception {
        assertThat( this.source.getJndiName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveTranslatorNameAfterConstruction() throws Exception {
        assertThat( this.source.getTranslatorName( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetJndiName() throws Exception {
        final String name = "jndiName";
        this.source.setJndiName( this.uow, name );
        assertThat( this.source.getJndiName( this.uow ), is( name ) );
    }

    @Test
    public void shouldSetTranslatorName() throws Exception {
        final String name = "translatorName";
        this.source.setTranslatorName( this.uow, name );
        assertThat( this.source.getTranslatorName( this.uow ), is( name ) );
    }

}
