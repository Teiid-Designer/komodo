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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TranslatorImplTest extends RelationalModelTest {

    private Translator translator;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.translator = vdb.addTranslator( this.uow, "translator", "type" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.translator.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotTranslator() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TranslatorImpl( this.uow, _repo, this.translator.getParent( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.translator.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.translator.getTypeIdentifier( this.uow ), is(KomodoType.VDB_TRANSLATOR));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.translator.getPropertyNames( this.uow );
        final String[] rawProps = this.translator.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.translator.getParent( this.uow ), is( instanceOf( Vdb.class ) ) );
    }

    @Test
    public void shouldHaveTypeAfterConstruction() throws Exception {
        assertThat( this.translator.getType( this.uow ), is( notNullValue() ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.translator.addChild( this.uow, "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyType() throws Exception {
        this.translator.setType( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullType() throws Exception {
        this.translator.setType( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.translator.getPropertyNames( this.uow );
        final Filter[] filters = this.translator.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.translator.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetCustomProperty() throws Exception {
        final String propName = "custom";
        final String propValue = "value";
        this.translator.setProperty( this.uow, propName, propValue );

        assertThat( this.translator.getProperty( this.uow, propName ), is( notNullValue() ) );
        assertThat( this.translator.getProperty( this.uow, propName ).getStringValue( this.uow ), is( propValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.translator.setDescription( this.uow, newValue );
        assertThat( this.translator.getDescription( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetType() throws Exception {
        final String newValue = "newType";
        this.translator.setType( this.uow, newValue );
        assertThat( this.translator.getType( this.uow ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";

        final RelationalProperties props = new RelationalProperties();
        props.add( new RelationalProperty( VdbLexicon.Translator.TYPE, "oracle" ) );

        final KomodoObject kobject = TranslatorImpl.RESOLVER.create( this.uow,
                                                                     _repo,
                                                                     this.translator.getParent( this.uow ),
                                                                     name,
                                                                     props );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Translator.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        TranslatorImpl.RESOLVER.create( this.uow, _repo, bogusParent, "blah", new RelationalProperties() );
    }

}
