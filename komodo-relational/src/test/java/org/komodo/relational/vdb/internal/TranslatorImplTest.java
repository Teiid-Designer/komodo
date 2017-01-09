/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TranslatorImplTest extends RelationalModelTest {

    private Translator translator;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.translator = vdb.addTranslator( getTransaction(), "translator", "type" );
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
                new TranslatorImpl( getTransaction(), _repo, this.translator.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.translator.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.translator.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_TRANSLATOR));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.translator.getPropertyNames( getTransaction() );
        final String[] rawProps = this.translator.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        KomodoObject parent = this.translator.getParent(getTransaction());
        assertEquals(KomodoType.VDB, parent.getTypeIdentifier(getTransaction()));
    }

    @Test
    public void shouldHaveTypeAfterConstruction() throws Exception {
        assertThat( this.translator.getType( getTransaction() ), is( notNullValue() ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.translator.addChild( getTransaction(), "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyType() throws Exception {
        this.translator.setType( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullType() throws Exception {
        this.translator.setType( getTransaction(), null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.translator.getPropertyNames( getTransaction() );
        final Filter[] filters = this.translator.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.translator.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.translator.rename( getTransaction(), newName );
        assertThat( this.translator.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetCustomProperty() throws Exception {
        final String propName = "custom";
        final String propValue = "value";
        this.translator.setProperty( getTransaction(), propName, propValue );

        assertThat( this.translator.getProperty( getTransaction(), propName ), is( notNullValue() ) );
        assertThat( this.translator.getProperty( getTransaction(), propName ).getStringValue( getTransaction() ), is( propValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.translator.setDescription( getTransaction(), newValue );
        assertThat( this.translator.getDescription( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetType() throws Exception {
        final String newValue = "newType";
        this.translator.setType( getTransaction(), newValue );
        assertThat( this.translator.getType( getTransaction() ), is( newValue ) );
    }

}
