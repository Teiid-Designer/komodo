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
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.RestProperty;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbTranslatorTest {

    private static final String DESCRIPTION = "my description";
    private static final String NAME = "MyTranslator";
    private static final String TYPE = "oracle";

    private static final List<RestProperty> PROPS = new ArrayList<>();
    static {
        PROPS.add(new RestProperty("larry", "bird"));
        PROPS.add(new RestProperty("magic", "johnson"));
        PROPS.add(new RestProperty("michael", "jordan"));
    }

    private RestVdbTranslator translator;

    @Before
    public void init() {
        this.translator = new RestVdbTranslator();
        this.translator.setId(NAME);
        this.translator.setType(TYPE);
        this.translator.setDescription( DESCRIPTION );
        this.translator.setProperties( PROPS );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator();
        thatTranslator.setId(this.translator.getId());
        thatTranslator.setType(this.translator.getType());
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator, is( thatTranslator ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyTranslators() {
        final RestVdbTranslator empty1 = new RestVdbTranslator();
        final RestVdbTranslator empty2 = new RestVdbTranslator();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyTranslator() {
        final RestVdbTranslator empty = new RestVdbTranslator();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getId(), is( nullValue() ) );
        assertThat( empty.getType(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().size(), is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator();
        thatTranslator.setId(this.translator.getId());
        thatTranslator.setType(this.translator.getType());
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator.hashCode(), is( thatTranslator.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator();
        thatTranslator.setId(this.translator.getId() + "blah");
        thatTranslator.setType(this.translator.getType());
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator.getId(), is( not( thatTranslator.getId() ) ) );
        assertThat( this.translator, is( not( thatTranslator ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenPropertiesAreDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator();
        thatTranslator.setId(this.translator.getId());
        thatTranslator.setType(this.translator.getType());
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setLinks( this.translator.getLinks() );

        List<RestProperty> props = new ArrayList<>();
        props.addAll(this.translator.getProperties() );
        props.add(new RestProperty("blah", "blah" ));
        thatTranslator.setProperties( props );

        assertThat( this.translator, is( not( thatTranslator ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenTypeIsDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator();
        thatTranslator.setId(this.translator.getId());
        thatTranslator.setType(this.translator.getType() + "blah");
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator.getType(), is( not( thatTranslator.getType() ) ) );
        assertThat( this.translator, is( not( thatTranslator ) ) );
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.translator.setDescription( newDescription );
        assertThat( this.translator.getDescription(), is( newDescription ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.translator.setId( newName );
        assertThat( this.translator.getId(), is( newName ) );
    }

    @Test
    public void shouldSetProperties() {
        List<RestProperty> newProperties = new ArrayList<>();
        newProperties.add(new RestProperty("blah", "blah" ));
        this.translator.setProperties( newProperties );
        assertThat( this.translator.getProperties().size(), is( newProperties.size() ) );
    }

    @Test
    public void shouldSetType() {
        final String newType = "blah";
        this.translator.setType( newType );
        assertThat( this.translator.getType(), is( newType ) );
    }

}
