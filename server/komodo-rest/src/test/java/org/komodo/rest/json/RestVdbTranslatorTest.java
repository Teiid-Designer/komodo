/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbTranslatorTest {

    private static final String DESCRIPTION = "my description";
    private static final String NAME = "MyTranslator";
    private static final String TYPE = "oracle";
    private static final Map< String, String > PROPS = Collections.unmodifiableMap( new HashMap< String, String >() {

        private static final long serialVersionUID = 1L;

        {
            put( "larry", "bird" );
            put( "magic", "johnson" );
            put( "michael", "jordan" );
        }
    } );

    private RestVdbTranslator translator;

    @Before
    public void init() {
        this.translator = new RestVdbTranslator( NAME, TYPE );
        this.translator.setDescription( DESCRIPTION );
        this.translator.setProperties( PROPS );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator( this.translator.getName(), this.translator.getType() );
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
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getType(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator( this.translator.getName(), this.translator.getType() );
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator.hashCode(), is( thatTranslator.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator( this.translator.getName() + "blah",
                                                                        this.translator.getType() );
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setProperties( this.translator.getProperties() );
        thatTranslator.setLinks( this.translator.getLinks() );

        assertThat( this.translator.getName(), is( not( thatTranslator.getName() ) ) );
        assertThat( this.translator, is( not( thatTranslator ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenPropertiesAreDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator( this.translator.getName(), this.translator.getType() );
        thatTranslator.setDescription( this.translator.getDescription() );
        thatTranslator.setLinks( this.translator.getLinks() );

        final Map< String, String > props = new HashMap< >( this.translator.getProperties() );
        props.put( "blah", "blah" );
        thatTranslator.setProperties( props );

        assertThat( this.translator, is( not( thatTranslator ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenTypeIsDifferent() {
        final RestVdbTranslator thatTranslator = new RestVdbTranslator( this.translator.getName(),
                                                                        this.translator.getType() + "blah" );
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
        this.translator.setName( newName );
        assertThat( this.translator.getName(), is( newName ) );
    }

    @Test
    public void shouldSetProperties() {
        final Map< String, String > newProperties = new HashMap< >();
        newProperties.put( "blah", "blah" );
        this.translator.setProperties( newProperties );
        assertThat( this.translator.getProperties().size(), is( newProperties.size() ) );
        assertThat( this.translator.getProperties().keySet(), is( newProperties.keySet() ) );
    }

    @Test
    public void shouldSetType() {
        final String newType = "blah";
        this.translator.setType( newType );
        assertThat( this.translator.getType(), is( newType ) );
    }

}
