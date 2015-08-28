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
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestTranslatorTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyTranslator\",\"type\":\"oracle\",\"description\":\"my description\",\"properties\":{\"magic\":\"johnson\",\"michael\":\"jordan\",\"larry\":\"bird\"}}";
    private static final String NAME = "MyTranslator";
    private static final int NUM_PROPS = 3;
    private static final String[] PROP_NAMES = { "magic", "michael", "larry" };
    private static final String[] PROP_VALUES = { "johnson", "jordan", "bird" };
    private static final String TYPE = "oracle";
    private static final Map< String, String > PROPS = Collections.unmodifiableMap( new HashMap< String, String >() {

        private static final long serialVersionUID = 1L;

        {
            put( "larry", "bird" );
            put( "magic", "johnson" );
            put( "michael", "jordan" );
        }
    } );

    private RestTranslator translator;

    @Before
    public void init() {
        this.translator = new RestTranslator( NAME, TYPE );
        this.translator.setDescription( DESCRIPTION );
        this.translator.setProperties( PROPS );
    }

    @Test
    public void shouldBeEqual() {
        final RestTranslator translator2 = new RestTranslator( this.translator.getName(), this.translator.getType() );
        translator2.setDescription( this.translator.getDescription() );
        translator2.setProperties( this.translator.getProperties() );

        assertThat( this.translator, is( translator2 ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyTranslators() {
        final RestTranslator empty1 = new RestTranslator();
        final RestTranslator empty2 = new RestTranslator();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyTranslator() {
        final RestTranslator empty = new RestTranslator();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getType(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldExportJson() {
        assertThat( JSON_BUILDER.toJson( this.translator ), is( JSON ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestTranslator translator2 = new RestTranslator( this.translator.getName(), this.translator.getType() );
        translator2.setDescription( this.translator.getDescription() );
        translator2.setProperties( this.translator.getProperties() );

        assertThat( this.translator.hashCode(), is( translator2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestTranslator translator = JSON_BUILDER.fromJson( JSON, RestTranslator.class );
        assertThat( translator.getDescription(), is( DESCRIPTION ) );
        assertThat( translator.getName(), is( NAME ) );
        assertThat( translator.getType(), is( TYPE ) );
        assertThat( translator.getLinks().length, is( 0 ) );
        assertThat( translator.getProperties().size(), is( NUM_PROPS ) );

        // check keys
        for ( final String key : PROP_NAMES ) {
            assertThat( translator.getProperties().containsKey( key ), is( true ) );
        }

        // check values
        for ( final String value : PROP_VALUES ) {
            assertThat( translator.getProperties().containsValue( value ), is( true ) );
        }
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestTranslator translator2 = new RestTranslator( this.translator.getName() + "blah", this.translator.getType() );
        translator2.setDescription( this.translator.getDescription() );
        translator2.setProperties( this.translator.getProperties() );

        assertThat( this.translator.getName(), is( not( translator2.getName() ) ) );
        assertThat( this.translator, is( not( translator2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenTypeIsDifferent() {
        final RestTranslator translator2 = new RestTranslator( this.translator.getName(), this.translator.getType() + "blah" );
        translator2.setDescription( this.translator.getDescription() );
        translator2.setProperties( this.translator.getProperties() );

        assertThat( this.translator.getType(), is( not( translator2.getType() ) ) );
        assertThat( this.translator, is( not( translator2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenPropertiesAreDifferent() {
        final RestTranslator translator2 = new RestTranslator( this.translator.getName(), this.translator.getType() );
        translator2.setDescription( this.translator.getDescription() );

        final Map< String, String > props = new HashMap< >( this.translator.getProperties() );
        props.put( "blah", "blah" );
        translator2.setProperties( props );

        assertThat( this.translator, is( not( translator2 ) ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"type\":\"oracle\",\"description\":\"my description\",\"properties\":{\"magic\":\"johnson\",\"michael\":\"jordan\",\"larry\":\"bird\"}}";
        JSON_BUILDER.fromJson( malformed, RestTranslator.class );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenTypeIsMissing() {
        final String malformed = "{\"id\":\"MyTranslator\",\"description\":\"my description\",\"properties\":{\"magic\":\"johnson\",\"michael\":\"jordan\",\"larry\":\"bird\"}}";
        JSON_BUILDER.fromJson( malformed, RestTranslator.class );
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
