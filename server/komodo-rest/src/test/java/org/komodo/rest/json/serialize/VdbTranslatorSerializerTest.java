/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestVdbTranslator;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbTranslatorSerializerTest {

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

    private RestVdbTranslator translator;

    @Before
    public void init() {
        this.translator = new RestVdbTranslator( NAME, TYPE );
        this.translator.setDescription( DESCRIPTION );
        this.translator.setProperties( PROPS );
    }

    @Test
    public void shouldExportJson() {
        assertThat( KomodoJsonMarshaller.marshall( this.translator ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbTranslator translator = KomodoJsonMarshaller.unmarshall( JSON, RestVdbTranslator.class );
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

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbTranslator incomplete = new RestVdbTranslator();
        translator.setType( TYPE );
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenTypeIsMissing() {
        final RestVdbTranslator incomplete = new RestVdbTranslator();
        translator.setName( NAME );
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"type\":\"oracle\",\"description\":\"my description\",\"properties\":{\"magic\":\"johnson\",\"michael\":\"jordan\",\"larry\":\"bird\"}}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbTranslator.class );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenTypeIsMissing() {
        final String malformed = "{\"id\":\"MyTranslator\",\"description\":\"my description\",\"properties\":{\"magic\":\"johnson\",\"michael\":\"jordan\",\"larry\":\"bird\"}}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbTranslator.class );
    }

}
