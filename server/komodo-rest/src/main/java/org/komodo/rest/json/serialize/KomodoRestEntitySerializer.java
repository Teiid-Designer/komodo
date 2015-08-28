/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.json.KomodoRestEntity;
import org.komodo.rest.json.RestLink;
import com.google.common.reflect.TypeToken;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for the Komodo REST objects.
 *
 * @param <T>
 *        the {@link KomodoRestEntity} subclass
 */
public abstract class KomodoRestEntitySerializer< T extends KomodoRestEntity > extends TypeAdapter< T > {

    @SuppressWarnings( "serial" )
    private static final Type PROP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    protected void beginRead( final JsonReader in ) throws IOException {
        in.beginObject();
    }

    protected void beginWrite( final JsonWriter out ) throws IOException {
        out.beginObject();
    }

    protected void endRead( final JsonReader in ) throws IOException {
        in.endObject();
    }

    protected void endWrite( final JsonWriter out ) throws IOException {
        out.endObject();
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public abstract T read( final JsonReader in ) throws IOException;

    protected void readLinks( final JsonReader in,
                              final T value ) {
        final RestLink[] links = JSON_BUILDER.fromJson( in, RestLink[].class );
        value.setLinks( links );
    }

    protected void readProperties( final JsonReader in,
                                   final T value ) {
        final Map< String, String > props = JSON_BUILDER.fromJson( in, Map.class );
        value.setProperties( props );
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public abstract void write( final JsonWriter out,
                                final T value ) throws IOException;

    protected void writeLinks( final JsonWriter out,
                               final T value ) throws IOException {
        if ( value.getLinks().length != 0 ) {
            out.name( JsonConstants.LINKS );

            out.beginArray();
            for ( final RestLink link : value.getLinks() ) {
                JSON_BUILDER.toJson( link, RestLink.class, out );
            }
            out.endArray();
        }
    }

    protected void writeProperties( final JsonWriter out,
                                    final T value ) throws IOException {
        if ( !value.getProperties().isEmpty() ) {
            out.name( JsonConstants.PROPERTIES );
            JSON_BUILDER.toJson( value.getProperties(), PROP_TYPE, out );
        }
    }

}
