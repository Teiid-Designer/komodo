/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.KomodoStatusObject;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoStatusObject}s.
 */
public final class StatusObjectSerializer extends TypeAdapter< KomodoStatusObject > {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoStatusObject read( final JsonReader in ) throws IOException {
        final KomodoStatusObject status = new KomodoStatusObject();
        boolean foundTitle = false;
        boolean foundInfo = false;

        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if ( !foundTitle && KomodoStatusObject.TITLE_LABEL.equals( name ) ) {
                status.setTitle( in.nextString() );
                foundTitle = true;
            } else if ( !foundInfo && KomodoStatusObject.INFO_LABEL.equals( name ) ) {
                Map< String, String > attributes = BUILDER.fromJson( in, Map.class );
                status.setAttributes( attributes );
                foundInfo = true;
            } else {
                throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return status;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoStatusObject value ) throws IOException {

        out.beginObject();

        // Title of object
        out.name(KomodoStatusObject.TITLE_LABEL);
        out.value(value.getTitle());

        out.name(KomodoStatusObject.INFO_LABEL);
        BUILDER.toJson(value.getAttributes(), STRING_MAP_TYPE, out);

        out.endObject();
    }

}
