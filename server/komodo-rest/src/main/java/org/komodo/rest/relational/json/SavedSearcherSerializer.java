/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.KomodoSavedSearcher;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoSearchObject}s.
 */
public final class SavedSearcherSerializer extends TypeAdapter< KomodoSavedSearcher > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoSavedSearcher read( final JsonReader in ) throws IOException {
        final KomodoSavedSearcher status = new KomodoSavedSearcher();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoSavedSearcher.NAME_LABEL:
                    status.setName(in.nextString());
                    break;
                case KomodoSavedSearcher.QUERY_LABEL:
                    status.setQuery(in.nextString());
                    break;
                default:
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
                       final KomodoSavedSearcher value ) throws IOException {

        out.beginObject();

        // Title of object
        out.name(KomodoSavedSearcher.NAME_LABEL);
        out.value(value.getName());

        out.name(KomodoSavedSearcher.QUERY_LABEL);
        out.value(value.getQuery());

        out.endObject();
    }

}
