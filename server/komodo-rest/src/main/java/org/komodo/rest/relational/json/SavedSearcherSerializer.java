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
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.KomodoSavedSearcher;
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
                case KomodoSavedSearcher.PARAMETER_LABEL:
                    final String[] parameters = BUILDER.fromJson( in, String[].class );
                    status.setParameters(Arrays.asList(parameters));
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

        if (value.getParameters() != null && ! value.getParameters().isEmpty()) {
            out.name(KomodoSavedSearcher.PARAMETER_LABEL);
            out.beginArray();
            for (String val: value.getParameters().toArray(new String[0])) {
                out.value(val);
            }
            out.endArray();
        }

        out.endObject();
    }

}
