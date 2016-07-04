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
 * but WITHOUKomodoQueryAttribute ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational.json;

import java.io.IOException;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoArtifactPathAttribute}.
 */
public class QueryAttributeSerializer extends TypeAdapter<KomodoQueryAttribute> {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoQueryAttribute read( final JsonReader in ) throws IOException {
        final KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();
            switch (name) {
                case KomodoQueryAttribute.QUERY_LABEL:
                    queryAttr.setQuery(in.nextString());
                    break;
                case KomodoQueryAttribute.TARGET_LABEL:
                    queryAttr.setTarget(in.nextString());
                    break;
                case KomodoQueryAttribute.LIMIT_LABEL:
                    queryAttr.setLimit(in.nextInt());
                    break;
                case KomodoQueryAttribute.OFFSET_LABEL:
                    queryAttr.setOffset(in.nextInt());
                    break;
            }
        }

        in.endObject();

        return queryAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final KomodoQueryAttribute value ) throws IOException {
        out.beginObject();

        out.name(KomodoQueryAttribute.QUERY_LABEL);
        out.value(value.getQuery());

        out.name(KomodoQueryAttribute.TARGET_LABEL);
        out.value(value.getTarget());

        out.name(KomodoQueryAttribute.LIMIT_LABEL);
        out.value(value.getLimit());

        out.name(KomodoQueryAttribute.OFFSET_LABEL);
        out.value(value.getOffset());

        out.endObject();
    }

}
