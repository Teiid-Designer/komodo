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

import java.io.IOException;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoArtifactPathAttribute}.
 */
public class PathAttributeSerializer<T extends KomodoPathAttribute> extends TypeAdapter<T> {

    @SuppressWarnings( "unchecked" )
    protected T createEntity() {
        return (T) new KomodoPathAttribute();
    }

    protected String readPath(JsonReader in, String name, T pathAttr) throws IOException {
        if (KomodoPathAttribute.PATH_LABEL.equals(name)) {
            String path = in.nextString();
            pathAttr.setPath(path);
            return path;
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public T read( final JsonReader in ) throws IOException {
        final T pathAttr = createEntity();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();
            readPath(in, name, pathAttr);
        }

        in.endObject();

        return pathAttr;
    }

    protected void writePath(JsonWriter out, T value) throws IOException {
        out.name(KomodoPathAttribute.PATH_LABEL);
        out.value(value.getPath());
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final T value ) throws IOException {

        out.beginObject();
        writePath(out, value);
        out.endObject();
    }

}
