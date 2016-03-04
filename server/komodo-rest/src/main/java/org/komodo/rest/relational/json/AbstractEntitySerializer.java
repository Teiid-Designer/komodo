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

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Map;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestProperty;
import org.komodo.rest.json.JsonConstants;
import org.komodo.utils.StringUtils;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for the Komodo REST objects.
 *
 * @param <T>
 *        the {@link RestBasicEntity} subclass
 */
public abstract class AbstractEntitySerializer< T extends RestBasicEntity > extends TypeAdapter< T >
    implements JsonConstants {

    protected static final Type BOOLEAN_MAP_TYPE = new TypeToken< Map< String, Boolean > >() {/* nothing to do */}.getType();

    protected static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

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

    protected void readProperties( final JsonReader in, final T value ) {
        final RestProperty[] props = BUILDER.fromJson( in, RestProperty[].class );
        if (props == null)
            value.setProperties(null);
        else
            value.setProperties( Arrays.asList(props) );
    }

    protected void readLinks( final JsonReader in,
                              final T value ) {

        final RestLink[] links = BUILDER.fromJson( in, RestLink[].class );
        value.setLinks( Arrays.asList(links) );
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public abstract void write( final JsonWriter out, final T value ) throws IOException;

    protected void writeProperties( final JsonWriter out, final T value ) throws IOException {
        if (value.getProperties().isEmpty())
            return;

        out.name( JsonConstants.PROPERTIES );
        BUILDER.toJson( value.getProperties().toArray(new RestProperty[0]), RestProperty[].class, out );
    }

    protected void writeLinks( final JsonWriter out,
                               final T value ) throws IOException {
        if ( value.getLinks().size() != 0 ) {
            out.name( JsonConstants.LINKS );
            BUILDER.toJson( value.getLinks().toArray(new RestLink[0]), RestLink[].class, out );
        }
    }

    /**
     * @param entity the entity
     * @return true if entity's id, data path and kType have been populated, false otherwise
     */
    protected boolean isComplete(T entity) {
        return ! StringUtils.isBlank(entity.getId()) && ! StringUtils.isBlank(entity.getDataPath()) &&
                       entity.getkType() != null;
    }
}
