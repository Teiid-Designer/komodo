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
import java.lang.reflect.Type;
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoConnectionObject}s.
 */
public final class ConnectionAttributesSerializer extends TypeAdapter<KomodoConnectionAttributes> {

    private static final Type OBJECT_MAP_TYPE = new TypeToken< Map< String, Object > >() {/* nothing to do */}.getType();

    protected KomodoConnectionAttributes createEntity() {
        return new KomodoConnectionAttributes();
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoConnectionAttributes read( final JsonReader in ) throws IOException {
        final KomodoConnectionAttributes ConnectionAttr = createEntity();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoConnectionAttributes.JNDI_LABEL:
                    ConnectionAttr.setJndi(in.nextString());
                    break;
                case KomodoConnectionAttributes.JDBC_LABEL:
                    ConnectionAttr.setJdbc(in.nextBoolean());
                    break;
                case KomodoConnectionAttributes.DRIVER_LABEL:
                    ConnectionAttr.setDriver(in.nextString());
                    break;
                case KomodoConnectionAttributes.PARAMETERS_LABEL:
                    Map<String, String> parameters = BUILDER.fromJson(in, Map.class);
                    for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                        ConnectionAttr.setParameter(parameter.getKey(), parameter.getValue());
                    }
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return ConnectionAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoConnectionAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoConnectionAttributes.JNDI_LABEL);
        out.value(value.getJndi());

        out.name(KomodoConnectionAttributes.DRIVER_LABEL);
        out.value(value.getDriver());

        out.name(KomodoConnectionAttributes.JDBC_LABEL);
        out.value(value.isJdbc());

        if (! value.getParameters().isEmpty()) {
            out.name(KomodoConnectionAttributes.PARAMETERS_LABEL);
            BUILDER.toJson(value.getParameters(), OBJECT_MAP_TYPE, out);
        }

        out.endObject();
    }

}
