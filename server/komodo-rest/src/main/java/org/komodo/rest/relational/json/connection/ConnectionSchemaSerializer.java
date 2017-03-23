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
package org.komodo.rest.relational.json.connection;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;

import org.komodo.rest.relational.connection.ConnectionSchema;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status ConnectionSchema}.
 */
public class ConnectionSchemaSerializer extends TypeAdapter<ConnectionSchema> {

    @Override
    public void write(JsonWriter out, ConnectionSchema source) throws IOException {
        out.beginObject();

        out.name(ConnectionSchema.ID_LABEL);
        out.value(source.getId());

        out.name(ConnectionSchema.KTYPE_LABEL);
        out.value(source.getkType());

        out.name(ConnectionSchema.DESCRIPTION_LABEL);
        out.value(source.getDescription());

        if (source.getProperties() == null || source.getProperties().isEmpty())
            return;

        out.name(ConnectionSchema.PROPERTIES_LABEL);
        BUILDER.toJson(source.getProperties(), ConnectionSchemaPropertyListSerializer.class, out);

        out.endObject();
    }

    @Override
    public ConnectionSchema read(JsonReader in) throws IOException {
        throw new UnsupportedOperationException();
    }
}
