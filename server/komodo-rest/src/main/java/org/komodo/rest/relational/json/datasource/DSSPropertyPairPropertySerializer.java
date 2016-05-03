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
package org.komodo.rest.relational.json.datasource;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import org.komodo.rest.relational.datasource.DSSPropertyPairProperty;
import org.komodo.rest.relational.datasource.DataSourceSchema;
import org.komodo.rest.relational.datasource.DataSourceSchemaProperty;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status DataSourceSchemaProperty}.
 */
public class DSSPropertyPairPropertySerializer extends TypeAdapter<DSSPropertyPairProperty> {

    @Override
    public DSSPropertyPairProperty read(JsonReader in) throws IOException {
        // Not expecting to ever read the json
        throw new UnsupportedOperationException();
    }

    @Override
    public void write(JsonWriter out, DSSPropertyPairProperty value) throws IOException {
        out.beginObject();

        out.name(DataSourceSchema.PROPERTIES_LABEL);
        BUILDER.toJson(value.getProperties(), DSSPropertyListSerializer.class, out);

        out.name(DataSourceSchemaProperty.TYPE_LABEL);
        out.value(value.getType());

        out.name(DataSourceSchemaProperty.REQUIRED_LABEL);
        out.value(value.isRequired());

        out.name(DataSourceSchemaProperty.REPEATABLE_LABEL);
        out.value(value.isRepeatable());

        out.name(DataSourceSchemaProperty.LIMIT_LABEL);
        out.value(value.getLimit());

        out.endObject();
    }
}
