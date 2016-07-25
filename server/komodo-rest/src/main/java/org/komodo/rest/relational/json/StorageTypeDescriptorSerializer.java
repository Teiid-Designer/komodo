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
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestStorageTypeDescriptor}.
 */
public class StorageTypeDescriptorSerializer extends TypeAdapter<RestStorageTypeDescriptor> {

    @Override
    public RestStorageTypeDescriptor read(JsonReader in) throws IOException {
        final RestStorageTypeDescriptor storageTypeDescriptor = new RestStorageTypeDescriptor();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestStorageTypeDescriptor.NAME_LABEL:
                    storageTypeDescriptor.setName(in.nextString());
                    break;
                case RestStorageTypeDescriptor.DESCRIPTION_LABEL:
                    storageTypeDescriptor.setDescription(in.nextString());
                    break;
                case RestStorageTypeDescriptor.REQUIRED_LABEL:
                    storageTypeDescriptor.setRequired(in.nextBoolean());
                    break;
                case RestStorageTypeDescriptor.ENCODED_LABEL:
                    storageTypeDescriptor.setEncoded(in.nextBoolean());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return storageTypeDescriptor;
    }

    @Override
    public void write(JsonWriter out, RestStorageTypeDescriptor value) throws IOException {
        out.beginObject();

        out.name(RestStorageTypeDescriptor.NAME_LABEL);
        out.value(value.getName());

        out.name(RestStorageTypeDescriptor.DESCRIPTION_LABEL);
        out.value(value.getDescription());

        out.name(RestStorageTypeDescriptor.REQUIRED_LABEL);
        out.value(value.isRequired());

        out.name(RestStorageTypeDescriptor.ENCODED_LABEL);
        out.value(value.isEncoded());

        out.endObject();
    }
}
