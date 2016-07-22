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
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestStorageType;
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestStorageType}s.
 */
public class StorageTypeSerializer extends TypeAdapter< RestStorageType > {

        /**
         * {@inheritDoc}
         *
         * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
         */
        @Override
        public RestStorageType read( final JsonReader in ) throws IOException {
            final RestStorageType storageType = new RestStorageType();
            in.beginObject();

            while ( in.hasNext() ) {
                final String name = in.nextName();

                switch ( name ) {
                    case RestStorageType.NAME_LABEL:
                        storageType.setName(in.nextString());
                        break;
                    case RestStorageType.DESCRIPTION_LABEL:
                        storageType.setDescription(in.nextString());
                        break;
                    case RestStorageType.DESCRIPTORS_LABEL:
                        RestStorageTypeDescriptor[] descriptors = BUILDER.fromJson(in, RestStorageTypeDescriptor[].class);
                        storageType.setDescriptors(descriptors);
                        break;
                    default:
                        throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
                }
            }

            in.endObject();

            return storageType;
        }

        /**
         * {@inheritDoc}
         *
         * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
         */
        @Override
        public void write( final JsonWriter out, final RestStorageType value ) throws IOException {

            out.beginObject();

            // Title of object
            out.name(RestStorageType.NAME_LABEL);
            out.value(value.getName());

            out.name(RestStorageType.DESCRIPTION_LABEL);
            out.value(value.getDescription());

            if (value.getDescriptors() == null || value.getDescriptors().isEmpty())
                return;

            out.name(RestStorageType.DESCRIPTORS_LABEL);
            BUILDER.toJson(value.getDescriptors().toArray(new RestStorageTypeDescriptor[0]), RestStorageTypeDescriptor[].class, out);

            out.endObject();
        }
}
