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
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.spi.repository.DocumentType;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoStorageObject}s.
 */
public final class StorageAttributesSerializer extends AbstractContentSerializer<KomodoStorageAttributes> {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoStorageAttributes read( final JsonReader in ) throws IOException {
        final KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (readContent(in, storageAttr, name) != null)
                continue;

            switch ( name ) {
                case KomodoStorageAttributes.STORAGE_TYPE_LABEL:
                    storageAttr.setStorageType(in.nextString());
                    break;
                case KomodoStorageAttributes.ARTIFACT_PATH_LABEL:
                    storageAttr.setArtifactPath(in.nextString());
                    break;
                case KomodoStorageAttributes.PARAMETERS_LABEL:
                    Map<String, String> parameters = BUILDER.fromJson(in, Map.class);
                    for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                        storageAttr.setParameter(parameter.getKey(), parameter.getValue());
                    }
                    break;
                case KomodoStorageAttributes.DOCUMENT_TYPE_LABEL:
                    String docTypeValue = in.nextString();
                    DocumentType docType = DocumentType.documentType(docTypeValue);
                    storageAttr.setDocumentType(docType);
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return storageAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoStorageAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoStorageAttributes.STORAGE_TYPE_LABEL);
        out.value(value.getStorageType());

        out.name(KomodoStorageAttributes.ARTIFACT_PATH_LABEL);
        out.value(value.getArtifactPath());

        writeContent(out, value);

        String docType = value.getDocumentType();
        if (docType != null) {
            out.name(KomodoStorageAttributes.DOCUMENT_TYPE_LABEL);
            out.value(docType.toString());
        }

        if (! value.getParameters().isEmpty()) {
            out.name(KomodoStorageAttributes.PARAMETERS_LABEL);
            BUILDER.toJson(value.getParameters(), STRING_MAP_TYPE, out);
        }

        out.endObject();
    }

}
