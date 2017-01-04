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
import java.util.List;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.request.KomodoDataserviceUpdateAttributes;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoDataserviceUpdateAttribute}s.
 */
public final class DataserviceUpdateAttributesSerializer extends TypeAdapter< KomodoDataserviceUpdateAttributes > {

    private static final Type STRING_LIST_TYPE = new TypeToken< List< String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoDataserviceUpdateAttributes read( final JsonReader in ) throws IOException {
        final KomodoDataserviceUpdateAttributes updateAttrs = new KomodoDataserviceUpdateAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoDataserviceUpdateAttributes.DATASERVICE_NAME_LABEL:
                	updateAttrs.setDataserviceName(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_TABLE_PATH_LABEL:
                    updateAttrs.setTablePath(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_MODEL_SOURCE_PATH_LABEL:
                    updateAttrs.setModelSourcePath(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_COLUMN_NAMES_LABEL:
                    List<String> colNames = BUILDER.fromJson(in, List.class);
                    updateAttrs.setColumnNames(colNames);
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_RH_TABLE_PATH_LABEL:
                    updateAttrs.setRhTablePath(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_RH_MODEL_SOURCE_PATH_LABEL:
                    updateAttrs.setRhModelSourcePath(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_RH_COLUMN_NAMES_LABEL:
                    List<String> rhColNames = BUILDER.fromJson(in, List.class);
                    updateAttrs.setRhColumnNames(rhColNames);
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_TYPE_LABEL:
                    updateAttrs.setJoinType(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_LH_COLUMN_LABEL:
                    updateAttrs.setLhJoinColumn(in.nextString());
                    break;
                case KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_RH_COLUMN_LABEL:
                    updateAttrs.setRhJoinColumn(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return updateAttrs;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoDataserviceUpdateAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_NAME_LABEL);
        out.value(value.getDataserviceName());

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_TABLE_PATH_LABEL);
        out.value(value.getTablePath());

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_MODEL_SOURCE_PATH_LABEL);
        out.value(value.getModelSourcePath());
        
        if (! value.getColumnNames().isEmpty()) {
            out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_COLUMN_NAMES_LABEL);
            BUILDER.toJson(value.getColumnNames(), STRING_LIST_TYPE, out);
        }

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_RH_TABLE_PATH_LABEL);
        out.value(value.getRhTablePath());

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_RH_MODEL_SOURCE_PATH_LABEL);
        out.value(value.getRhModelSourcePath());
        
        if (! value.getRhColumnNames().isEmpty()) {
            out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_RH_COLUMN_NAMES_LABEL);
            BUILDER.toJson(value.getRhColumnNames(), STRING_LIST_TYPE, out);
        }

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_TYPE_LABEL);
        out.value(value.getJoinType());

        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_LH_COLUMN_LABEL);
        out.value(value.getLhJoinColumn());
        
        out.name(KomodoDataserviceUpdateAttributes.DATASERVICE_JOIN_RH_COLUMN_LABEL);
        out.value(value.getRhJoinColumn());

        out.endObject();
    }

}
