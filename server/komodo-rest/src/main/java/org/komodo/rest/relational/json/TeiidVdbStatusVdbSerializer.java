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
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestTeiidVdbStatusVdb;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestTeiidVdbStatusVdb}s.
 */
public final class TeiidVdbStatusVdbSerializer extends TypeAdapter< RestTeiidVdbStatusVdb > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestTeiidVdbStatusVdb read( final JsonReader in ) throws IOException {
        final RestTeiidVdbStatusVdb vdb = new RestTeiidVdbStatusVdb();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_NAME:
                    vdb.setName(in.nextString());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_DEPLOYED_NAME:
                    vdb.setDeployedName(in.nextString());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_VERSION:
                    vdb.setVersion(in.nextString());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_ACTIVE:
                    vdb.setActive(in.nextBoolean());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_LOADING:
                    vdb.setLoading(in.nextBoolean());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_FAILED:
                    vdb.setFailed(in.nextBoolean());
                    break;
                case RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_ERROR:
                    final String[] errors = BUILDER.fromJson( in, String[].class );
                    vdb.setErrors(Arrays.asList(errors));
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return vdb;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestTeiidVdbStatusVdb value ) throws IOException {

        out.beginObject();

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_NAME);
        out.value(value.getName());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_DEPLOYED_NAME);
        out.value(value.getDeployedName());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_VERSION);
        out.value(value.getVersion());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_ACTIVE);
        out.value(value.isActive());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_LOADING);
        out.value(value.isLoading());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_FAILED);
        out.value(value.isFailed());

        out.name(RestTeiidVdbStatusVdb.TEIID_VDB_STATUS_ERROR);
        out.beginArray();
        for (String val: value.getErrors()) {
            out.value(val);
        }
        out.endArray();

        out.endObject();
    }
}
