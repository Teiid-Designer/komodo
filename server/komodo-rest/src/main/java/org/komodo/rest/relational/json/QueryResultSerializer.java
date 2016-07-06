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
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestQueryResult}.
 */
public class QueryResultSerializer extends TypeAdapter<RestQueryResult> {

    @Override
    public RestQueryResult read(JsonReader in) throws IOException {
        final RestQueryResult queryResult = new RestQueryResult();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestQueryResult.COLUMNS_LABEL:
                    RestQueryColumn[] columns = BUILDER.fromJson(in, RestQueryColumn[].class);
                    queryResult.setColumns(columns);
                    break;
                case RestQueryResult.ROWS_LABEL:
                    RestQueryRow[] rows = BUILDER.fromJson(in, RestQueryRow[].class);
                    queryResult.setRows(rows);
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return queryResult;
    }

    @Override
    public void write(JsonWriter out, RestQueryResult value) throws IOException {
        out.beginObject();

        if (value.getColumns().length > 0) {
            out.name(RestQueryResult.COLUMNS_LABEL);
            BUILDER.toJson(value.getColumns(), RestQueryColumn[].class, out);
        }

        if (value.getRows().length > 0) {
            out.name(RestQueryResult.ROWS_LABEL);
            BUILDER.toJson(value.getRows(), RestQueryRow[].class, out);
        }

        out.endObject();
    }
}
