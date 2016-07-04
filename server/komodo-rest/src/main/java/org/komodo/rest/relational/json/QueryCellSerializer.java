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
import org.komodo.rest.relational.response.RestQueryCell;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestQueryCell}.
 */
public class QueryCellSerializer extends TypeAdapter<RestQueryCell> {

    @Override
    public RestQueryCell read(JsonReader in) throws IOException {
        final RestQueryCell queryCell = new RestQueryCell();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestQueryCell.VALUE_LABEL:
                    queryCell.setValue(in.nextString());
                    break;
                case RestQueryCell.TYPE_LABEL:
                    queryCell.setType(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return queryCell;
    }

    @Override
    public void write(JsonWriter out, RestQueryCell value) throws IOException {
        out.beginObject();

        out.name(RestQueryCell.VALUE_LABEL);
        out.value(value.getValue());

        out.name(RestQueryCell.TYPE_LABEL);
        out.value(value.getType());

        out.endObject();
    }
}
