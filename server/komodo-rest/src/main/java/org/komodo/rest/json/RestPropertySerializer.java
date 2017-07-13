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
package org.komodo.rest.json;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import org.komodo.rest.RestProperty;
import org.komodo.rest.Messages;
import org.komodo.utils.StringUtils;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

/**
 *
 */
public class RestPropertySerializer extends TypeAdapter<RestProperty> implements JsonConstants {

    protected boolean isComplete(final RestProperty property) {
        return !StringUtils.isBlank(property.getName());
    }

    @Override
    public RestProperty read(JsonReader in) throws IOException {

        String propName = null;
        Object propValue = null;

        in.beginObject();

        while (in.hasNext()) {
            String name = in.nextName();
            if (RestProperty.NAME_LABEL.equals(name))
                propName = in.nextString();
            else if (RestProperty.VALUE_LABEL.equals(name)) {
                JsonToken token = in.peek();
                switch (token) {
                    case BOOLEAN:
                        propValue = in.nextBoolean();
                        break;
                    case NUMBER: {
                        double value = in.nextDouble();
                        if (value % 1 == 0)
                            propValue = (int)value;
                        else propValue = value;
                        break;
                    }
                    case STRING:
                        propValue = in.nextString();
                        break;
                    case NULL:
                        in.nextNull();
                        propValue = null;
                        break;
                    case BEGIN_ARRAY:
                        final Object[] value = BUILDER.fromJson(in, Object[].class);

                        //
                        // BUILDER always converts json numbers to double regardless
                        // of them being integers so need to do some checking and on-the-fly
                        // conversion
                        //
                        for (int i = 0; i < value.length; ++i) {
                            if (value[i] instanceof Double && ((double)value[i] % 1) == 0)
                                value[i] = ((Double)value[i]).intValue();
                        }

                        propValue = value;
                        break;
                    default:
                        throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
                }
            }
        }

        in.endObject();

        RestProperty property = new RestProperty(propName, propValue);
        if (!isComplete(property))
            throw new IOException(Messages.getString(Messages.Error.INCOMPLETE_JSON, RestProperty.class.getSimpleName()));

        return property;
    }

    protected void writeValue(final JsonWriter out, Object value) throws IOException {
        if (value == null)
            out.nullValue();
        else if (value instanceof Boolean)
            out.value((Boolean) value);
        else if (value instanceof Integer)
            out.value((int) value);
        else if (value instanceof Long)
            out.value((long) value);
        else if (value instanceof Double)
            out.value((double) value);
        else if (value instanceof Float)
            out.value((double) value);
        else if (value instanceof String[]) {
            out.beginArray();
            for (String val: (String[]) value) {
                out.value(val);
            }
            out.endArray();
        } else if (value instanceof Object[]) {
            out.beginArray();
            for (Object val: (Object[]) value) {
                writeValue(out, val);
            }
            out.endArray();
        } else
            out.value(value.toString());
    }

    @Override
    public void write(JsonWriter out, RestProperty value) throws IOException {
        out.beginObject();

        out.name(RestProperty.NAME_LABEL);
        out.value(value.getName());

        out.name(RestProperty.VALUE_LABEL);
        
        writeValue(out, value.getValue());

        out.endObject();
    }
}
