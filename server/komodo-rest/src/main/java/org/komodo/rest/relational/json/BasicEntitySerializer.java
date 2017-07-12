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

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.RestBasicEntity;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestBasicEntity}s.
 * @param <T> the specific type of {@link RestBasicEntity}
 */
public class BasicEntitySerializer<T extends RestBasicEntity> extends AbstractEntitySerializer<T> {

    @SuppressWarnings( "unchecked" )
    protected T createEntity() {
        return (T) new RestBasicEntity();
    }

    /**
     * Sub-classes should implement this to write further data to the json
     *
     * @param name
     * @param entity
     * @param in
     *
     * @throws IOException
     */
    protected String readExtension(String name, T entity, JsonReader in) {
        // Do nothing by default
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.AbstractEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public T read(final JsonReader in) throws IOException {
        final T entity = createEntity();

        beginRead(in);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (readExtension(name, entity, in) != null)
                continue;

            if (PROPERTIES.equals(name))
                readProperties(in, entity);
            else if (LINKS.equals(name))
                readLinks(in, entity);
            else {
                JsonToken token = in.peek();
                switch (token) {
                    case BOOLEAN:
                        entity.addTuple(name, in.nextBoolean());
                        break;
                    case NUMBER:
                    {
                        double value = in.nextDouble();
                        if (value % 1 == 0)
                            entity.addTuple(name, (int) value);
                        else
                            entity.addTuple(name, value);
                        break;
                    }
                    case STRING:
                        entity.addTuple(name, in.nextString());
                        break;
                    case NULL:
                        in.nextNull();
                        entity.addTuple(name, null);
                        break;
                    case BEGIN_ARRAY:
                        final Object[] value = BUILDER.fromJson( in, Object[].class );

                        //
                        // BUILDER always converts json numbers to double regardless
                        // of them being integers so need to do some checking and on-the-fly
                        // conversion
                        //
                        for (int i = 0; i < value.length; ++i) {
                            if (value[i] instanceof Double && ((double) value[i] % 1) == 0)
                                value[i] = ((Double) value[i]).intValue();
                        }

                        entity.addTuple(name, value);
                        break;
                    default:
                        throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
                }
            }
        }

        if ( !isComplete( entity ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName() ) );
        }

        endRead(in);
        return entity;
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

    protected void writeTuples(final JsonWriter out, final T entity) throws IOException {
        for (Map.Entry<String, Object>entry : entity.getTuples().entrySet()) {
            out.name(entry.getKey());
            Object value = entry.getValue();
            writeValue(out, value);
        }
    }

    /**
     * Sub-classes should implement this to write further data to the json
     *
     * @param out
     * @param entity
     * @throws IOException
     */
    protected void writeExtensions(final JsonWriter out, final T entity) throws IOException {
        // Do nothing by default
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.AbstractEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.RestBasicEntity)
     */
    @Override
    public void write(final JsonWriter out, final T entity) throws IOException {
        if (!isComplete(entity)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, getClass().getSimpleName()));
        }

        beginWrite(out);

        writeTuples(out, entity);

        writeExtensions(out, entity);

        writeProperties(out, entity);

        writeLinks(out, entity);

        endWrite(out);
    }
}
