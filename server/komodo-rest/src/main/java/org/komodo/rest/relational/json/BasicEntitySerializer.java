/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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

    protected T createEntity() {
        return (T) new RestBasicEntity();
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
                        entity.addTuple(name, in.nextInt());
                        break;
                    case STRING:
                        entity.addTuple(name, in.nextString());
                        break;
                    case NULL:
                        in.nextNull();
                        entity.addTuple(name, null);
                        break;
                    case BEGIN_ARRAY:
                        final String[] value = BUILDER.fromJson( in, String[].class );
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

        for (Map.Entry<String, Object>entry : entity.getTuples().entrySet()) {
            out.name(entry.getKey());
            Object value = entry.getValue();
            if (value == null)
                out.nullValue();
            else if (value instanceof Boolean)
                out.value((Boolean) value);
            else if (value instanceof Integer)
                out.value((Integer) value);
            else if (value instanceof Boolean)
                out.value((Boolean) value);
            else if (value instanceof String[]) {
                out.beginArray();
                for (String val: (String[]) value) {
                    out.value(val);
                }
                out.endArray();
            } else
                out.value(value.toString());
        }

        writeProperties(out, entity);

        writeLinks(out, entity);

        endWrite(out);
    }

}
