/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.KomodoSearcherAttributes;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoSearchObject}s.
 */
public final class SearcherAttributesSerializer extends TypeAdapter< KomodoSearcherAttributes > {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoSearcherAttributes read( final JsonReader in ) throws IOException {
        final KomodoSearcherAttributes searcherAttr = new KomodoSearcherAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoSearcherAttributes.SEARCH_NAME_LABEL:
                    searcherAttr.setSearchName(in.nextString());
                    break;
                case KomodoSearcherAttributes.TYPE_LABEL:
                    searcherAttr.setType(in.nextString());
                    break;
                case KomodoSearcherAttributes.PARENT_LABEL:
                    searcherAttr.setParent(in.nextString());
                    break;
                case KomodoSearcherAttributes.ANCESTOR_LABEL:
                    searcherAttr.setAncestor(in.nextString());
                    break;
                case KomodoSearcherAttributes.PATH_LABEL:
                    searcherAttr.setPath(in.nextString());
                    break;
                case KomodoSearcherAttributes.CONTAINS_LABEL:
                    searcherAttr.setContains(in.nextString());
                    break;
                case KomodoSearcherAttributes.OBJECT_NAME_LABEL:
                    searcherAttr.setObjectName(in.nextString());
                    break;
                case KomodoSearcherAttributes.PARAMETERS_LABEL:
                    Map<String, String> parameters = BUILDER.fromJson(in, Map.class);
                    for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                        searcherAttr.setParameter(parameter.getKey(), parameter.getValue());
                    }
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return searcherAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoSearcherAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoSearcherAttributes.SEARCH_NAME_LABEL);
        out.value(value.getSearchName());

        out.name(KomodoSearcherAttributes.ANCESTOR_LABEL);
        out.value(value.getAncestor());

        out.name(KomodoSearcherAttributes.CONTAINS_LABEL);
        out.value(value.getContains());

        out.name(KomodoSearcherAttributes.OBJECT_NAME_LABEL);
        out.value(value.getObjectName());

        out.name(KomodoSearcherAttributes.PARENT_LABEL);
        out.value(value.getParent());

        out.name(KomodoSearcherAttributes.PATH_LABEL);
        out.value(value.getPath());

        out.name(KomodoSearcherAttributes.TYPE_LABEL);
        out.value(value.getType());

        if (! value.getParameters().isEmpty()) {
            out.name(KomodoSearcherAttributes.PARAMETERS_LABEL);
            BUILDER.toJson(value.getParameters(), STRING_MAP_TYPE, out);
        }

        out.endObject();
    }

}
