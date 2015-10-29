/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbTranslator}s.
 */
public final class VdbTranslatorSerializer extends KomodoRestEntitySerializer<RestVdbTranslator> {

    private boolean isComplete(final RestVdbTranslator translator) {
        return (!StringUtils.isBlank(translator.getId()) && !StringUtils.isBlank(translator.getType()));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbTranslator read(final JsonReader in) throws IOException {
        final RestVdbTranslator translator = new RestVdbTranslator();

        beginRead(in);

        readBasicProperties(in, translator);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestVdbTranslator.DESCRIPTION_LABEL.equals(name))
                translator.setDescription(in.nextString());
            else if (RestVdbTranslator.TYPE_LABEL.equals(name))
                translator.setType(in.nextString());
            else if (JsonConstants.PROPERTIES.equals(name))
                readProperties(in, translator);
            else if (JsonConstants.LINKS.equals(name))
                readLinks(in, translator);
            else
                throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
        }

        if (!isComplete(translator)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbTranslator.class.getSimpleName()));
        }

        endRead(in);
        return translator;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write(final JsonWriter out, final RestVdbTranslator translator) throws IOException {
        if (!isComplete(translator)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbTranslator.class.getSimpleName()));
        }

        beginWrite(out);

        writeBasicProperties(out, translator);

        // type
        out.name(RestVdbTranslator.TYPE_LABEL);
        out.value(translator.getType());

        // description
        if (!StringUtils.isBlank(translator.getDescription())) {
            out.name(RestVdbTranslator.DESCRIPTION_LABEL);
            out.value(translator.getDescription());
        }

        writeProperties(out, translator);
        writeLinks(out, translator);
        endWrite(out);
    }

}
