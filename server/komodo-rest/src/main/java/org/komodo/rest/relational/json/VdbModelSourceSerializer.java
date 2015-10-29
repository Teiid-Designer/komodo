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
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbModelSource;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbModelSourceSerializer extends KomodoRestEntitySerializer<RestVdbModelSource> {

    protected boolean isComplete(final RestVdbModelSource source) {
        return super.isComplete(source);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbModelSource read(final JsonReader in) throws IOException {
        final RestVdbModelSource source = new RestVdbModelSource();

        beginRead(in);

        readBasicProperties(in, source);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestVdbModelSource.JNDI_NAME_LABEL.equals(name))
                source.setJndiName(in.nextString());
            else if (RestVdbModelSource.TRANSLATOR_LABEL.equals(name))
                source.setTranslator(in.nextString());
            else if (JsonConstants.LINKS.equals(name))
                readLinks(in, source);
            else
                throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
        }

        if (!isComplete(source)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdb.class.getSimpleName()));
        }

        endRead(in);
        return source;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write(final JsonWriter out, final RestVdbModelSource source) throws IOException {
        if (!isComplete(source)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbModelSource.class.getSimpleName()));
        }

        beginWrite(out);

        writeBasicProperties(out, source);

        out.name(RestVdbModelSource.JNDI_NAME_LABEL);
        out.value(source.getJndiName());

        out.name(RestVdbModelSource.TRANSLATOR_LABEL);
        out.value(source.getTranslator());

        writeLinks(out, source);

        endWrite(out);
    }

}
