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
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbSerializer extends KomodoRestEntitySerializer<RestVdb> {

    protected boolean isComplete(final RestVdb vdb) {
        return super.isComplete(vdb) && !StringUtils.isBlank(vdb.getName());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdb read(final JsonReader in) throws IOException {
        final RestVdb vdb = new RestVdb();

        beginRead(in);

        readBasicProperties(in, vdb);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestVdb.NAME_LABEL.equals(name))
                vdb.setName(in.nextString());
            else if (RestVdb.DESCRIPTION_LABEL.equals(name))
                vdb.setDescription(in.nextString());
            else if (RestVdb.FILE_PATH_LABEL.equals(name))
                vdb.setOriginalFilePath(in.nextString());
            else if (RestVdb.PREVIEW_LABEL.equals(name))
                vdb.setPreview(in.nextBoolean());
            else if (RestVdb.CONNECTION_TYPE_LABEL.equals(name))
                vdb.setConnectionType(in.nextString());
            else if (RestVdb.VERSION_LABEL.equals(name))
                vdb.setVersion(in.nextInt());
            else if (JsonConstants.PROPERTIES.equals(name))
                readProperties(in, vdb);
            else if (JsonConstants.LINKS.equals(name))
                readLinks(in, vdb);
            else
                throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
        }

        if (!isComplete(vdb)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdb.class.getSimpleName()));
        }

        endRead(in);
        return vdb;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write(final JsonWriter out, final RestVdb vdb) throws IOException {
        if (!isComplete(vdb)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdb.class.getSimpleName()));
        }

        beginWrite(out);

        writeBasicProperties(out, vdb);

        out.name(RestVdb.NAME_LABEL);
        out.value(vdb.getName());

        out.name(RestVdb.DESCRIPTION_LABEL);
        out.value(vdb.getDescription());

        out.name(RestVdb.FILE_PATH_LABEL);
        out.value(vdb.getOriginalFilePath());

        out.name(RestVdb.PREVIEW_LABEL);
        out.value(vdb.isPreview());

        out.name(RestVdb.CONNECTION_TYPE_LABEL);
        out.value(vdb.getConnectionType());

        out.name(RestVdb.VERSION_LABEL);
        out.value(vdb.getVersion());

        writeProperties(out, vdb);

        writeLinks(out, vdb);

        endWrite(out);
    }

}
