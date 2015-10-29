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
import org.komodo.relational.model.Model;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbModel;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbModelSerializer extends KomodoRestEntitySerializer<RestVdbModel> {

    protected boolean isComplete(final RestVdbModel model) {
        return super.isComplete(model) && model.getModelType() != null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbModel read(final JsonReader in) throws IOException {
        final RestVdbModel model = new RestVdbModel();

        beginRead(in);

        readBasicProperties(in, model);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestVdbModel.DESCRIPTION_LABEL.equals(name))
                model.setDescription(in.nextString());
            else if (RestVdbModel.MODEL_TYPE_LABEL.equals(name)) {
                String modelType = in.nextString();
                Model.Type type = Model.Type.findType(modelType);
                model.setModelType(type);
            } else if (RestVdbModel.VISIBLE_LABEL.equals(name))
                model.setVisible(in.nextBoolean());
            else if (RestVdbModel.METADATA_TYPE_LABEL.equals(name))
                model.setMetadataType(in.nextString());
            else if (JsonConstants.DDL_ATTRIBUTE.equals(name))
                model.setDdl(in.nextString());
            else if (JsonConstants.PROPERTIES.equals(name))
                readProperties(in, model);
            else if (JsonConstants.LINKS.equals(name))
                readLinks(in, model);
            else
                throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
        }

        if (!isComplete(model)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdb.class.getSimpleName()));
        }

        endRead(in);
        return model;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write(final JsonWriter out, final RestVdbModel model) throws IOException {
        if (!isComplete(model)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbModel.class.getSimpleName()));
        }

        beginWrite(out);

        writeBasicProperties(out, model);

        out.name(RestVdbModel.DESCRIPTION_LABEL);
        out.value(model.getDescription());

        out.name(RestVdbModel.MODEL_TYPE_LABEL);
        out.value(model.getModelType() != null ? model.getModelType().toString() : null);

        out.name(RestVdbModel.VISIBLE_LABEL);
        out.value(model.isVisible());

        out.name(RestVdbModel.METADATA_TYPE_LABEL);
        out.value(model.getMetadataType());

        out.name(JsonConstants.DDL_ATTRIBUTE);
        out.value(model.getDdl());

        writeProperties(out, model);

        writeLinks(out, model);

        endWrite(out);
    }

}
