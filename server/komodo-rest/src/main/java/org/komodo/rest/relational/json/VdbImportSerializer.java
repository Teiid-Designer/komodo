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
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbImport}s.
 */
public final class VdbImportSerializer extends KomodoRestEntitySerializer< RestVdbImport > {

    private boolean isComplete( final RestVdbImport vdbImport ) {
        return !StringUtils.isBlank( vdbImport.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbImport read( final JsonReader in ) throws IOException {
        final RestVdbImport vdbImport = new RestVdbImport();

        beginRead( in );

        readBasicProperties(in, vdbImport);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestVdbImport.NAME_LABEL.equals(name))
                vdbImport.setName(in.nextString());
            else if (RestVdbImport.IMPORT_POLICIES_LABEL.equals(name))
                vdbImport.setImportDataPolicies(in.nextBoolean());
            else if (RestVdbImport.VERSION_LABEL.equals(name))
                vdbImport.setVersion(in.nextInt());
            else if (JsonConstants.LINKS.equals(name))
                readLinks(in, vdbImport);
            else
                throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
        }

        if (!isComplete(vdbImport)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbImport.class.getSimpleName()));
        }

        endRead( in );
        return vdbImport;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbImport vdbImport ) throws IOException {
        if ( !isComplete( vdbImport ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbImport.class.getSimpleName() ) );
        }

        beginWrite( out );

        writeBasicProperties(out, vdbImport);

        // name
        out.name(RestVdbImport.NAME_LABEL);
        out.value(vdbImport.getName());

        // version
        out.name(RestVdbImport.VERSION_LABEL);
        out.value(vdbImport.getVersion());

        // importDataPolicies
        out.name(RestVdbImport.IMPORT_POLICIES_LABEL);
        out.value(vdbImport.isImportDataPolicies());

        writeLinks( out, vdbImport );
        endWrite( out );
    }

}
