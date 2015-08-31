/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.json.RestVdbDescriptor;
import org.komodo.rest.json.RestVdbDirectory;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbDirectory}s.
 */
public final class VdbDirectorySerializer extends KomodoRestEntitySerializer< RestVdbDirectory > {

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbDirectory read( final JsonReader in ) throws IOException {
        final RestVdbDirectory vdbDir = new RestVdbDirectory();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.LINKS:
                    readLinks( in, vdbDir );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, vdbDir );
                    break;
                case JsonConstants.VDBS:
                    final RestVdbDescriptor[] vdbs = JSON_BUILDER.fromJson( in, RestVdbDescriptor[].class );
                    vdbDir.setVdbs( vdbs );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        endRead( in );
        return vdbDir;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.json.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbDirectory value ) throws IOException {
        beginWrite( out );
        out.name( JsonConstants.VDBS );

        out.beginArray();
        for ( final RestVdbDescriptor vdb : value.getDescriptors() ) {
            JSON_BUILDER.toJson( vdb, RestVdbDescriptor.class, out );
        }
        out.endArray();

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
