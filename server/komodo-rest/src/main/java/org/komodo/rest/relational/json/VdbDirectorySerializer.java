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
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbDescriptor;
import org.komodo.rest.relational.RestVdbDirectory;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbDirectory}s.
 */
public final class VdbDirectorySerializer extends KomodoRestEntitySerializer< RestVdbDirectory > {

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
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
                case RelationalJsonConstants.VDBS:
                    final RestVdbDescriptor[] vdbs = BUILDER.fromJson( in, RestVdbDescriptor[].class );
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
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbDirectory value ) throws IOException {
        beginWrite( out );

        out.name( RelationalJsonConstants.VDBS );
        BUILDER.toJson( value.getDescriptors(), RestVdbDescriptor[].class, out );

        writeProperties( out, value );
        writeLinks( out, value );

        endWrite( out );
    }

}
