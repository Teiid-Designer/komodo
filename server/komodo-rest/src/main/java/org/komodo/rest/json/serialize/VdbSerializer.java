/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.json.RestVdb;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdb}s.
 */
public final class VdbSerializer extends KomodoRestEntitySerializer< RestVdb > {

    private boolean isComplete( final RestVdb vdb ) {
        return !StringUtils.isBlank( vdb.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdb read( final JsonReader in ) throws IOException {
        final RestVdb vdb = new RestVdb();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.DESCRIPTION:
                    final String description = in.nextString();
                    vdb.setDescription( description );
                    break;
                case JsonConstants.ID:
                    final String id = in.nextString();
                    vdb.setName( id );
                    break;
                case JsonConstants.LINKS:
                    readLinks( in, vdb );
                    break;
                case JsonConstants.ORIGINAL_FILE:
                    final String path = in.nextString();
                    vdb.setOriginalFilePath( path );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, vdb );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        if ( !isComplete( vdb ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdb.class.getSimpleName() ) );
        }

        endRead( in );
        return vdb;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.json.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdb value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdb.class.getSimpleName() ) );
        }

        beginWrite( out );

        // id
        out.name( JsonConstants.ID );
        out.value( value.getName() );

        // description
        if ( !StringUtils.isBlank( value.getDescription() ) ) {
            out.name( JsonConstants.DESCRIPTION );
            out.value( value.getDescription() );
        }

        // original file
        if ( !StringUtils.isBlank( value.getOriginalFilePath() ) ) {
            out.name( JsonConstants.ORIGINAL_FILE );
            out.value( value.getOriginalFilePath() );
        }

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
