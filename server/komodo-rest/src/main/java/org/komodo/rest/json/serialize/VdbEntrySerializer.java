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
import org.komodo.rest.json.RestVdbEntry;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbEntry}s.
 */
public final class VdbEntrySerializer extends KomodoRestEntitySerializer< RestVdbEntry > {

    private boolean isComplete( final RestVdbEntry entry ) {
        return ( !StringUtils.isBlank( entry.getName() ) && !StringUtils.isBlank( entry.getPath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbEntry read( final JsonReader in ) throws IOException {
        final RestVdbEntry entry = new RestVdbEntry();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.DESCRIPTION:
                    final String description = in.nextString();
                    entry.setDescription( description );
                    break;
                case JsonConstants.ID:
                    final String id = in.nextString();
                    entry.setName( id );
                    break;
                case JsonConstants.LINKS:
                    readLinks( in, entry );
                    break;
                case JsonConstants.PATH:
                    final String path = in.nextString();
                    entry.setPath( path );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, entry );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        if ( !isComplete( entry ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbEntry.class.getSimpleName() ) );
        }

        endRead( in );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.json.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbEntry value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbEntry.class.getSimpleName() ) );
        }

        beginWrite( out );

        // id
        out.name( JsonConstants.ID );
        out.value( value.getName() );

        // path
        out.name( JsonConstants.PATH );
        out.value( value.getPath() );

        // description
        if ( !StringUtils.isBlank( value.getDescription() ) ) {
            out.name( JsonConstants.DESCRIPTION );
            out.value( value.getDescription() );
        }

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
