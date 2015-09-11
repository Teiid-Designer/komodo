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
import org.komodo.rest.json.RestVdbTranslator;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbTranslator}s.
 */
public final class VdbTranslatorSerializer extends KomodoRestEntitySerializer< RestVdbTranslator > {

    private boolean isComplete( final RestVdbTranslator translator ) {
        return ( !StringUtils.isBlank( translator.getName() ) && !StringUtils.isBlank( translator.getType() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbTranslator read( final JsonReader in ) throws IOException {
        final RestVdbTranslator translator = new RestVdbTranslator();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.DESCRIPTION:
                    final String description = in.nextString();
                    translator.setDescription( description );
                    break;
                case JsonConstants.ID:
                    final String id = in.nextString();
                    translator.setName( id );
                    break;
                case JsonConstants.LINKS:
                    readLinks( in, translator );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, translator );
                    break;
                case JsonConstants.TYPE:
                    final String type = in.nextString();
                    translator.setType( type );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        if ( !isComplete( translator ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbTranslator.class.getSimpleName() ) );
        }

        endRead( in );
        return translator;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.json.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbTranslator value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbTranslator.class.getSimpleName() ) );
        }

        beginWrite( out );

        // id
        out.name( JsonConstants.ID );
        out.value( value.getName() );

        // type
        out.name( JsonConstants.TYPE );
        out.value( value.getType() );

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
