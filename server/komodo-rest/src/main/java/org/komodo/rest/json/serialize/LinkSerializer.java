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
import javax.ws.rs.core.UriBuilder;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.json.RestLink;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.rest.json.RestLink.MethodType;
import org.komodo.rest.json.RestVdbEntry;
import org.komodo.rest.json.RestVdbImport;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestLink}s.
 */
public final class LinkSerializer extends TypeAdapter< RestLink > {

    private boolean isComplete( final RestLink link ) {
        return ( ( link.getRel() != null ) && ( link.getHref() != null ) && ( link.getMethod() != null ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestLink read( final JsonReader in ) throws IOException {
        final RestLink link = new RestLink();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.HREF:
                    final String uri = in.nextString();
                    link.setHref( UriBuilder.fromUri( uri ).build() );
                    break;
                case JsonConstants.METHOD:
                    final String method = in.nextString();
                    link.setMethod( MethodType.fromString( method ) );
                    break;
                case JsonConstants.REL:
                    final String rel = in.nextString();
                    link.setRel( LinkType.fromString( rel ) );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        if ( !isComplete( link ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbImport.class.getSimpleName() ) );
        }

        return link;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestLink value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbEntry.class.getSimpleName() ) );
        }

        out.beginObject();

        // rel
        out.name( JsonConstants.REL );
        out.value( value.getRel().toString().toLowerCase() );

        // href
        out.name( JsonConstants.HREF );
        out.value( value.getHref().toString() );

        // method
        out.name( JsonConstants.METHOD );
        out.value( value.getMethod().name() );

        out.endObject();
    }

}
