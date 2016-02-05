/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import javax.ws.rs.core.UriBuilder;
import org.komodo.rest.Messages;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.RestVdbImport;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestLink}s.
 */
public final class LinkSerializer extends TypeAdapter< RestLink > {

    private boolean isComplete( final RestLink link ) {
        return ( ( link.getRel() != null ) && ( link.getHref() != null ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestLink read( final JsonReader in ) throws IOException {
        final RestLink link = new RestLink();
        boolean foundHref = false;
        boolean foundRel = false;

        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if ( !foundHref && JsonConstants.HREF.equals( name ) ) {
                final String uri = in.nextString();
                link.setHref( UriBuilder.fromUri( uri ).build() );
                foundHref = true;
            } else if ( !foundRel && JsonConstants.REL.equals( name ) ) {
                final String rel = in.nextString();
                link.setRel( LinkType.fromString( rel ) );
                foundRel = true;
            } else {
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
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestLink.class.getSimpleName() ) );
        }

        out.beginObject();

        // rel
        out.name( JsonConstants.REL );
        out.value( value.getRel().toString() );

        // href
        out.name( JsonConstants.HREF );
        out.value( value.getHref().toString() );

        out.endObject();
    }

}
