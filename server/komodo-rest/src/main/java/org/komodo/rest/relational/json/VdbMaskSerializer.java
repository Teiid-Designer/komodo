/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.RestVdbMask;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbMask}s.
 */
public final class VdbMaskSerializer extends KomodoRestEntitySerializer< RestVdbMask > {

    private boolean isComplete( final RestVdbMask condition ) {
        return !StringUtils.isBlank( condition.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbMask read( final JsonReader in ) throws IOException {
        final RestVdbMask mask = new RestVdbMask();

        beginRead( in );

        readBasicProperties(in, mask);

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (RestVdbMask.NAME_LABEL.equals(name)) {
                mask.setName(in.nextString());
            }
            else if (RestVdbMask.ORDER_LABEL.equals(name)) {
                final String order = in.nextString();
                mask.setOrder(order);
            }
            else
                throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
        }

        if ( !isComplete( mask ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbMask.class.getSimpleName() ) );
        }

        endRead( in );
        return mask;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbMask value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbMask.class.getSimpleName() ) );
        }

        beginWrite( out );

        writeBasicProperties(out, value);

        // name
        out.name(RestVdbMask.NAME_LABEL);
        out.value(value.getName());

        // allow alter
        out.name(RestVdbMask.ORDER_LABEL);
        out.value(value.getOrder());

        endWrite( out );
    }

}
