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
import org.komodo.rest.relational.RestVdbCondition;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbCondition}s.
 */
public final class VdbConditionSerializer extends KomodoRestEntitySerializer< RestVdbCondition > {

    private boolean isComplete( final RestVdbCondition condition ) {
        return !StringUtils.isBlank( condition.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbCondition read( final JsonReader in ) throws IOException {
        final RestVdbCondition condition = new RestVdbCondition();

        beginRead( in );

        readBasicProperties(in, condition);

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (RestVdbCondition.NAME_LABEL.equals(name)) {
                condition.setName(in.nextString());
            }
            else if (RestVdbCondition.CONSTRAINT_LABEL.equals(name)) {
                final boolean constraint = in.nextBoolean();
                condition.setConstraint(constraint);
            }
            else
                throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
        }

        if ( !isComplete( condition ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbCondition.class.getSimpleName() ) );
        }

        endRead( in );
        return condition;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbCondition value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbCondition.class.getSimpleName() ) );
        }

        beginWrite( out );

        writeBasicProperties(out, value);

        // name
        out.name(RestVdbCondition.NAME_LABEL);
        out.value(value.getName());

        // allow alter
        out.name(RestVdbCondition.CONSTRAINT_LABEL);
        out.value(value.isConstraint());

        endWrite( out );
    }

}
