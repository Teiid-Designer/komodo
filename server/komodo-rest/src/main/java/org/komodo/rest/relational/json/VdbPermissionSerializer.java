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
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbPermission}s.
 */
public final class VdbPermissionSerializer extends KomodoRestEntitySerializer< RestVdbPermission > {

    private boolean isComplete( final RestVdbPermission permission ) {
        return !StringUtils.isBlank( permission.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbPermission read( final JsonReader in ) throws IOException {
        final RestVdbPermission permission = new RestVdbPermission();

        beginRead( in );

        readBasicProperties(in, permission);

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (RestVdbPermission.ALLOW_ALTER_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowAlter( allow );
            }
            else if (RestVdbPermission.ALLOW_CREATE_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowCreate( allow );
            }
            else if (RestVdbPermission.ALLOW_DELETE_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowDelete( allow );
            }
            else if (RestVdbPermission.ALLOW_EXECUTE_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowExecute( allow );
            }
            else if (RestVdbPermission.ALLOW_LANGUAGE_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowLanguage( allow );
            }
            else if (RestVdbPermission.ALLOW_READ_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowRead( allow );
            }
            else if (RestVdbPermission.ALLOW_UPDATE_LABEL.equals(name)) {
                final boolean allow = in.nextBoolean();
                permission.setAllowUpdate( allow );
            }
            else if (RestVdbPermission.NAME_LABEL.equals(name)) {
                final String permissionName = in.nextString();
                permission.setName( permissionName );
            }
            else if (JsonConstants.LINKS.equals(name)) {
                readLinks( in, permission );
            }
            else
                throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
        }

        if ( !isComplete( permission ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbPermission.class.getSimpleName() ) );
        }

        endRead( in );
        return permission;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbPermission value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbPermission.class.getSimpleName() ) );
        }

        beginWrite( out );

        writeBasicProperties(out, value);

        // name
        out.name(RestVdbPermission.NAME_LABEL);
        out.value(value.getName());

        // allow alter
        out.name(RestVdbPermission.ALLOW_ALTER_LABEL);
        out.value(value.isAllowAlter());

        // allow create
        out.name(RestVdbPermission.ALLOW_CREATE_LABEL);
        out.value(value.isAllowCreate());

        // allow delete
        out.name(RestVdbPermission.ALLOW_DELETE_LABEL);
        out.value(value.isAllowDelete());

        // allow execute
        out.name(RestVdbPermission.ALLOW_EXECUTE_LABEL);
        out.value(value.isAllowExecute());

        // allow language
        out.name(RestVdbPermission.ALLOW_LANGUAGE_LABEL);
        out.value(value.isAllowLanguage());

        // allow read
        out.name(RestVdbPermission.ALLOW_READ_LABEL);
        out.value(value.isAllowRead());

        // allow update
        out.name(RestVdbPermission.ALLOW_UPDATE_LABEL);
        out.value(value.isAllowUpdate());

        writeLinks( out, value );
        endWrite( out );
    }

}
