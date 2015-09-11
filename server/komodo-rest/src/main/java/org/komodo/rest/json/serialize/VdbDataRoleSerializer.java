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
import static org.komodo.rest.json.serialize.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.json.RestVdbDataRole;
import org.komodo.rest.json.RestVdbPermission;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbDataRole}s.
 */
public final class VdbDataRoleSerializer extends KomodoRestEntitySerializer< RestVdbDataRole > {

    private boolean isComplete( final RestVdbDataRole dataRole ) {
        return !StringUtils.isBlank( dataRole.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbDataRole read( final JsonReader in ) throws IOException {
        final RestVdbDataRole dataRole = new RestVdbDataRole();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.ALLOW_CREATE_TEMP_TABLES:
                    final boolean allowCreateTempTables = in.nextBoolean();
                    dataRole.setAllowCreateTempTables( allowCreateTempTables );
                    break;
                case JsonConstants.ANY_AUTHENTICATED:
                    final boolean anyAuthenticated = in.nextBoolean();
                    dataRole.setAnyAuthenticated( anyAuthenticated );
                    break;
                case JsonConstants.DESCRIPTION:
                    final String description = in.nextString();
                    dataRole.setDescription( description );
                    break;
                case JsonConstants.ID:
                    final String dataRoleName = in.nextString();
                    dataRole.setName( dataRoleName );
                    break;
                case JsonConstants.GRANT_ALL:
                    final boolean grantAll = in.nextBoolean();
                    dataRole.setGrantAll( grantAll );
                    break;
                case JsonConstants.LINKS:
                    readLinks( in, dataRole );
                    break;
                case JsonConstants.MAPPED_ROLES:
                    final String[] mappedRoles = BUILDER.fromJson( in, String[].class );
                    dataRole.setMappedRoles( mappedRoles );
                    break;
                case JsonConstants.PERMISSIONS:
                    final RestVdbPermission[] permissions = BUILDER.fromJson( in, RestVdbPermission[].class );
                    dataRole.setPermissions( permissions );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, dataRole );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        if ( !isComplete( dataRole ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbDataRole.class.getSimpleName() ) );
        }

        endRead( in );
        return dataRole;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.json.serialize.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.json.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbDataRole value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbDataRole.class.getSimpleName() ) );
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

        // create temp tables
        out.name( JsonConstants.ALLOW_CREATE_TEMP_TABLES );
        out.value( value.isAllowCreateTempTables() );

        // any authenticated
        out.name( JsonConstants.ANY_AUTHENTICATED );
        out.value( value.isAnyAuthenticated() );

        // grant all
        out.name( JsonConstants.GRANT_ALL );
        out.value( value.isGrantAll() );

        // mapped roles
        out.name( JsonConstants.MAPPED_ROLES );

        out.beginArray();
        for ( final String roleName : value.getMappedRoles() ) {
            out.value( roleName );
        }
        out.endArray();

        // permissions
        out.name( JsonConstants.PERMISSIONS );
        BUILDER.toJson( value.getPermissions(), RestVdbPermission[].class, out );
//
//        out.beginArray();
//        for ( final RestVdbPermission permission : value.getPermissions() ) {
//            BUILDER.getAdapter( RestVdbPermission.class ).write( out, permission );
//        }
//        out.endArray();

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
