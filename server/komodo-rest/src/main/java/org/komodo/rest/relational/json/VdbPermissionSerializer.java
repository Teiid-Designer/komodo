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
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.util.Map;
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

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RelationalJsonConstants.ALLOW_ALTER: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowAlter( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_CREATE: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowCreate( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_DELETE: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowDelete( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_EXECUTE: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowExecute( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_LANGUAGE: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowLanguage( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_READ: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowRead( allow );
                    break;
                }
                case RelationalJsonConstants.ALLOW_UPDATE: {
                    final boolean allow = in.nextBoolean();
                    permission.setAllowUpdate( allow );
                    break;
                }
                case RelationalJsonConstants.CONDITIONS: {
                    final Map< String, Boolean > conditions = BUILDER.fromJson( in, Map.class );
                    permission.setConditions( conditions );
                    break;
                }
                case JsonConstants.ID: {
                    final String permissionName = in.nextString();
                    permission.setName( permissionName );
                    break;
                }
                case RelationalJsonConstants.MASKS: {
                    final Map< String, String > masks = BUILDER.fromJson( in, Map.class );
                    permission.setMasks( masks );
                    break;
                }
                case JsonConstants.LINKS: {
                    readLinks( in, permission );
                    break;
                }
                case JsonConstants.PROPERTIES: {
                    readProperties( in, permission );
                    break;
                }
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
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

        // id
        out.name( JsonConstants.ID );
        out.value( value.getName() );

        // allow alter
        out.name( RelationalJsonConstants.ALLOW_ALTER );
        out.value( value.isAllowAlter() );

        // allow create
        out.name( RelationalJsonConstants.ALLOW_CREATE );
        out.value( value.isAllowCreate() );

        // allow delete
        out.name( RelationalJsonConstants.ALLOW_DELETE );
        out.value( value.isAllowDelete() );

        // allow execute
        out.name( RelationalJsonConstants.ALLOW_EXECUTE );
        out.value( value.isAllowExecute() );

        // allow language
        out.name( RelationalJsonConstants.ALLOW_LANGUAGE );
        out.value( value.isAllowLanguage() );

        // allow read
        out.name( RelationalJsonConstants.ALLOW_READ );
        out.value( value.isAllowRead() );

        // allow update
        out.name( RelationalJsonConstants.ALLOW_UPDATE );
        out.value( value.isAllowUpdate() );

        // conditions
        out.name( RelationalJsonConstants.CONDITIONS );
        BUILDER.toJson( value.getConditions(), BOOLEAN_MAP_TYPE, out );

        // masks
        out.name( RelationalJsonConstants.MASKS );
        BUILDER.toJson( value.getMasks(), STRING_MAP_TYPE, out );

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
