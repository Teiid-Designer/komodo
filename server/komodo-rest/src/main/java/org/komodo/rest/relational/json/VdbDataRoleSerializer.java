/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational.json;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.utils.StringUtils;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbDataRole}s.
 */
public final class VdbDataRoleSerializer extends BasicEntitySerializer<RestVdbDataRole> {

    @Override
    protected boolean isComplete( final RestVdbDataRole dataRole ) {
        return super.isComplete(dataRole) && !StringUtils.isBlank( dataRole.getName() );
    }

    @Override
    protected RestVdbDataRole createEntity() {
        return new RestVdbDataRole();
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#readExtension(java.lang.String, org.komodo.rest.RestBasicEntity,
     *      com.google.gson.stream.JsonReader)
     */
    @Override
    protected String readExtension( final String name,
                                    final RestVdbDataRole dataRole,
                                    final JsonReader in ) {
        if ( RestVdbDataRole.PERMISSIONS_LABEL.equals( name ) ) {
            final RestVdbPermission[] permissions = BUILDER.fromJson( in, RestVdbPermission[].class );
            dataRole.setPermissions( permissions );
            return Integer.toString( permissions.length );
        }

        return null; // not processed
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#writeExtensions(com.google.gson.stream.JsonWriter, org.komodo.rest.RestBasicEntity)
     */
    @Override
    protected void writeExtensions( final JsonWriter out,
                                    final RestVdbDataRole dataRole ) throws IOException {
        final RestVdbPermission[] permissions = dataRole.getPermissions();
        
        if ( permissions.length != 0 ) {
            out.name( RestVdbDataRole.PERMISSIONS_LABEL );
            out.beginArray();
            
            for ( final RestVdbPermission permission : permissions ) {
                BUILDER.getAdapter( RestVdbPermission.class ).write( out, permission );
            }
            
            out.endArray();
        }
    }
    
}
