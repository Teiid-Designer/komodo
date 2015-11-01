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
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbDataRole;
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
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbDataRole read( final JsonReader in ) throws IOException {
        final RestVdbDataRole dataRole = new RestVdbDataRole();

        beginRead( in );

        readBasicProperties(in, dataRole);

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (RestVdbDataRole.NAME_LABEL.equals(name)) {
               dataRole.setName(in.nextString());
            } else if (RestVdbDataRole.ALLOW_CREATE_TEMP_TABLES_LABEL.equals(name)) {
                final boolean allowCreateTempTables = in.nextBoolean();
                dataRole.setAllowCreateTempTables( allowCreateTempTables );
            } else if (RestVdbDataRole.ANY_AUTHENTICATED_LABEL.equals(name)) {
                final boolean anyAuthenticated = in.nextBoolean();
                dataRole.setAnyAuthenticated( anyAuthenticated );
            } else if (RestVdbDataRole.DESCRIPTION_LABEL.equals(name)) {
                final String description = in.nextString();
                dataRole.setDescription( description );
            } else if (RestVdbDataRole.GRANT_ALL_LABEL.equals(name)) {
                final boolean grantAll = in.nextBoolean();
                dataRole.setGrantAll( grantAll );
            } else if (JsonConstants.LINKS.equals(name)) {
                readLinks( in, dataRole );
            } else if (RestVdbDataRole.MAPPED_ROLES_LABEL.equals(name)) {
                final String[] mappedRoles = BUILDER.fromJson( in, String[].class );
                dataRole.setMappedRoles( mappedRoles );
            }
            else
                throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
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
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out, final RestVdbDataRole dataRole) throws IOException {
        if (!isComplete(dataRole)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, RestVdbDataRole.class.getSimpleName()));
        }

        beginWrite(out);

        writeBasicProperties(out, dataRole);

        // name
        out.name(RestVdbDataRole.NAME_LABEL);
        out.value(dataRole.getName());

        // description
        if (!StringUtils.isBlank(dataRole.getDescription())) {
            out.name(RelationalJsonConstants.DESCRIPTION);
            out.value(dataRole.getDescription());
        }

        // create temp tables
        out.name(RestVdbDataRole.ALLOW_CREATE_TEMP_TABLES_LABEL);
        out.value(dataRole.isAllowCreateTempTables());

        // any authenticated
        out.name(RestVdbDataRole.ANY_AUTHENTICATED_LABEL);
        out.value(dataRole.isAnyAuthenticated());

        // grant all
        out.name(RestVdbDataRole.GRANT_ALL_LABEL);
        out.value(dataRole.isGrantAll());

        // mapped roles
        out.name(RestVdbDataRole.MAPPED_ROLES_LABEL);

        out.beginArray();
        for (final String roleName : dataRole.getMappedRoles()) {
            out.value(roleName);
        }
        out.endArray();

        writeLinks(out, dataRole);
        endWrite(out);
    }

}
