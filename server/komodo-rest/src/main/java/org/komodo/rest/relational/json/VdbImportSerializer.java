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
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbImport}s.
 */
public final class VdbImportSerializer extends KomodoRestEntitySerializer< RestVdbImport > {

    private boolean isComplete( final RestVdbImport vdbImport ) {
        return !StringUtils.isBlank( vdbImport.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVdbImport read( final JsonReader in ) throws IOException {
        final RestVdbImport vdbImport = new RestVdbImport();

        beginRead( in );

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.ID:
                    final String vdbName = in.nextString();
                    vdbImport.setName( vdbName );
                    break;
                case RelationalJsonConstants.IMPORT_DATA_POLICIES:
                    final boolean importDataPolicies = in.nextBoolean();
                    vdbImport.setImportDataPolicies( importDataPolicies );
                    break;
                case JsonConstants.LINKS:
                    readLinks( in, vdbImport );
                    break;
                case JsonConstants.PROPERTIES:
                    readProperties( in, vdbImport );
                    break;
                case RelationalJsonConstants.VERSION:
                    final int version = in.nextInt();
                    vdbImport.setVersion( version );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        if ( !isComplete( vdbImport ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbImport.class.getSimpleName() ) );
        }

        endRead( in );
        return vdbImport;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.KomodoRestEntitySerializer#write(com.google.gson.stream.JsonWriter,
     *      org.komodo.rest.KomodoRestEntity)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVdbImport value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbImport.class.getSimpleName() ) );
        }

        beginWrite( out );

        // id
        out.name( JsonConstants.ID );
        out.value( value.getName() );

        // version
        out.name( RelationalJsonConstants.VERSION );
        out.value( value.getVersion() );

        // importDataPolicies
        out.name( RelationalJsonConstants.IMPORT_DATA_POLICIES );
        out.value( value.isImportDataPolicies() );

        writeProperties( out, value );
        writeLinks( out, value );
        endWrite( out );
    }

}
