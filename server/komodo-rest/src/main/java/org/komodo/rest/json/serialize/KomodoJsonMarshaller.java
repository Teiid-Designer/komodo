/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import org.komodo.rest.json.KomodoRestEntity;
import org.komodo.rest.json.RestLink;
import org.komodo.rest.json.RestVdb;
import org.komodo.rest.json.RestVdbDataRole;
import org.komodo.rest.json.RestVdbDescriptor;
import org.komodo.rest.json.RestVdbDirectory;
import org.komodo.rest.json.RestVdbEntry;
import org.komodo.rest.json.RestVdbImport;
import org.komodo.rest.json.RestVdbPermission;
import org.komodo.rest.json.RestVdbTranslator;
import org.komodo.utils.ArgCheck;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * A JSON serializer and deserializer for {@link KomodoRestEntity Komodo REST objects}.
 */
public final class KomodoJsonMarshaller {

    private static final Logger LOGGER = LoggerFactory.getLogger( KomodoJsonMarshaller.class );

    /**
     * The shared JSON serialier/deserializer for {@link KomodoRestEntity} objects.
     */
    protected static final Gson BUILDER;

    protected static final Gson PRETTY_BUILDER;

    static {
        final GsonBuilder temp = new GsonBuilder().registerTypeAdapter( RestLink.class, new LinkSerializer() )
                                                  .registerTypeAdapter( RestVdb.class, new VdbSerializer() )
                                                  .registerTypeAdapter( RestVdbDataRole.class, new VdbDataRoleSerializer() )
                                                  .registerTypeAdapter( RestVdbDescriptor.class, new VdbDescriptorSerializer() )
                                                  .registerTypeAdapter( RestVdbDirectory.class, new VdbDirectorySerializer() )
                                                  .registerTypeAdapter( RestVdbEntry.class, new VdbEntrySerializer() )
                                                  .registerTypeAdapter( RestVdbImport.class, new VdbImportSerializer() )
                                                  .registerTypeAdapter( RestVdbPermission.class, new VdbPermissionSerializer() )
                                                  .registerTypeAdapter( RestVdbTranslator.class, new VdbTranslatorSerializer() );
        BUILDER = temp.create();
        PRETTY_BUILDER = temp.setPrettyPrinting().create();
    }

    /**
     * Outputs a non-pretty printed JSON representation.
     *
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @return the JSON representation (never empty)
     */
    public static String marshall( final KomodoRestEntity entity ) {
        return marshall( entity, false );
    }

    /**
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @param prettyPrint
     *        <code>true</code> if JSON output should be pretty printed
     * @return the JSON representation (never empty)
     */
    public static String marshall( final KomodoRestEntity entity,
                                   final boolean prettyPrint ) {
        ArgCheck.isNotNull( entity, "entity" ); //$NON-NLS-1$

        String json = null;

        if ( prettyPrint ) {
            json = PRETTY_BUILDER.toJson( entity );
        } else {
            json = BUILDER.toJson( entity );
        }

        LOGGER.debug( "marshall: {0}", json ); //$NON-NLS-1$
        return json;
    }

    /**
     * @param <T>
     *        the {@link KomodoRestEntity} type of the output
     * @param json
     *        the JSON representation being converted to a {@link KomodoRestEntity} (cannot be empty)
     * @param entityClass
     *        the type of {@link KomodoRestEntity} the JSON will be converted to (cannot be <code>null</code>)
     * @return the {@link KomodoRestEntity} (never <code>null</code>)
     */
    public static < T extends KomodoRestEntity > T unmarshall( final String json,
                                                               final Class< T > entityClass ) {
        final T entity = BUILDER.fromJson( json, entityClass );
        LOGGER.debug( "unmarshall: class = {0}, entity = {1}", entityClass, entity ); //$NON-NLS-1$
        return entity;
    }

    /**
     * Don't allow construction outside of this class.
     */
    private KomodoJsonMarshaller() {
        // nothing to do
    }

}
