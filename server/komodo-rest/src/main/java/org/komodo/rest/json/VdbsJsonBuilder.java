/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A VDBs JSON document builder.
 *
 * <pre>
 * {
 *     vdbs: {
 *         {
 *             "id" : "MyVdb",
 *             "description" : "my vdb description goes here",
 *             "links" : [
 *                 { "rel" : "self", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb" },
 *                 { "rel" : "xml", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/MyVdb/vdb.xml" }
 *             ]
 *         },
 *         {
 *             "id" : "YourVdb",
 *             "description" : "your vdb description goes here",
 *             "links" : [
 *                 { "rel" : "self", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/YourVdb" },
 *                 { "rel" : "xml", "href" : "http://localhost:8080/komodo/v1/workspace/vdbs/YourVdb/vdb.xml" }
 *              ]
 *         }
 *     }
 * }
 * </pre>
 */
public final class VdbsJsonBuilder {

    /**
     * The shared instance of a VDBs JSON document builder.
     */
    public static final VdbsJsonBuilder BUILDER = new VdbsJsonBuilder();

    /**
     * Don't allow construction outside of this class.
     */
    private VdbsJsonBuilder() {
        // nothing to do
    }

    /**
     * @param vdbs
     *        the VDBs whose JSON representations are being requested (can be <code>null</code> or empty)
     * @param uow
     *        the transaction (cannot be <code>null</code> or already have been committed)
     * @param context
     *        the builder context (cannot be <code>null</code>)
     * @return the VDB descriptors JSON document (never <code>null</code>)
     * @throws KomodoRestException
     *         if an error occurs
     */
    public JsonObject build( final Vdb[] vdbs,
                             final UnitOfWork uow,
                             final JsonBuilderContext context ) throws KomodoRestException {
        final JsonObjectBuilder result = Json.createObjectBuilder();

        if ( ( vdbs != null ) && ( vdbs.length != 0 ) ) {
            final JsonArrayBuilder builder = Json.createArrayBuilder();

            for ( final Vdb vdb : vdbs ) {
                builder.add( VdbDescriptorJsonBuilder.BUILDER.build( vdb, uow, context ) );
            }

            result.add( JsonName.VDBS, builder );
        }

        return result.build();
    }

}
