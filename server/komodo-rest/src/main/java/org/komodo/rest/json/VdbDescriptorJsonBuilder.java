/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.komodo.rest.Messages.Error.VDB_DESCRIPTOR_BUILDER_ERROR;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.Messages;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A VDB descriptor JSON document builder.
 *
 * <pre>
 * {
 *     "id" : "MyVdb",
 *     "description" : "vdb description goes here",
 *     "links" : [
 *         { "rel" : "self", "href" : "http://.../workspace/vdbs/MyVdb" },
 *         { "rel" : "xml", "href" : "http://.../workspace/vdbs/MyVdb/vdb.xml" }
 *     ]
 * }
 * </pre>
 */
public final class VdbDescriptorJsonBuilder {

    /**
     * The shared instance of a VDB descriptor JSON document builder.
     */
    public static final VdbDescriptorJsonBuilder BUILDER = new VdbDescriptorJsonBuilder();

    /**
     * Don't allow construction outside of this class.
     */
    private VdbDescriptorJsonBuilder() {
        // nothing to do
    }

    /**
     * @param vdb
     *        the VDB whose JSON descriptor is being requested (cannot be <code>null</code>)
     * @param uow
     *        the transaction (cannot be <code>null</code> or already have been committed)
     * @param context
     *        the builder context (cannot be <code>null</code>)
     * @return the VDB descriptor JSON document (never <code>null</code>)
     * @throws KomodoRestException
     *         if an error occurs
     */
    public JsonObject build( final Vdb vdb,
                             final UnitOfWork uow,
                             final JsonBuilderContext context ) throws KomodoRestException {
        final JsonObjectBuilder result = Json.createObjectBuilder();
        String vdbName = null;

        try {
            vdbName = vdb.getName( uow );
            result.add( JsonName.ID, vdbName );
            result.add( JsonName.DESCRIPTION, vdb.getDescription( uow ) );
        } catch ( final Exception e ) {
            throw new KomodoRestException( Messages.getString( VDB_DESCRIPTOR_BUILDER_ERROR, uow.getName() ), e );
        }

        { // create links
            final JsonArrayBuilder linksBuilder = Json.createArrayBuilder();

            { // self link
                final JsonObjectBuilder selfLinkBuilder = Json.createObjectBuilder();
                selfLinkBuilder.add( JsonName.REL, JsonName.SELF );
                selfLinkBuilder.add( JsonName.HREF, String.format( KomodoJsonBuilder.VDB_VIEW_LINK, context.rootUri, vdbName ) );
                linksBuilder.add( selfLinkBuilder );
            }

            { // view link
                final JsonObjectBuilder selfLinkBuilder = Json.createObjectBuilder();
                selfLinkBuilder.add( JsonName.REL, JsonName.VIEW );
                selfLinkBuilder.add( JsonName.HREF, String.format( KomodoJsonBuilder.VDB_VIEW_LINK, context.rootUri, vdbName ) );
                linksBuilder.add( selfLinkBuilder );
            }

            // add links to result
            result.add( JsonName.LINKS, linksBuilder );
        }

        return result.build();
    }

}
