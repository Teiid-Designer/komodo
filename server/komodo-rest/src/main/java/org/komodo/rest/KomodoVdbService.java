/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.NO_VALUE;
import static org.komodo.rest.Messages.Error.VDB_SERVICE_ADD_VDB_ERROR;
import static org.komodo.rest.Messages.Error.VDB_SERVICE_GET_VDBS_ERROR;
import javax.json.JsonObject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import org.komodo.core.KEngine;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.json.JsonBuilderContext;
import org.komodo.rest.json.VdbDescriptorJsonBuilder;
import org.komodo.rest.json.VdbsJsonBuilder;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path( "workspace/vdbs" )
public final class KomodoVdbService extends KomodoService {

    protected final JsonBuilderContext context;
    protected final WorkspaceManager wsMgr;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws KException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoVdbService( final KEngine engine ) throws KException {
        super( engine );
        this.context = new JsonBuilderContext();
        this.context.rootUri = "http://localhost:8080/"; //$NON-NLS-1$  // TODO should not be hardcoded
        this.wsMgr = WorkspaceManager.getInstance( this.repo );
    }

    /**
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param parentPath
     *        the parent path where the VDB should be created (can be <code>null</code> if workspace root is the parent)
     * @param externalFilePath
     *        the external file path of the VDB (can be empty)
     * @return a JSON representation of the new VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the VDB
     */
    @Consumes( "application/json" )
    @Path( "{vdbName}" )
    @POST
    @Produces( "application/json" )
    public Response addVdb( final @PathParam( "vdbName" ) String vdbName,
                            final String parentPath,
                            final String externalFilePath) throws KomodoRestException {
        try {
            final UnitOfWork uow = createTransaction( "addVdb", false ); //$NON-NLS-1$
            final String extPath = ( StringUtils.isBlank( externalFilePath ) ? Messages.getString( NO_VALUE ) : externalFilePath );
            final Vdb vdb = this.wsMgr.createVdb( uow, null, vdbName, extPath );

            final JsonObject json = VdbDescriptorJsonBuilder.BUILDER.build( vdb, uow, this.context );
            final Response response = commit( uow, json );
            return response;
        } catch ( final Exception e ) {
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            final String path = ( StringUtils.isBlank( parentPath ) ? Messages.getString( NO_VALUE ) : parentPath );
            final String filePath = ( StringUtils.isBlank( externalFilePath ) ? Messages.getString( NO_VALUE ) : externalFilePath );
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_ADD_VDB_ERROR, vdbName, path, filePath ), e );
        }
    }

    /**
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     */
    @GET
    @Path( "{vdbName}" )
    @Produces( "application/json" )
    public Response getVdb( @PathParam( "vdbName" ) final String vdbName) {
        // TODO implement
        return null;
    }

    /**
     * @return a JSON document representing all the VDBs in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path( "/" )
    @Produces( "application/json" )
    public Response getVdbs() throws KomodoRestException {
        try {
            final UnitOfWork uow = createTransaction( "getVdbs", true ); //$NON-NLS-1$
            final Vdb[] vdbs = this.wsMgr.findVdbs( uow );
            final JsonObject json = VdbsJsonBuilder.BUILDER.build( vdbs, uow, this.context );
            final Response response = commit( uow, json );
            return response;
        } catch ( final Exception e ) {
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_GET_VDBS_ERROR ), e );
        }
    }

}
