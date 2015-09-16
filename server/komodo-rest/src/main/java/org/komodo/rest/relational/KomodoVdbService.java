/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import static org.komodo.rest.KomodoRestEntity.NO_CONTENT;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_WORKSPACE_MGR_ERROR;
import static org.komodo.rest.Messages.General.DELETE_OPERATION_NAME;
import static org.komodo.rest.Messages.General.GET_OPERATION_NAME;
import static org.komodo.rest.Messages.General.NO_VALUE;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDBS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_MISSING_VDB;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_MISSING_VDB_NAME;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_VDB_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_VDB_NAME_ERROR;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.KomodoService;
import org.komodo.rest.Messages;
import org.komodo.rest.RestLink;
import org.komodo.rest.KomodoRestEntity.ResourceNotFound;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path( KomodoRestV1Application.V1Constants.VDBS_RELATIVE_PATH )
public final class KomodoVdbService extends KomodoService {

    /**
     * Query parameters used by the service methods.
     */
    public interface QueryParam {

        /**
         * A regex expression used when searching. If not present, all objects are returned.
         */
        String PATTERN = "pattern"; //$NON-NLS-1$

        /**
         * The number of objects to return. If not present, all objects are returned.
         */
        String SIZE = "size"; //$NON-NLS-1$

        /**
         * The index of the first object to return. Defaults to zero.
         */
        String START = "start"; //$NON-NLS-1$

    }

    private static final int ALL_AVAILABLE = -1;
    private static final Logger LOGGER = LoggerFactory.getLogger( KomodoVdbService.class );

    private final WorkspaceManager wsMgr;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws ServerErrorException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoVdbService( final KEngine engine ) throws ServerErrorException {
        super( engine );

        try {
            this.wsMgr = WorkspaceManager.getInstance( this.repo );
        } catch ( final Exception e ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_WORKSPACE_MGR_ERROR ),
                                            Status.INTERNAL_SERVER_ERROR );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @param vdbJson
     *        the VDB JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the VDB
     */
    @PUT
    @Path( "{vdbName}" )
    @Consumes( MediaType.APPLICATION_JSON )
    @Produces( MediaType.APPLICATION_JSON )
    public Response addOrUpdateVdb( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    final @PathParam( "vdbName" ) String vdbName,
                                    final String vdbJson) throws KomodoRestException {
        if ( vdbJson == null ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB ) );
        }

        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );

        if ( StringUtils.isBlank( restVdb.getName() ) ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
        }

        // if name parameter is different than JSON name then do a rename if it exists
        final String vdbNameJson = restVdb.getName();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "addOrUpdateVdb", false ); //$NON-NLS-1$
            final boolean exists = this.wsMgr.hasChild( uow, vdbName );
            final boolean namesMatch = vdbName.equals( vdbNameJson );

            if ( !exists && !namesMatch ) {
                throw new KomodoRestException( Messages.getString( VDB_SERVICE_VDB_NAME_ERROR, vdbName, vdbNameJson ) );
            }

            // create new VDB
            if ( !exists ) {
                return doAddVdb( uow, uriInfo.getBaseUri(), restVdb );
            }

            // must be an update
            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            // TODO parse the JSON input to set VDB properties and children

            // rename if names did not match
            if ( !namesMatch ) {
                vdb.rename( uow, vdbNameJson );
            }

            final RestVdbDescriptor descriptor = buildVdbDescriptorEntity( vdb, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, descriptor );
            return response;
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbJson
     *        the VDB JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the VDB
     */
    @POST
    @Consumes( MediaType.APPLICATION_JSON )
    @Produces( MediaType.APPLICATION_JSON )
    public Response addVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final String vdbJson ) throws KomodoRestException {
        if ( vdbJson == null ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB ) );
        }

        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );

        if ( StringUtils.isBlank( restVdb.getName() ) ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
        }

        UnitOfWork uow = null;
        final String vdbName = restVdb.getName();

        try {
            uow = createTransaction( "createVdb", false ); //$NON-NLS-1$

            if ( this.wsMgr.hasChild( uow, vdbName ) ) {
                throw new KomodoRestException( Messages.getString( VDB_SERVICE_VDB_EXISTS ) );
            }

            return doAddVdb( uow, uriInfo.getBaseUri(), restVdb );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
        }
    }

    private RestVdbDescriptor buildVdbDescriptorEntity( final Vdb vdb,
                                                        final URI baseUri,
                                                        final UnitOfWork uow ) throws KException {
        final String vdbName = vdb.getName( uow );
        final RestVdbDescriptor result = new RestVdbDescriptor( vdbName, baseUri, getLinkTypesToGenerate( vdb ) );
        result.setDescription( vdb.getDescription( uow ) );

        LOGGER.debug( "buildVdbDescriptorEntity:VDB '{0}' descriptor entity was constructed", vdbName ); //$NON-NLS-1$
        return result;
    }

    private RestVdb buildVdbEntity( final Vdb vdb,
                                    final URI baseUri,
                                    final UnitOfWork uow ) throws KException {
        final String vdbName = vdb.getName( uow );
        final RestVdb entity = new RestVdb( vdbName );
        entity.setDescription( vdb.getDescription( uow ) );
        entity.setOriginalFilePath( vdb.getOriginalFilePath( uow ) );
        //
        //        { // data roles
        //            final DataRole[] dataRoles = vdb.getDataRoles( uow );
        //            entity.setDataRoles( newDataRoles );
        //        }
        //
        //        { // entries
        //            final Entry[] entries = vdb.getEntries( uow );
        //            entity.setEntries( newEntries );
        //        }
        //
        //        { // VDB imports
        //            final VdbImport[] imports = vdb.getImports( uow );
        //            entity.setImports( newImports );
        //        }
        //
        //        { // translators
        //            final Translator[] translators = vdb.getTranslators( uow );
        //            entity.setTranslators( newTranslators );
        //        }
        //
        //        entity.setLinks( newLinks );
        //        entity.setProperties( newProperties );

        LOGGER.debug( "buildVdbEntity:VDB '{0}' entity was constructed", vdbName ); //$NON-NLS-1$
        return entity;
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being deleted (cannot be empty)
     * @return the JSON representation of the VDB that was deleted (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem deleting the specified workspace VDB or constructing the JSON representation
     */
    @DELETE
    @Path( "{vdbName}" )
    @Produces( MediaType.APPLICATION_JSON )
    public Response deleteVdb( final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "deleteVdb", false ); //$NON-NLS-1$

            // make sure VDB exists
            if ( this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
                final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );
                vdb.remove( uow );

                LOGGER.debug( "deleteVdb:VDB '{0}' was deleted", vdbName ); //$NON-NLS-1$
                final Response response = commit( uow, KomodoRestEntity.NO_CONTENT );

                return response;
            }

            LOGGER.debug( "deleteVdb:VDB '{0}' was not found to delete", vdbName ); //$NON-NLS-1$
            return commit( uow, new ResourceNotFound( vdbName, Messages.getString( DELETE_OPERATION_NAME ) ) );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_GET_VDB_ERROR, vdbName ), e );
        }
    }

    private Response doAddVdb( final UnitOfWork uow,
                               final URI baseUri,
                               final RestVdb restVdb ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restVdb != null );

        final String vdbName = restVdb.getName();
        String extPath = Messages.getString( NO_VALUE );

        try {
            // see if there is a external file path set
            if ( !StringUtils.isBlank( restVdb.getOriginalFilePath() ) ) {
                extPath = restVdb.getOriginalFilePath();
            }

            final Vdb vdb = this.wsMgr.createVdb( uow, null, vdbName, extPath );
            LOGGER.debug( "doAddVdb:VDB '{0}' was created", vdbName ); //$NON-NLS-1$

            // TODO parse the JSON input to set VDB properties and children
            if ( !StringUtils.isBlank( restVdb.getDescription() ) ) {
                vdb.setDescription( uow, restVdb.getDescription() );
            }

            final RestVdbDescriptor descriptor = buildVdbDescriptorEntity( vdb, baseUri, uow );
            final Response response = commit( uow, descriptor );
            return response;
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
        }
    }

    private LinkType[] getLinkTypesToGenerate( final Vdb vdb ) {
        // TODO figure out which ones???
        return new LinkType[] { LinkType.SELF, LinkType.PARENT, LinkType.DELETE, LinkType.MANIFEST };
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( "{vdbName}" )
    @Produces( MediaType.APPLICATION_JSON )
    public Response getVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getVdb", true ); //$NON-NLS-1$

            // make sure VDB exists
            if ( this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
                final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );
                final RestVdb restVdb = buildVdbEntity( vdb, uriInfo.getBaseUri(), uow );

                LOGGER.debug( "getVdb:VDB '{0}' was found", vdbName ); //$NON-NLS-1$
                final Response response = commit( uow, restVdb );
                return response;
            }

            LOGGER.debug( "getVdb:VDB '{0}' was not found", vdbName ); //$NON-NLS-1$
            return commit( uow, new ResourceNotFound( vdbName, Messages.getString( GET_OPERATION_NAME ) ) );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_GET_VDB_ERROR, vdbName ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the VDBs in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path( "/" )
    @Produces( MediaType.APPLICATION_JSON )
    public Response getVdbs( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo ) throws KomodoRestException {
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParam.PATTERN );

            // find VDBs
            uow = createTransaction( "getVdbs", true ); //$NON-NLS-1$
            Vdb[] vdbs = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                vdbs = this.wsMgr.findVdbs( uow );
                LOGGER.debug( "getVdbs:found '{0}' VDBs", vdbs.length ); //$NON-NLS-1$
            } else {
                final String[] vdbPaths = this.wsMgr.findByType( uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, null, searchPattern );

                if ( vdbPaths.length == 0 ) {
                    vdbs = Vdb.NO_VDBS;
                } else {
                    vdbs = new Vdb[ vdbPaths.length ];
                    int i = 0;

                    for ( final String path : vdbPaths ) {
                        vdbs[ i++ ] = this.wsMgr.resolve( uow, new ObjectImpl( this.wsMgr.getRepository(), path, 0 ), Vdb.class );
                    }

                    LOGGER.debug( "getVdbs:found '{0}' VDBs using pattern '{1}'", vdbs.length, searchPattern ); //$NON-NLS-1$
                }
            }

            if ( vdbs.length == 0 ) {
                final Response response = commit( uow, NO_CONTENT );
                return response;
            }

            int start = 0;

            { // start query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParam.START );

                if ( qparam != null ) {

                    try {
                        start = Integer.parseInt( qparam );

                        if ( start < 0 ) {
                            start = 0;
                        }
                    } catch ( final Exception e ) {
                        start = 0;
                    }
                }
            }

            int size = ALL_AVAILABLE;

            { // size query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParam.SIZE );

                if ( qparam != null ) {

                    try {
                        size = Integer.parseInt( qparam );

                        if ( size <= 0 ) {
                            size = ALL_AVAILABLE;
                        }
                    } catch ( final Exception e ) {
                        size = ALL_AVAILABLE;
                    }
                }
            }

            final List< RestVdbDescriptor > descriptors = new ArrayList< >();
            int i = 0;

            for ( final Vdb vdb : vdbs ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( descriptors.size() < size ) ) {
                        descriptors.add( buildVdbDescriptorEntity( vdb, uriInfo.getBaseUri(), uow ) );
                    } else {
                        break;
                    }
                }

                ++i;
            }

            // create response
            final RestVdbDirectory vdbDir = new RestVdbDirectory( descriptors.toArray( new RestVdbDescriptor[ descriptors.size() ] ) );
            final Response response = commit( uow, vdbDir );
            return response;
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( Messages.getString( VDB_SERVICE_GET_VDBS_ERROR ), e );
        }
    }

}
