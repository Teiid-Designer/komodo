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
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_DATA_ROLES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORTS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORT_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODELS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODEL_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATORS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATOR_ERROR;
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
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoRestEntity.ResourceNotFound;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT)
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
    private static final KLog LOGGER = KLog.getLogger( );

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

        if ( StringUtils.isBlank( restVdb.getId() ) ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
        }

        // if name parameter is different than JSON name then do a rename if it exists
        final String vdbNameJson = restVdb.getId();
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

            final RestVdb entity = RestVdb.build(vdb, false, uriInfo.getBaseUri(), uow);
            LOGGER.debug("addOrUpdateVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
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
     * @return about information of this service
     */
    @GET
    @Path(V1Constants.ABOUT)
    public Response about() {
        String msg = "The Rest service is up and running"; //$NON-NLS-1$
        return Response.status(200).entity(msg).build();
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

        if ( StringUtils.isBlank( restVdb.getId() ) ) {
            throw new KomodoRestException( Messages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
        }

        UnitOfWork uow = null;
        final String vdbName = restVdb.getId();

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

    private Response commitNoVdbFound(UnitOfWork uow, List<MediaType> mediaTypes, String vdbName) throws Exception {
        LOGGER.debug( "getVdb:VDB '{0}' was not found", vdbName ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( vdbName, Messages.getString( GET_OPERATION_NAME ) ) );
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
                final Response response = commit( uow, headers.getAcceptableMediaTypes(), KomodoRestEntity.NO_CONTENT );

                return response;
            }

            LOGGER.debug( "deleteVdb:VDB '{0}' was not found to delete", vdbName ); //$NON-NLS-1$
            return commit( uow, headers.getAcceptableMediaTypes(), new ResourceNotFound( vdbName, Messages.getString( DELETE_OPERATION_NAME ) ) );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_VDBS_ERROR, vdbName ), e );
        }
    }

    private Response doAddVdb( final UnitOfWork uow,
                               final URI baseUri,
                               final RestVdb restVdb ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restVdb != null );

        final String vdbName = restVdb.getId();
        String extPath = Messages.getString( NO_VALUE );

        //TODO
//
//        try {
//            // see if there is a external file path set
//            if ( !StringUtils.isBlank( restVdb.getOriginalFilePath() ) ) {
//                extPath = restVdb.getOriginalFilePath();
//            }
//
//            final Vdb vdb = this.wsMgr.createVdb( uow, null, vdbName, extPath );
//            LOGGER.debug( "doAddVdb:VDB '{0}' was created", vdbName ); //$NON-NLS-1$
//
//            // TODO parse the JSON input to set VDB properties and children
//            if ( !StringUtils.isBlank( restVdb.getDescription() ) ) {
//                vdb.setDescription( uow, restVdb.getDescription() );
//            }
//
//            final RestVdb entity = buildVdbEntity( vdb, baseUri, uow );
//            final Response response = commit( uow, entity );
//            return response;
//        } catch ( final Exception e ) {
//            if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
//                uow.rollback();
//            }
//
//            if ( e instanceof KomodoRestException ) {
//                throw ( KomodoRestException )e;
//            }
//
//            throw new KomodoRestException( Messages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
//        }

        return null;
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
    @Path( V1Constants.VDBS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    public Response getVdbs( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo ) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
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
                final Response response = commit( uow, mediaTypes, NO_CONTENT );
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

            final List< RestVdb > entities = new ArrayList< >();
            int i = 0;

            for ( final Vdb vdb : vdbs ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        entities.add(RestVdb.build(vdb, false, uriInfo.getBaseUri(), uow));
                        LOGGER.debug("getVdbs:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
                    } else {
                        break;
                    }
                }

                ++i;
            }

            // create response
            return commit( uow, mediaTypes, entities );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_VDBS_ERROR, errorMsg ), e );
        }
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
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    public Response getVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getVdb", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );
            final RestVdb restVdb = RestVdb.build(vdb, mediaTypes.contains(MediaType.APPLICATION_XML_TYPE), uriInfo.getBaseUri(), uow);
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$

            LOGGER.debug( "getVdb:VDB '{0}' was found", vdbName ); //$NON-NLS-1$
            return commit( uow, mediaTypes, restVdb );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_VDB_ERROR, vdbName, errorMsg ), e );
        }
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
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getModels( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getModels", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            Model[] models = vdb.getModels(uow);
            if (models == null || models.length == 0) {
                LOGGER.debug("getModels:No models found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH + V1Constants.MODELS_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            List<RestVdbModel> restModels = new ArrayList<>(models.length);
            for (Model model : models) {
                restModels.add(RestVdbModel.build(model, vdb, uriInfo.getBaseUri(), uow));
                LOGGER.debug("getModels:Model from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restModels );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_MODELS_ERROR, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the name of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB model (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getModel( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getModel", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getModel:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                   V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                   modelName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            Model model = this.wsMgr.resolve( uow, kModel, Model.class );

            RestVdbModel restModel = RestVdbModel.build(model, vdb, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getModel:Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restModel);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_MODEL_ERROR, modelName, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translators (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getTranslators( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getTranslators", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null || translators.length == 0) {
                LOGGER.debug("getTranslators:No translators found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH + V1Constants.TRANSLATORS_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            List<RestVdbTranslator> restTranslators = new ArrayList<>(translators.length);
            for (Translator translator : translators) {
                restTranslators.add(RestVdbTranslator.build(translator, vdb, uriInfo.getBaseUri(), uow));
                LOGGER.debug("getTranslators:Translator from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restTranslators );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_TRANSLATORS_ERROR, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param translatorName
     *        the name of the translator being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translator (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATOR_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getTranslator( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "translatorName" ) String translatorName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getTranslator", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getTranslator:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null || translators.length == 0) {
                LOGGER.debug("getTranslators:No translators found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH +
                                                     V1Constants.TRANSLATORS_SEGMENT + FORWARD_SLASH +
                                                     translatorName;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Translator translator = null;
            for (Translator tr : translators) {
                if (translatorName.equals(tr.getName(uow))) {
                    translator = tr;
                    break;
                }
            }

            // make sure source exists
            if (translator == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.TRANSLATORS_SEGMENT + FORWARD_SLASH +
                                                   translatorName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbTranslator restTranslator = RestVdbTranslator.build(translator, vdb, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getTranslator:Translator '{0}' from VDB '{1}' entity was constructed", translatorName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restTranslator);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_TRANSLATOR_ERROR, translatorName, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getImports( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getImports", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null || imports.length == 0) {
                LOGGER.debug("getImports:No imports found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH + V1Constants.IMPORTS_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            List<RestVdbImport> restImports = new ArrayList<>(imports.length);
            for (VdbImport vdbImport : imports) {
                restImports.add(RestVdbImport.build(vdbImport, vdb, uriInfo.getBaseUri(), uow));
                LOGGER.debug("getImports:Import from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restImports);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_IMPORTS_ERROR, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param importName
     *        the name of the import being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORT_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getImport( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "importName" ) String importName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getImport", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getImport:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null || imports.length == 0) {
                LOGGER.debug("getImport:No import found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH +
                                                     V1Constants.TRANSLATORS_SEGMENT + FORWARD_SLASH +
                                                     importName;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            VdbImport vdbImport = null;
            for (VdbImport imp : imports) {
                if (importName.equals(imp.getName(uow))) {
                    vdbImport = imp;
                    break;
                }
            }

            // make sure source exists
            if (vdbImport == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.IMPORTS_SEGMENT + FORWARD_SLASH +
                                                   importName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbImport restImport = RestVdbImport.build(vdbImport, vdb, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getImport:Import '{0}' from VDB '{1}' entity was constructed", importName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restImport);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_IMPORT_ERROR, importName, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getDataRoles( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getDataRoles", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            DataRole[] dataRoles = vdb.getDataRoles(uow);
            if (dataRoles == null || dataRoles.length == 0) {
                LOGGER.debug("getDataRoles:No data roles found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH + V1Constants.DATA_ROLES_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            List<RestVdbDataRole> restImports = new ArrayList<>(dataRoles.length);
            for (DataRole dataRole : dataRoles) {
                restImports.add(RestVdbDataRole.build(dataRole, vdb, uriInfo.getBaseUri(), uow));
                LOGGER.debug("getDataRoles:Data role from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restImports);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_DATA_ROLES_ERROR, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param dataRoleName
     *        the name of the import being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getDataRole( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "dataRoleName" ) String dataRoleName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getDataRole", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getDataRole:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            DataRole[] dataRoles = vdb.getDataRoles(uow);
            if (dataRoles == null || dataRoles.length == 0) {
                LOGGER.debug("getDataRole:No data role found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH +
                                                     V1Constants.TRANSLATORS_SEGMENT + FORWARD_SLASH +
                                                     dataRoleName;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            DataRole dataRole = null;
            for (DataRole drole : dataRoles) {
                if (dataRoleName.equals(drole.getName(uow))) {
                    dataRole = drole;
                    break;
                }
            }

            // make sure source exists
            if (dataRole == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.IMPORTS_SEGMENT + FORWARD_SLASH +
                                                   dataRoleName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbDataRole restDataRole = RestVdbDataRole.build(dataRole, vdb, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getDataRole:data role '{0}' from VDB '{1}' entity was constructed", dataRoleName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restDataRole);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_IMPORT_ERROR, dataRoleName, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the name of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getSources( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getSources", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getSources:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            // make sure model exists
            if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                   modelName + FORWARD_SLASH +
                                                   V1Constants.SOURCES_SEGMENT,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            Model model = this.wsMgr.resolve( uow, kModel, Model.class );

            ModelSource[] sources = model.getSources(uow);
            if (sources == null || sources.length == 0) {
                LOGGER.debug("getSources:No sources found for model '{0}' in vdb '{1}'", modelName, vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH +
                                                     V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                     modelName + FORWARD_SLASH +
                                                     V1Constants.SOURCES_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            List<RestVdbModelSource> restSources = new ArrayList<>(sources.length);
            for (ModelSource source : sources) {
                restSources.add(RestVdbModelSource.build(source, model, vdb, uriInfo.getBaseUri(), uow));
                LOGGER.debug("getSources:Source from Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restSources );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_SOURCES_ERROR, modelName, vdbName, errorMsg ), e );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the name of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the name of the source being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    public Response getSource( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            final @PathParam( "vdbName" ) String vdbName,
                            final @PathParam( "modelName" ) String modelName,
                            final @PathParam( "sourceName" ) String sourceName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getSource", true ); //$NON-NLS-1$

            // make sure VDB exists
            if (! this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            }

            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );

            LOGGER.debug( "getSource:VDB '{0}' was found", vdbName ); //$NON-NLS-1$

            // make sure model exists
            if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                   modelName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            Model model = this.wsMgr.resolve( uow, kModel, Model.class );

            ModelSource[] sources = model.getSources(uow);
            if (sources == null || sources.length == 0) {
                LOGGER.debug("getSource:No sources found for model '{0}' in vdb '{1}'", modelName, vdbName); //$NON-NLS-1$
                String resourceName = vdbName + FORWARD_SLASH +
                                                     V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                     modelName + FORWARD_SLASH +
                                                     V1Constants.SOURCES_SEGMENT;
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            //
            // Using model.getSources rather than model.getChild bypasses the VdbModelSources
            // collection object that hangs underneath the model.
            //
            ModelSource source = null;
            for (ModelSource src : sources) {
                if (sourceName.equals(src.getName(uow))) {
                    source = src;
                    break;
                }
            }

            // make sure source exists
            if (source == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(
                                                   vdbName + FORWARD_SLASH +
                                                    V1Constants.MODELS_SEGMENT + FORWARD_SLASH +
                                                   modelName + FORWARD_SLASH +
                                                   V1Constants.SOURCES_SEGMENT + FORWARD_SLASH +
                                                   sourceName,
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbModelSource restSource = RestVdbModelSource.build(source, model, vdb, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getSource:Source '{0}' from Model '{1}' from VDB '{2}' entity was constructed", //$NON-NLS-1$
                         sourceName, modelName, vdbName);

            return commit( uow, mediaTypes, restSource);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_SOURCE_ERROR, sourceName, modelName, vdbName, errorMsg ), e );
        }
    }
}
