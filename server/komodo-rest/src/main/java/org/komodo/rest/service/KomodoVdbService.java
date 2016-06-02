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
package org.komodo.rest.service;

import static org.komodo.rest.Messages.General.GET_OPERATION_NAME;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_CONDITIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_CONDITION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_DATA_ROLES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_DATA_ROLE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORTS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORT_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MASKS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MASK_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODELS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODEL_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_PERMISSIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_PERMISSION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATORS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATOR_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDBS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.Messages;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestBasicEntity.ResourceNotFound;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbCondition;
import org.komodo.rest.relational.RestVdbDataRole;
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.rest.relational.RestVdbMask;
import org.komodo.rest.relational.RestVdbModel;
import org.komodo.rest.relational.RestVdbModelSource;
import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.VDBS_SEGMENT)
@Api(tags = {V1Constants.VDBS_SEGMENT})
public final class KomodoVdbService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws ServerErrorException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoVdbService( final KEngine engine ) throws ServerErrorException {
        super( engine );
    }

//    /**
//     * @param headers
//     *        the request headers (never <code>null</code>)
//     * @param uriInfo
//     *        the request URI information (never <code>null</code>)
//     * @param vdbName
//     *        the VDB name (cannot be empty)
//     * @param vdbJson
//     *        the VDB JSON representation (cannot be <code>null</code>)
//     * @return a JSON representation of the new VDB (never <code>null</code>)
//     * @throws KomodoRestException
//     *         if there is an error creating the VDB
//     */
//    @PUT
//    @Path( "{vdbName}" )
//    @Consumes( MediaType.APPLICATION_JSON )
//    @Produces( MediaType.APPLICATION_JSON )
//    public Response addOrUpdateVdb( final @Context HttpHeaders headers,
//                                    final @Context UriInfo uriInfo,
//                                    final @PathParam( "vdbName" ) String vdbName,
//                                    final String vdbJson) throws KomodoRestException {
//        if ( vdbJson == null ) {
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_MISSING_VDB ) );
//        }
//
//        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );
//
//        if ( StringUtils.isBlank( restVdb.getId() ) ) {
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
//        }
//
//        // if name parameter is different than JSON name then do a rename if it exists
//        final String vdbNameJson = restVdb.getId();
//        UnitOfWork uow = null;
//
//        try {
//            uow = createTransaction( "addOrUpdateVdb", false ); //$NON-NLS-1$
//            final boolean exists = this.wsMgr.hasChild( uow, vdbName );
//            final boolean namesMatch = vdbName.equals( vdbNameJson );
//
//            if ( !exists && !namesMatch ) {
//                throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_VDB_NAME_ERROR, vdbName, vdbNameJson ) );
//            }
//
//            // create new VDB
//            if ( !exists ) {
//                return doAddVdb( uow, uriInfo.getBaseUri(), restVdb );
//            }
//
//            // must be an update
//            final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
//            final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );
//
//            // TODO parse the JSON input to set VDB properties and children
//
//            // rename if names did not match
//            if ( !namesMatch ) {
//                vdb.rename( uow, vdbNameJson );
//            }
//
//            KomodoProperties properties = new KomodoProperties();
//            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
//            final RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
//            LOGGER.debug("addOrUpdateVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
//            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
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
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
//        }
//    }
//
//    /**
//     * @param headers
//     *        the request headers (never <code>null</code>)
//     * @param uriInfo
//     *        the request URI information (never <code>null</code>)
//     * @param vdbJson
//     *        the VDB JSON representation (cannot be <code>null</code>)
//     * @return a JSON representation of the new VDB (never <code>null</code>)
//     * @throws KomodoRestException
//     *         if there is an error creating the VDB
//     */
//    @POST
//    @Consumes( MediaType.APPLICATION_JSON )
//    @Produces( MediaType.APPLICATION_JSON )
//    public Response addVdb( final @Context HttpHeaders headers,
//                            final @Context UriInfo uriInfo,
//                            final String vdbJson ) throws KomodoRestException {
//        if ( vdbJson == null ) {
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_MISSING_VDB ) );
//        }
//
//        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );
//
//        if ( StringUtils.isBlank( restVdb.getId() ) ) {
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_MISSING_VDB_NAME ) );
//        }
//
//        UnitOfWork uow = null;
//        final String vdbName = restVdb.getId();
//
//        try {
//            uow = createTransaction( "createVdb", false ); //$NON-NLS-1$
//
//            if ( this.wsMgr.hasChild( uow, vdbName ) ) {
//                throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_VDB_EXISTS ) );
//            }
//
//            return doAddVdb( uow, uriInfo.getBaseUri(), restVdb );
//        } catch ( final Exception e ) {
//            if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
//                uow.rollback();
//            }
//
//            if ( e instanceof KomodoRestException ) {
//                throw ( KomodoRestException )e;
//            }
//
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
//        }
//    }

    private Model findModel(UnitOfWork uow, List<MediaType> mediaTypes,
                                                String modelName, Vdb vdb) throws KException {
        if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
            return null;
        }

        KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        Model model = this.wsMgr.resolve( uow, kModel, Model.class );
        LOGGER.debug( "Model '{0}' was found", modelName ); //$NON-NLS-1$
        return model;
    }

    private DataRole findDataRole(UnitOfWork uow, List<MediaType> mediaTypes,
                                                      String dataRoleId, Vdb vdb) throws KException {
        DataRole[] dataRoles = vdb.getDataRoles(uow);
        if (dataRoles == null || dataRoles.length == 0) {
            return null;
        }

        DataRole dataRole = null;
        for (DataRole drole : dataRoles) {
            if (dataRoleId.equals(drole.getName(uow))) {
                dataRole = drole;
                break;
            }
        }

        return dataRole;
    }

    private Permission findPermission(UnitOfWork uow, List<MediaType> mediaTypes,
                                                              String permissionId, DataRole dataRole, Vdb vdb) throws KException {
        Permission[] permissions = dataRole.getPermissions(uow);
        if (permissions == null || permissions.length == 0) {
            return null;
        }

        Permission permission = null;
        for (Permission perm : permissions) {
            if (permissionId.equals(perm.getName(uow))) {
                permission = perm;
                break;
            }
        }

        return permission;
    }

//    /**
//     * @param headers
//     *        the request headers (never <code>null</code>)
//     * @param uriInfo
//     *        the request URI information (never <code>null</code>)
//     * @param vdbName
//     *        the id of the VDB being deleted (cannot be empty)
//     * @return the JSON representation of the VDB that was deleted (never <code>null</code>)
//     * @throws KomodoRestException
//     *         if there is a problem deleting the specified workspace VDB or constructing the JSON representation
//     */
//    @DELETE
//    @Path( "{vdbName}" )
//    @Produces( MediaType.APPLICATION_JSON )
//    public Response deleteVdb( final @Context HttpHeaders headers,
//                               final @Context UriInfo uriInfo,
//                               final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {
//        UnitOfWork uow = null;
//
//        try {
//            uow = createTransaction( "deleteVdb", false ); //$NON-NLS-1$
//
//            // make sure VDB exists
//            if ( this.wsMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
//                final KomodoObject kobject = this.wsMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
//                final Vdb vdb = this.wsMgr.resolve( uow, kobject, Vdb.class );
//                vdb.remove( uow );
//
//                LOGGER.debug( "deleteVdb:VDB '{0}' was deleted", vdbName ); //$NON-NLS-1$
//                final Response response = commit( uow, headers.getAcceptableMediaTypes(), RestBasicEntity.NO_CONTENT );
//
//                return response;
//            }
//
//            LOGGER.debug( "deleteVdb:VDB '{0}' was not found to delete", vdbName ); //$NON-NLS-1$
//            return commit( uow, headers.getAcceptableMediaTypes(), new ResourceNotFound( vdbName, Messages.getString( DELETE_OPERATION_NAME ) ) );
//        } catch ( final Exception e ) {
//            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
//                uow.rollback();
//            }
//
//            if ( e instanceof KomodoRestException ) {
//                throw ( KomodoRestException )e;
//            }
//
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_GET_VDBS_ERROR, vdbName ), e );
//        }
//    }
//
//    private Response doAddVdb( final UnitOfWork uow,
//                               final URI baseUri,
//                               final RestVdb restVdb ) throws KomodoRestException {
//        assert( !uow.isRollbackOnly() );
//        assert( uow.getState() == State.NOT_STARTED );
//        assert( restVdb != null );
//
//        final String vdbName = restVdb.getId();
//        String extPath = Messages.getString( NO_VALUE );
//
//        //TODO
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
//            throw new KomodoRestException( RelationalMessages.getString( VDB_SERVICE_CREATE_VDB_ERROR ), e );
//        }
//
//        return null;
//    }

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
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Display the collection of vdbs",
                            response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbs( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo ) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find VDBs
            uow = createTransaction( "getVdbs", true ); //$NON-NLS-1$
            Vdb[] vdbs = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                vdbs = this.wsMgr.findVdbs( uow );
                LOGGER.debug( "getVdbs:found '{0}' VDBs", vdbs.length ); //$NON-NLS-1$
            } else {
                final String[] vdbPaths = this.wsMgr.findByType( uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, null, searchPattern, false );

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

            int start = 0;

            { // start query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.START );

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
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.SIZE );

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

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
            for ( final Vdb vdb : vdbs ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_VDBS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find vdb by name", response = RestVdb.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getVdb", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, mediaTypes.contains(MediaType.APPLICATION_XML_TYPE));
            final RestVdb restVdb = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restVdb );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_VDB_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find all models belonging to the vdb", response = RestVdb.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No models could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModels( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getModels", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model[] models = vdb.getModels(uow);
            if (models == null)
                models = new Model[0];

            List<RestVdbModel> restModels = new ArrayList<>(models.length);
            for (Model model : models) {
                RestVdbModel entity = entityFactory.create(model, uriInfo.getBaseUri(), uow);
                restModels.add(entity);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_MODELS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB model (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named model belonging to the vdb", response = RestVdbModel.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModel( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the model to be fetched", required = true)
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getModel", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            RestVdbModel restModel = entityFactory.create(model, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getModel:Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restModel);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_MODEL_ERROR, modelName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translators (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find all translators belonging to the vdb", response = RestVdbTranslator[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No translators could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslators( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getTranslators", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null)
                translators = new Translator[0];

            List<RestVdbTranslator> restTranslators = new ArrayList<>(translators.length);
            for (Translator translator : translators) {
                RestVdbTranslator entity = entityFactory.create(translator, uriInfo.getBaseUri(), uow);
                restTranslators.add(entity);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_TRANSLATORS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param translatorName
     *        the id of the translator being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translator (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATOR_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named translator belonging to the vdb", response = RestVdbTranslator.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No translator could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslator( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the translator to be fetched", required = true)
                            final @PathParam( "translatorName" ) String translatorName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getTranslator", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null || translators.length == 0) {
                LOGGER.debug("getTranslators:No translators found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, TRANSLATORS_SEGMENT, translatorName);
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
                              new ResourceNotFound(uri(vdbName, V1Constants.TRANSLATORS_SEGMENT, translatorName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbTranslator restTranslator = entityFactory.create(translator, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getTranslator:Translator '{0}' from VDB '{1}' entity was constructed", translatorName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restTranslator);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_TRANSLATOR_ERROR, translatorName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find all imports belonging to the vdb", response = RestVdbImport[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No imports could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getImports( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getImports", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null)
                imports = new VdbImport[0];

            List<RestVdbImport> restImports = new ArrayList<>(imports.length);
            for (VdbImport vdbImport : imports) {
                RestVdbImport entity = entityFactory.create(vdbImport, uriInfo.getBaseUri(), uow);
                restImports.add(entity);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_IMPORTS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param importName
     *        the id of the import being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORT_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named vdb import belonging to the vdb", response = RestVdbImport.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No import could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getImport( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the vdb import to be fetched", required = true)
                            final @PathParam( "importName" ) String importName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getImport", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null || imports.length == 0) {
                LOGGER.debug("getImport:No import found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, TRANSLATORS_SEGMENT, importName);
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
                              new ResourceNotFound(uri(vdbName, IMPORTS_SEGMENT, importName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbImport restImport = entityFactory.create(vdbImport, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getImport:Import '{0}' from VDB '{1}' entity was constructed", importName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restImport);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_IMPORT_ERROR, importName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON} )
    @ApiOperation(value = "Find all data roles belonging to the vdb", response = RestBasicEntity[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No data roles could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataRoles( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getDataRoles", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole[] dataRoles = vdb.getDataRoles(uow);
            if (dataRoles == null)
                dataRoles = new DataRole[0];

            List<RestVdbDataRole> restDataRoles = new ArrayList<>(dataRoles.length);
            for (DataRole dataRole : dataRoles) {
                RestVdbDataRole entity = entityFactory.create(dataRole, uriInfo.getBaseUri(), uow);
                restDataRoles.add(entity);
                LOGGER.debug("getDataRoles:Data role from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restDataRoles);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_DATA_ROLES_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named data role belonging to the vdb", response = RestVdbDataRole.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No data role could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataRole( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getDataRole", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null) {
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);
            }

            RestBasicEntity restDataRole = entityFactory.create(dataRole, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getDataRole:data role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restDataRole);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_DATA_ROLE_ERROR, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find all sources of the model belonging to the vdb", response = RestVdbModelSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No sources could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSources( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the model to be fetched", required = true)
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getSources", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            ModelSource[] sources = model.getSources(uow);
            if (sources == null)
                sources = new ModelSource[0];

            List<RestVdbModelSource> restSources = new ArrayList<>(sources.length);
            for (ModelSource source : sources) {
                RestVdbModelSource entity = entityFactory.create(source, uriInfo.getBaseUri(), uow);
                restSources.add(entity);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_SOURCES_ERROR, modelName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the id of the source being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named source belonging to the model of the vdb", response = RestVdbModelSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 404, message = "No source could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSource( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the model to be fetched", required = true)
                            final @PathParam( "modelName" ) String modelName,
                            @ApiParam(value = "Id of the model source to be fetched", required = true)
                            final @PathParam( "sourceName" ) String sourceName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getSource", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            ModelSource[] sources = model.getSources(uow);
            if (sources == null || sources.length == 0) {
                LOGGER.debug("getSource:No sources found for model '{0}' in vdb '{1}'", modelName, vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, MODELS_SEGMENT, modelName, SOURCES_SEGMENT);
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
                              new ResourceNotFound(uri(vdbName, MODELS_SEGMENT,
                                                                           modelName, SOURCES_SEGMENT,
                                                                           sourceName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbModelSource restSource = entityFactory.create(source, uriInfo.getBaseUri(), uow);
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

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_SOURCE_ERROR, sourceName, modelName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permissions (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON} )
    @ApiOperation(value = "Find all permissions belonging to the vdb data role", response = RestVdbPermission[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb or data role could be found with given names"),
        @ApiResponse(code = 200, message = "No permissions could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getPermissions( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getPermissions", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null) {
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);
            }

            Permission[] permissions = dataRole.getPermissions(uow);
            if (permissions == null)
                permissions = new Permission[0];

            List<RestVdbPermission> restPermissions = new ArrayList<>(permissions.length);
            for (Permission permission : permissions) {
                RestVdbPermission entity = entityFactory.create(permission, uriInfo.getBaseUri(), uow);
                restPermissions.add(entity);
                LOGGER.debug("getPermissions:Permission from Data role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restPermissions);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_PERMISSIONS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the named permission belonging to the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getPermission( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Id of the permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getPermission", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            RestVdbPermission restPermission = entityFactory.create(permission, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getPermission:permission '{0}' from data role '{1}' from VDB '{2}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restPermission);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_PERMISSION_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission's conditions (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the conditions belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with given names"),
        @ApiResponse(code = 200, message = "No conditions could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConditions( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Id of the permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getConditions", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Condition[] conditions = permission.getConditions(uow);
            List<RestVdbCondition> restConditions = new ArrayList<>(conditions.length);
            for (Condition condition : conditions) {
                RestVdbCondition entity = entityFactory.create(condition, uriInfo.getBaseUri(), uow);
                restConditions.add(entity);
                LOGGER.debug("getConditions:Condition from Permission from Data Role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restConditions);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_CONDITIONS_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @param conditionId
     *        the id of the condition being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission condition (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITION_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the condition belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role, permission or condition could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getCondition( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Id of the permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId,
                            @ApiParam(value = "Id of the condition to be fetched", required = true)
                            final @PathParam( "conditionId" ) String conditionId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getCondition", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Condition[] conditions = permission.getConditions(uow);
            if (conditions == null || conditions.length == 0) {
                LOGGER.debug("getConition:No conditions found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, DATA_ROLES_SEGMENT,
                                                          dataRoleId, PERMISSIONS_SEGMENT,
                                                          permissionId, CONDITIONS_SEGMENT,
                                                          conditionId);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Condition condition = null;
            for (Condition con : conditions) {
                if (conditionId.equals(con.getName(uow))) {
                    condition = con;
                    break;
                }
            }

            if (condition == null) {
                LOGGER.debug("No condition '{0}' for permission '{1}' for data role '{2}' found for vdb '{3}'", //$NON-NLS-1$
                             conditionId, permissionId, dataRoleId, vdbName);
                return commit(uow, mediaTypes, new ResourceNotFound(
                                                                    uri(vdbName, DATA_ROLES_SEGMENT,
                                                                        dataRoleId, PERMISSIONS_SEGMENT,
                                                                        permissionId, CONDITIONS_SEGMENT,
                                                                        conditionId),
                                                                    Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbCondition restCondition = entityFactory.create(condition, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getCondition:condition '{0}' from permission '{1}' from data role '{2}' from VDB '{3}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restCondition);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_CONDITION_ERROR, conditionId, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission's masks (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MASKS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the masks belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with given names"),
        @ApiResponse(code = 200, message = "No masks could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getMasks( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Id of the permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getMasks", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Mask[] masks = permission.getMasks(uow);
            List<RestVdbMask> restMasks = new ArrayList<>(masks.length);
            for (Mask mask : masks) {
                RestVdbMask entity = entityFactory.create(mask, uriInfo.getBaseUri(), uow);
                restMasks.add(entity);
                LOGGER.debug("getMasks:Mask from Permission from Data Role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restMasks);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_MASKS_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @param maskId
     *        the id of the mask being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission mask (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MASKS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MASK_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find the mask belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role, permission or mask could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getMask( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Id of the data role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Id of the permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId,
                            @ApiParam(value = "Id of the mask to be fetched", required = true)
                            final @PathParam( "maskId" ) String maskId) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getMask", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Mask[] masks = permission.getMasks(uow);
            if (masks == null || masks.length == 0) {
                LOGGER.debug("getConition:No masks found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, DATA_ROLES_SEGMENT,
                                                          dataRoleId, PERMISSIONS_SEGMENT,
                                                          permissionId, MASKS_SEGMENT,
                                                          maskId);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Mask mask = null;
            for (Mask con : masks) {
                if (maskId.equals(con.getName(uow))) {
                    mask = con;
                    break;
                }
            }

            if (mask == null) {
                LOGGER.debug("No mask '{0}' for permission '{1}' for data role '{2}' found for vdb '{3}'", //$NON-NLS-1$
                             maskId, permissionId, dataRoleId, vdbName);
                return commit(uow, mediaTypes, new ResourceNotFound(
                                                                    uri(vdbName, DATA_ROLES_SEGMENT,
                                                                        dataRoleId, PERMISSIONS_SEGMENT,
                                                                        permissionId, MASKS_SEGMENT,
                                                                        maskId),
                                                                    Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbMask restMask = entityFactory.create(mask, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getMask:mask '{0}' from permission '{1}' from data role '{2}' from VDB '{3}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restMask);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, VDB_SERVICE_GET_MASK_ERROR, maskId, permissionId, dataRoleId, vdbName);
        }
    }
}
