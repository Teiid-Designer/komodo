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

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Datasource information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.DATA_SOURCES_SEGMENT)
@Api(tags = {V1Constants.DATA_SOURCES_SEGMENT})
public final class KomodoDatasourceService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoDatasourceService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * Get the Datasources from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Datasources in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Datasource JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of data sources",
                            response = RestDataSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDatasources( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find Data sources
            uow = createTransaction( "getDatasources", true ); //$NON-NLS-1$
            Datasource[] dataSources = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                dataSources = this.wsMgr.findDatasources( uow );
                LOGGER.debug( "getDatasources:found '{0}' Datasources", dataSources.length ); //$NON-NLS-1$
            } else {
                final String[] datasourcePaths = this.wsMgr.findByType( uow, KomodoLexicon.DataSource.NODE_TYPE, null, searchPattern, false );

                if ( datasourcePaths.length == 0 ) {
                    dataSources = Datasource.NO_DATASOURCES;
                } else {
                    dataSources = new Datasource[ datasourcePaths.length ];
                    int i = 0;

                    for ( final String path : datasourcePaths ) {
                        dataSources[ i++ ] = this.wsMgr.resolve( uow, new ObjectImpl( this.wsMgr.getRepository(), path, 0 ), Datasource.class );
                    }

                    LOGGER.debug( "getDatasources:found '{0}' DataSources using pattern '{1}'", dataSources.length, searchPattern ); //$NON-NLS-1$
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

            final List< RestDataSource > entities = new ArrayList< >();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            for ( final Datasource dataSource : dataSources ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        RestDataSource entity = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
                        LOGGER.debug("getDatasources:Datasource '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_GET_DATASOURCES_ERROR);
        }
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the id of the Datasource being retrieved (cannot be empty)
     * @return the JSON representation of the Datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Datasource or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find datasource by name", response = RestDataSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Datasource could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDatasource( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(value = "Id of the datasource to be fetched", required = true)
                                    final @PathParam( "datasourceName" ) String datasourceName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( "getDatasource", true ); //$NON-NLS-1$

            Datasource datasource = findDatasource(uow, datasourceName);
            if (datasource == null)
                return commitNoDatasourceFound(uow, mediaTypes, datasourceName);

            KomodoProperties properties = new KomodoProperties();
            final RestDataSource restDatasource = entityFactory.create(datasource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", datasource.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restDatasource );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_GET_DATASOURCE_ERROR, datasourceName);
        }
    }
    
    /**
     * Create a new DataSource in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the datasource name (cannot be empty)
     * @param datasourceJson
     *        the datasource JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataSource
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a datasource in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createDatasource( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       final @PathParam( "datasourceName" ) String datasourceName,
                                       final String datasourceJson) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the datasource name is missing
        if (StringUtils.isBlank( datasourceName )) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_MISSING_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        final RestDataSource restDatasource = KomodoJsonMarshaller.unmarshall( datasourceJson, RestDataSource.class );
        final String jsonDatasourceName = restDatasource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDatasourceName ) ) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_JSON_MISSING_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = datasourceName.equals( jsonDatasourceName );
        if ( !namesMatch ) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_SOURCE_NAME_ERROR, datasourceName, jsonDatasourceName);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( "createDatasource", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a datasource with the supplied name.
            if ( this.wsMgr.hasChild( uow, datasourceName ) ) {
                String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_ALREADY_EXISTS);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }
            
            // create new Datasource
            return doAddDatasource( uow, uriInfo.getBaseUri(), mediaTypes, restDatasource );
            
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_DATASOURCE_ERROR, datasourceName);
        }
    }
    
    /**
     * Clone a DataSource in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the datasource name (cannot be empty)
     * @param newDatasourceName
     *        the new datasource name (cannot be empty)
     * @return a JSON representation of the new datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataSource
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CLONE_DATA_SOURCE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Clone a datasource in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response cloneDatasource( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       final @PathParam( "datasourceName" ) String datasourceName,
                                       final String newDatasourceName) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the datasource name is missing
        if (StringUtils.isBlank( datasourceName )) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_MISSING_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        // Error if the new datasource name is missing
        if ( StringUtils.isBlank( newDatasourceName ) ) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_MISSING_NEW_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = datasourceName.equals( newDatasourceName );
        if ( namesMatch ) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_SAME_NAME_ERROR, newDatasourceName);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( "cloneDatasource", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a datasource with the supplied name.
            if ( this.wsMgr.hasChild( uow, newDatasourceName ) ) {
                String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_ALREADY_EXISTS);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }
            
            // create new Datasource
            // must be an update
            final KomodoObject kobject = this.wsMgr.getChild( uow, datasourceName, KomodoLexicon.DataSource.NODE_TYPE );
            final Datasource oldDatasource = this.wsMgr.resolve( uow, kobject, Datasource.class );
            final RestDataSource oldEntity = entityFactory.create(oldDatasource, uriInfo.getBaseUri(), uow );
            
            final Datasource datasource = this.wsMgr.createDatasource( uow, null, newDatasourceName);

            setProperties( uow, datasource, oldEntity );

            final RestDataSource entity = entityFactory.create(datasource, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_DATASOURCE_ERROR);
        }
    }
    
    /**
     * Update a Datasource in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the datasource name (cannot be empty)
     * @param datasourceJson
     *        the datasource JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @PUT
    @Path( StringConstants.FORWARD_SLASH + V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a datasource in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateDatasource( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       final @PathParam( "datasourceName" ) String datasourceName,
                                       final String datasourceJson) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the datasource name is missing
        if (StringUtils.isBlank( datasourceName )) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_MISSING_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }


        final RestDataSource restDatasource = KomodoJsonMarshaller.unmarshall( datasourceJson, RestDataSource.class );
        final String jsonDatasourceName = restDatasource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDatasourceName ) ) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_JSON_MISSING_NAME);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction( "updateDatasource", false ); //$NON-NLS-1$

            final boolean exists = this.wsMgr.hasChild( uow, datasourceName );
            // Error if the specified service does not exist
            if ( !exists ) {
                String errorMessage = RelationalMessages.getString(RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_SOURCE_DNE);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            // must be an update
            final KomodoObject kobject = this.wsMgr.getChild( uow, datasourceName, KomodoLexicon.DataSource.NODE_TYPE );
            final Datasource datasource = this.wsMgr.resolve( uow, kobject, Datasource.class );

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, datasource, restDatasource);

            // rename if names did not match
            final boolean namesMatch = datasourceName.equals( jsonDatasourceName );
            if ( !namesMatch ) {
                datasource.rename( uow, jsonDatasourceName );
            }

            KomodoProperties properties = new KomodoProperties();
            final RestDataSource entity = entityFactory.create(datasource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateDatasource: datasource '{0}' entity was updated", datasource.getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_DATASOURCE_ERROR);
        }
    }

    private Response doAddDatasource( final UnitOfWork uow,
                                       final URI baseUri,
                                       final List<MediaType> mediaTypes,
                                       final RestDataSource restDatasource ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restDatasource != null );

        final String datasourceName = restDatasource.getId();
        try {
            final Datasource datasource = this.wsMgr.createDatasource( uow, null, datasourceName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, datasource, restDatasource);

            final RestDataSource entity = entityFactory.create(datasource, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_DATASOURCE_ERROR, datasourceName ), e );
        }
    }
    
    // Sets Datasource properties using the supplied RestDatasource object
    private void setProperties(final UnitOfWork uow, Datasource dataService, RestDataSource restDataSource) throws KException {
//        // 'New' = requested RestDatasource properties
//        String newDescription = restDataSource.getDescription();
//        
//        // 'Old' = current Datasource properties
//        String oldDescription = dataService.getDescription(uow);
//        
//        // Description
//        if ( !StringUtils.equals(newDescription, oldDescription) ) {
//            dataService.setDescription( uow, newDescription );
//        } 
    }

    /**
     * Delete the specified Datasource from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the name of the datasource to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{datasourceName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a datasource from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteDatasource( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       final @PathParam( "datasourceName" ) String datasourceName) throws KomodoRestException {
        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction("removeDatasourceFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager mgr = WorkspaceManager.getInstance( repo );
            KomodoObject datasource = mgr.getChild(uow, datasourceName, KomodoLexicon.DataSource.NODE_TYPE);
            
            if (datasource == null)
                return Response.noContent().build();

            mgr.delete(uow, datasource);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(datasourceName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_DELETE_DATASOURCE_ERROR);
        }
    }
    
}
