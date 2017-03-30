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
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
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
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Connection information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.CONNECTIONS_SEGMENT)
@Api(tags = {V1Constants.CONNECTIONS_SEGMENT})
public final class KomodoConnectionService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoConnectionService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * Get the Connections from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Connections in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Connection JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the collection of connections",
                            response = RestDataSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnections( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find Data sources
            uow = createTransaction(principal, "getConnections", true ); //$NON-NLS-1$
            Datasource[] dataSources = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                dataSources = getWorkspaceManager(uow).findDatasources( uow );
                LOGGER.debug( "getConnections:found '{0}' Datasources", dataSources.length ); //$NON-NLS-1$
            } else {
                final String[] datasourcePaths = getWorkspaceManager(uow).findByType( uow, DataVirtLexicon.Connection.NODE_TYPE, null, searchPattern, false );

                if ( datasourcePaths.length == 0 ) {
                    dataSources = Datasource.NO_DATASOURCES;
                } else {
                    dataSources = new Datasource[ datasourcePaths.length ];
                    int i = 0;

                    for ( final String path : datasourcePaths ) {
                        dataSources[ i++ ] = getWorkspaceManager(uow).resolve( uow, new ObjectImpl( getWorkspaceManager(uow).getRepository(), path, 0 ), Datasource.class );
                    }

                    LOGGER.debug( "getConnections:found '{0}' DataSources using pattern '{1}'", dataSources.length, searchPattern ); //$NON-NLS-1$
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

            final List< RestDataSource > entities = new ArrayList<RestDataSource>();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            for ( final Datasource dataSource : dataSources ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {                        
                        RestDataSource entity = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
                        LOGGER.debug("getConnections:Datasource '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_GET_DATASOURCES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the JSON representation of the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Connection or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find connection by name", response = RestDataSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "Name of the connection",
                                             required = true
                                   )
                                   final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getConnection", true ); //$NON-NLS-1$

            Datasource datasource = findDatasource(uow, connectionName);
            if (datasource == null)
                return commitNoDatasourceFound(uow, mediaTypes, connectionName);

            KomodoProperties properties = new KomodoProperties();
            final RestDataSource restDatasource = entityFactory.create(datasource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getConnection:Datasource '{0}' entity was constructed", datasource.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restDatasource );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_GET_DATASOURCE_ERROR, connectionName);
        }
    }

    /**
     * Create a new Connection in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param connectionJson
     *        the connection JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Connection
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a connection in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                                value = "Name of the connection",
                                                required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName,
                                      @ApiParam(
                                                value = "" + 
                                                        "JSON of the properties of the new connection:<br>" +
                                                        OPEN_PRE_TAG +
                                                        OPEN_BRACE + BR +
                                                        NBSP + "keng\\_\\_id: \"id of the connection\"" + BR +
                                                        NBSP + "dv\\_\\_driverName: \"name of the driver, eg. mysql\"" + BR +
                                                        NBSP + "dv\\_\\_jndiName: \"the jndi name of the connection\"" + BR +
                                                        NBSP + "dv\\_\\_type: \"true if jdbc, otherwise false\"" + BR +
                                                        CLOSE_BRACE +
                                                        CLOSE_PRE_TAG,
                                                required = true
                                      )
                                      final String connectionJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_MISSING_NAME);
        }

        final RestDataSource restDatasource = KomodoJsonMarshaller.unmarshall( connectionJson, RestDataSource.class );
        final String jsonDatasourceName = restDatasource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDatasourceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_JSON_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = connectionName.equals( jsonDatasourceName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_SOURCE_NAME_ERROR, connectionName, jsonDatasourceName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, connectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_ALREADY_EXISTS);
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_CREATE_DATASOURCE_ERROR, connectionName);
        }
    }

    /**
     * Clone a Connection in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param newConnectionName
     *        the new connection name (cannot be empty)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Connection
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CLONE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Clone a connection in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response cloneConnection( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam(
                                               value = "Name of the connection",
                                               required = true
                                     )
                                     final @PathParam( "connectionName" ) String connectionName,
                                     @ApiParam(
                                               value = "The new name of the connection",
                                               required = true
                                     )
                                     final String newConnectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_MISSING_NAME);
        }

        // Error if the new connection name is missing
        if ( StringUtils.isBlank( newConnectionName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_MISSING_NEW_NAME);
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = connectionName.equals( newConnectionName );
        if ( namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_SAME_NAME_ERROR, newConnectionName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "cloneConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, newConnectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_ALREADY_EXISTS);
            }

            // create new Connection
            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            final Datasource oldDatasource = getWorkspaceManager(uow).resolve( uow, kobject, Datasource.class );
            final RestDataSource oldEntity = entityFactory.create(oldDatasource, uriInfo.getBaseUri(), uow );
            
            final Datasource datasource = getWorkspaceManager(uow).createDatasource( uow, null, newConnectionName);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_CLONE_DATASOURCE_ERROR);
        }
    }

    /**
     * Update a Connection in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param connectionJson
     *        the connection JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @PUT
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a connection in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                                value = "Name of the connection",
                                                required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName,
                                      @ApiParam(
                                                value = "" + 
                                                        "JSON of the properties of the connection:<br>" +
                                                        OPEN_PRE_TAG +
                                                        OPEN_BRACE + BR +
                                                        NBSP + "keng\\_\\_id: \"id of the connection\"" + BR +
                                                        NBSP + "dv\\_\\_driverName: \"name of the driver, eg. mysql\"" + BR +
                                                        NBSP + "dv\\_\\_jndiName: \"the jndi name of the connection\"" + BR +
                                                        NBSP + "dv\\_\\_type: \"true if jdbc, otherwise false\"" + BR +
                                                        CLOSE_BRACE +
                                                        CLOSE_PRE_TAG,
                                                required = true
                                      )
                                      final String connectionJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_MISSING_NAME);
        }

        final RestDataSource restDatasource = KomodoJsonMarshaller.unmarshall( connectionJson, RestDataSource.class );
        final String jsonDatasourceName = restDatasource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDatasourceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_JSON_MISSING_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateConnection", false ); //$NON-NLS-1$

            final boolean exists = getWorkspaceManager(uow).hasChild( uow, connectionName );
            // Error if the specified service does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_SOURCE_DNE);
            }

            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            final Datasource datasource = getWorkspaceManager(uow).resolve( uow, kobject, Datasource.class );

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, datasource, restDatasource);

            // rename if names did not match
            final boolean namesMatch = connectionName.equals( jsonDatasourceName );
            if ( !namesMatch ) {
                datasource.rename( uow, jsonDatasourceName );
            }

            KomodoProperties properties = new KomodoProperties();
            final RestDataSource entity = entityFactory.create(datasource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateConnection: datasource '{0}' entity was updated", datasource.getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_UPDATE_DATASOURCE_ERROR);
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
            final Datasource datasource = getWorkspaceManager(uow).createDatasource( uow, null, datasourceName);

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
        // 'New' = requested RestDatasource properties
        String newJndiName = restDataSource.getJndiName();
        String newDriverName = restDataSource.getDriverName();
        boolean newJdbc = restDataSource.isJdbc();
        
        // 'Old' = current Datasource properties
        String oldJndiName = dataService.getJndiName(uow);
        String oldDriverName = dataService.getDriverName(uow);
        boolean oldJdbc = dataService.isJdbc(uow);
        
        // JndiName
        if ( !StringUtils.equals(newJndiName, oldJndiName) ) {
            dataService.setJndiName( uow, newJndiName );
        } 
        // DriverName
        if ( !StringUtils.equals(newDriverName, oldDriverName) ) {
            dataService.setDriverName( uow, newDriverName );
        } 
        // jdbc
        if ( newJdbc != oldJdbc ) {
            dataService.setJdbc( uow, newJdbc );
        }
    }

    /**
     * Delete the specified Connection from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the name of the connection to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{connectionName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a connection from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                                value = "Name of the connection",
                                                required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeConnectionFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager mgr = WorkspaceManager.getInstance( repo, uow );
            KomodoObject datasource = mgr.getChild(uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE);

            if (datasource == null)
                return Response.noContent().build();

            mgr.delete(uow, datasource);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            if (mgr.hasChild(uow, connectionName))
                kso.addAttribute(connectionName, "Deletion failure"); //$NON-NLS-1$
            else
                kso.addAttribute(connectionName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASOURCE_SERVICE_DELETE_DATASOURCE_ERROR);
        }
    }

}
