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

import static org.komodo.rest.relational.RelationalMessages.Error.CONNECTION_SERVICE_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.CONNECTION_SERVICE_NAME_VALIDATION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_DATA_SOURCE_NAME_EXISTS;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
import org.komodo.relational.connection.Connection;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
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
                            response = RestConnection[].class)
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

            // find connections
            uow = createTransaction(principal, "getConnections", true ); //$NON-NLS-1$
            Connection[] connections = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                connections = getWorkspaceManager(uow).findConnections( uow );
                LOGGER.debug( "getConnections:found '{0}' Connections", connections.length ); //$NON-NLS-1$
            } else {
                final String[] connectionPaths = getWorkspaceManager(uow).findByType( uow, DataVirtLexicon.Connection.NODE_TYPE, null, searchPattern, false );

                if ( connectionPaths.length == 0 ) {
                    connections = Connection.NO_CONNECTIONS;
                } else {
                    connections = new Connection[ connectionPaths.length ];
                    int i = 0;

                    for ( final String path : connectionPaths ) {
                        connections[ i++ ] = getWorkspaceManager(uow).resolve( uow, new ObjectImpl( getWorkspaceManager(uow).getRepository(), path, 0 ), Connection.class );
                    }

                    LOGGER.debug( "getConnections:found '{0}' Connections using pattern '{1}'", connections.length, searchPattern ); //$NON-NLS-1$
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

            final List< RestConnection > entities = new ArrayList< >();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            for ( final Connection connection : connections ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {                        
                        RestConnection entity = entityFactory.create(connection, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
                        LOGGER.debug("getConnections:Connection '{0}' entity was constructed", connection.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_GET_CONNECTIONS_ERROR);
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
    @ApiOperation(value = "Find connection by name", response = RestConnection.class)
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

            Connection connection = findConnection(uow, connectionName);
            if (connection == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            KomodoProperties properties = new KomodoProperties();
            final RestConnection restConnection = entityFactory.create(connection, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getConnection:Connection '{0}' entity was constructed", connection.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restConnection );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_GET_CONNECTION_ERROR, connectionName);
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
                                                        NBSP + "driverName: \"name of the driver, eg. mysql\"" + COMMA + BR +
                                                        NBSP + "jndiName: \"the jndi name of the connection\"" + COMMA + BR +
                                                        NBSP + "jdbc: \"true if jdbc, otherwise false\"" + BR +
                                                        NBSP + "parameters: " + OPEN_BRACE + BR +
                                                        NBSP + NBSP + "property1...n: \"key/value pairs of properties applicable to connection" + BR +
                                                        NBSP + CLOSE_BRACE + BR +
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
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CREATE_MISSING_NAME);
        }

        RestConnection restConnection = new RestConnection();
        restConnection.setId(connectionName);

        final KomodoConnectionAttributes rcAttr;
        try {
            rcAttr = KomodoJsonMarshaller.unmarshall(connectionJson, KomodoConnectionAttributes.class);

            restConnection.setDriverName(rcAttr.getDriver());
            restConnection.setJndiName(rcAttr.getJndi());
            restConnection.setJdbc(rcAttr.isJdbc());

            for (Map.Entry<String, Object> entry : rcAttr.getParameters().entrySet()) {
                restConnection.addProperty(entry.getKey(), entry.getValue());
            }

        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "createConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, connectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CREATE_ALREADY_EXISTS);
            }

            // create new Connection
            return doAddConnection( uow, uriInfo.getBaseUri(), mediaTypes, restConnection );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_CREATE_CONNECTION_ERROR, connectionName);
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
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_MISSING_NAME);
        }

        // Error if the new connection name is missing
        if ( StringUtils.isBlank( newConnectionName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_MISSING_NEW_NAME);
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = connectionName.equals( newConnectionName );
        if ( namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_SAME_NAME_ERROR, newConnectionName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "cloneConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, newConnectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_ALREADY_EXISTS);
            }

            // create new Connection
            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            final Connection oldConnection = getWorkspaceManager(uow).resolve( uow, kobject, Connection.class );
            final RestConnection oldEntity = entityFactory.create(oldConnection, uriInfo.getBaseUri(), uow );
            
            final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, newConnectionName);

            setProperties( uow, connection, oldEntity );

            final RestConnection entity = entityFactory.create(connection, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_CONNECTION_ERROR);
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
                                                        NBSP + "driverName: \"name of the driver, eg. mysql\"" + COMMA + BR +
                                                        NBSP + "jndiName: \"the jndi name of the connection\"" + COMMA + BR +
                                                        NBSP + "jdbc: \"true if jdbc, otherwise false\"" + BR +
                                                        NBSP + "parameters: " + OPEN_BRACE + BR +
                                                        NBSP + NBSP + "property1...n: \"key/value pairs of properties applicable to connection" + BR +
                                                        NBSP + CLOSE_BRACE + BR +
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
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_UPDATE_MISSING_NAME);
        }

        RestConnection restConnection = new RestConnection();
        restConnection.setId(connectionName);

        final KomodoConnectionAttributes rcAttr;
        try {
            rcAttr = KomodoJsonMarshaller.unmarshall(connectionJson, KomodoConnectionAttributes.class);

            restConnection.setDriverName(rcAttr.getDriver());
            restConnection.setJndiName(rcAttr.getJndi());
            restConnection.setJdbc(rcAttr.isJdbc());

            for (Map.Entry<String, Object> entry : rcAttr.getParameters().entrySet()) {
                restConnection.addProperty(entry.getKey(), entry.getValue());
            }

        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateConnection", false ); //$NON-NLS-1$

            final boolean exists = getWorkspaceManager(uow).hasChild( uow, connectionName );
            // Error if the specified service does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_UPDATE_SOURCE_DNE);
            }

            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            getWorkspaceManager(uow).delete(uow, kobject);

            Response response = doAddConnection( uow, uriInfo.getBaseUri(), mediaTypes, restConnection );

            LOGGER.debug("updateConnection: connection '{0}' entity was updated", connectionName); //$NON-NLS-1$

            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_UPDATE_CONNECTION_ERROR);
        }
    }

    private Response doAddConnection( final UnitOfWork uow,
                                      final URI baseUri,
                                      final List<MediaType> mediaTypes,
                                      final RestConnection restConnection ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restConnection != null );

        final String connectionName = restConnection.getId();
        try {
            final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, connectionName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, connection, restConnection);

            final RestConnection entity = entityFactory.create(connection, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.CONNECTION_SERVICE_CREATE_CONNECTION_ERROR, connectionName ), e );
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
            KomodoObject connection = mgr.getChild(uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE);

            if (connection == null)
                return Response.noContent().build();

            mgr.delete(uow, connection);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_DELETE_CONNECTION_ERROR);
        }
    }

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param connectionName
     *            the Connection name being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is
     *         either an empty string, when the name is valid, or an error
     *         message
     * @throws KomodoRestException
     *             if there is a problem validating the name or constructing
     *             the response
     */
    @GET
    @Path( V1Constants.NAME_VALIDATION_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the Connection name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 500, message = "The Connection name cannot be empty." )
    } )
    public Response validateConnectionName( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam( value = "The Connection name being checked", required = true )
                                     final @PathParam( "connectionName" ) String connectionName ) throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName( connectionName );
        
        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "validateConnectionName", true ); //$NON-NLS-1$

            // make sure an existing Connection does not have that name
            final Connection connection = findConnection( uow, connectionName );

            if ( connection == null ) {
                // make sure an existing vdb does not have the same name
                final Vdb ds = findVdb( uow, connectionName );

                if ( ds == null ) {
                    // name is valid
                    return Response.ok().build();
                }

                // name is the same as an existing connection
                return Response.ok()
                               .entity( RelationalMessages.getString( VDB_DATA_SOURCE_NAME_EXISTS ) )
                               .build();
            }

            // name is the same as an existing connection
            return Response.ok()
                           .entity( RelationalMessages.getString( CONNECTION_SERVICE_NAME_EXISTS ) )
                           .build();
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( headers.getAcceptableMediaTypes(), 
                                                     e, 
                                                     CONNECTION_SERVICE_NAME_VALIDATION_ERROR );
        }
    }
}
