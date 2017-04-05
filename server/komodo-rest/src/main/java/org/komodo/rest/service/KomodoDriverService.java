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

import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService.QueryParamKeys;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Driver information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DRIVERS_SEGMENT)
@Api(tags = {V1Constants.DRIVERS_SEGMENT})
public final class KomodoDriverService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoDriverService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * Get the Drivers from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Drivers in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Driver JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the collection of installed drivers",
                            response = RestConnectionDriver[].class)
    @ApiImplicitParams({
        @ApiImplicitParam(
                          name = QueryParamKeys.PATTERN,
                          value = "A regex expression used when searching. If not present, all objects are returned.",
                          required = false,
                          dataType = "string",
                          paramType = "query"),
        @ApiImplicitParam(
                          name = QueryParamKeys.SIZE,
                          value = "The number of objects to return. If not present, all objects are returned",
                          required = false,
                          dataType = "integer",
                          paramType = "query"),
        @ApiImplicitParam(
                          name = QueryParamKeys.START,
                          value = "Index of the first dataservice to return",
                          required = false,
                          dataType = "integer",
                          paramType = "query")
    })
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDrivers( final @Context HttpHeaders headers,
                                final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find Drivers
            uow = createTransaction(principal, "getDrivers", true ); //$NON-NLS-1$
            Driver[] drivers = null;
            WorkspaceManager wsMgr = getWorkspaceManager(uow);

            if ( StringUtils.isBlank( searchPattern ) ) {
                drivers = wsMgr.findDrivers( uow );
                LOGGER.debug( "getDrivers:found '{0}' Drivers", drivers.length ); //$NON-NLS-1$
            } else {
                final String[] driverPaths = wsMgr.findByType( uow, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE, null, searchPattern, false );

                if ( driverPaths.length == 0 ) {
                    drivers = Driver.NO_DRIVERS;
                } else {
                    drivers = new Driver[ driverPaths.length ];
                    int i = 0;

                    for ( final String path : driverPaths ) {
                        drivers[ i++ ] = wsMgr.resolve( uow, new ObjectImpl( wsMgr.getRepository(), path, 0 ), Driver.class );
                    }

                    LOGGER.debug( "getDrivers:found '{0}' Drivers using pattern '{1}'", drivers.length, searchPattern ); //$NON-NLS-1$
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

            final List< RestConnectionDriver > entities = new ArrayList< >();
            int i = 0;

            for ( final Driver driver : drivers ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        ConnectionDriver aDriver = new ConnectionDriver(driver.getName(uow),null);
                        RestConnectionDriver entity = new RestConnectionDriver(aDriver);
                        entities.add(entity);
                        LOGGER.debug("getDrivers:Driver '{0}' entity was constructed", driver.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DRIVER_SERVICE_GET_DRIVERS_ERROR);
        }
    }
        
}
