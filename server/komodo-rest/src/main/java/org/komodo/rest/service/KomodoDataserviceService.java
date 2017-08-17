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

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_FIND_SOURCE_VDB_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_FIND_VIEW_INFO_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_CONNECTIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DRIVERS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_NAME_VALIDATION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_NAME_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_DATASERVICE_ERROR;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
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
import org.komodo.relational.ViewBuilderCriteriaPredicate;
import org.komodo.relational.ViewDdlBuilder;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoDataserviceUpdateAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestDataserviceViewInfo;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.teiid.language.SQLConstants;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Dataservice information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.DATA_SERVICES_SEGMENT)
@Api(tags = {V1Constants.DATA_SERVICES_SEGMENT})
public final class KomodoDataserviceService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;

    private static final String SERVICE_VDB_SUFFIX = "VDB"; //$NON-NLS-1$
    private static final String SERVICE_VDB_VIEW_MODEL = "views"; //$NON-NLS-1$
    private static final String SERVICE_VDB_VIEW_SUFFIX = "View"; //$NON-NLS-1$
    private static final String LH_TABLE_ALIAS = "A"; //$NON-NLS-1$
    private static final String LH_TABLE_ALIAS_DOT = "A."; //$NON-NLS-1$
    private static final String RH_TABLE_ALIAS = "B"; //$NON-NLS-1$
    private static final String RH_TABLE_ALIAS_DOT = "B."; //$NON-NLS-1$
    private static final String INNER_JOIN = "INNER JOIN"; //$NON-NLS-1$
    private static final String LEFT_OUTER_JOIN = "LEFT OUTER JOIN"; //$NON-NLS-1$
    private static final String RIGHT_OUTER_JOIN = "RIGHT OUTER JOIN"; //$NON-NLS-1$
    private static final String FULL_OUTER_JOIN = "FULL OUTER JOIN"; //$NON-NLS-1$
    private static final String OR = "OR"; //$NON-NLS-1$
    private static final String AND = "AND"; //$NON-NLS-1$
    private static final String EQ = "="; //$NON-NLS-1$
    private static final String NE = "<>"; //$NON-NLS-1$
    private static final String LT = "<"; //$NON-NLS-1$
    private static final String GT = ">"; //$NON-NLS-1$
    private static final String LE = "<="; //$NON-NLS-1$
    private static final String GE = ">="; //$NON-NLS-1$
    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoDataserviceService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * Get the Dataservices from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Dataservices in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Dataservices JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(
                  value = "Return the collection of data services",
                  response = RestDataservice[].class)
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
    @ApiResponses(
                  value = {
                      @ApiResponse(
                                   code = 403,
                                   message = "An error has occurred.")
                  }
    )
    public Response getDataservices( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find Data services
            uow = createTransaction(principal, "getDataservices", true ); //$NON-NLS-1$
            Dataservice[] dataServices = null;

            if ( StringUtils.isBlank( searchPattern ) ) {
                dataServices = getWorkspaceManager(uow).findDataservices( uow );
                LOGGER.debug( "getDataservices:found '{0}' Dataservices", dataServices.length ); //$NON-NLS-1$
            } else {
                final String[] dataservicePaths = getWorkspaceManager(uow).findByType( uow, DataVirtLexicon.DataService.NODE_TYPE, null, searchPattern, false );

                if ( dataservicePaths.length == 0 ) {
                    dataServices = Dataservice.NO_DATASERVICES;
                } else {
                    dataServices = new Dataservice[ dataservicePaths.length ];
                    int i = 0;

                    for ( final String path : dataservicePaths ) {
                        dataServices[ i++ ] = getWorkspaceManager(uow).resolve( uow, new ObjectImpl( getWorkspaceManager(uow).getRepository(), path, 0 ), Dataservice.class );
                    }

                    LOGGER.debug( "getDataservices:found '{0}' DataServices using pattern '{1}'", dataServices.length, searchPattern ); //$NON-NLS-1$
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

            final List< RestDataservice > entities = new ArrayList< >();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            for ( final Dataservice dataService : dataServices ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        RestDataservice entity = entityFactory.create(dataService, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
                        LOGGER.debug("getDataservices:Dataservice '{0}' entity was constructed", dataService.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice being retrieved (cannot be empty)
     * @return the JSON representation of the Dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Dataservice or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(
                  value = "Find dataservice by name",
                  response = RestDataservice.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataservice( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(
                                              value = "Id of the dataservice to be fetched, ie. the value of the 'keng__id' property",
                                              required = true
                                    )
                                    final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataservice", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            KomodoProperties properties = new KomodoProperties();
            final RestDataservice restDataservice = entityFactory.create(dataservice, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getDataservice:Dataservice '{0}' entity was constructed", dataservice.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restDataservice );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICE_ERROR, dataserviceName);
        }
    }

    /**
     * Create a new DataService in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the dataservice name (cannot be empty)
     * @param dataserviceJson
     *        the dataservice JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataService
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(
                  value = "Create a dataservice in the workspace",
                  consumes=MediaType.APPLICATION_JSON
    )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createDataservice( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(
                                                 value = "Name of the data service",
                                                 required = true
                                       )
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
                                       @ApiParam(
                                                 value = "" + 
                                                         "JSON of the properties of the new data service:<br>" +
                                                         OPEN_PRE_TAG +
                                                         OPEN_BRACE + BR +
                                                         NBSP + "keng\\_\\_id: \"id of the data service\"" + COMMA + BR +
                                                         NBSP + OPEN_PRE_CMT + "(identical to dataserviceName parameter)" + CLOSE_PRE_CMT + BR + BR +
                                                         NBSP + "tko__description: \"the description\"" + BR +
                                                         CLOSE_BRACE +
                                                         CLOSE_PRE_TAG,
                                                 required = true
                                       )
                                       final String dataserviceJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_MISSING_NAME);
        }

        final RestDataservice restDataservice = KomodoJsonMarshaller.unmarshall( dataserviceJson, RestDataservice.class );
        final String jsonDataserviceName = restDataservice.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDataserviceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_JSON_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals( jsonDataserviceName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName, jsonDataserviceName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createDataservice", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a dataservice with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, dataserviceName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
            }

            // create new Dataservice
            return doAddDataservice( uow, uriInfo.getBaseUri(), mediaTypes, restDataservice );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR, dataserviceName);
        }
    }

    /**
     * Clone a DataService in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the dataservice name (cannot be empty)
     * @param newDataserviceName
     *        the new dataservice name (cannot be empty)
     * @return a JSON representation of the new dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataService
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CLONE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Clone a dataservice in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response cloneDataservice( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(
                                                 value = "Name of the data service",
                                                 required = true
                                       )
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
                                       @ApiParam(
                                                 value = "Name of the cloned data service",
                                                 required = true
                                       )
                                       final String newDataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_MISSING_NAME);
        }

        // Error if the new dataservice name is missing
        if ( StringUtils.isBlank( newDataserviceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_MISSING_NEW_NAME);
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = dataserviceName.equals( newDataserviceName );
        if ( namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_SAME_NAME_ERROR, newDataserviceName);
        }

        UnitOfWork uow1 = null;
        UnitOfWork uow2 = null;

        try {
            uow1 = createTransaction(principal, "cloneDataservice", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a dataservice with the supplied name.
            if ( getWorkspaceManager(uow1).hasChild( uow1, newDataserviceName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_ALREADY_EXISTS);
            }

            // get the source dataservice
            final KomodoObject kobject = getWorkspaceManager(uow1).getChild( uow1, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
            final Dataservice srcDataservice = getWorkspaceManager(uow1).resolve( uow1, kobject, Dataservice.class );
            String srcDescription = srcDataservice.getDescription(uow1);
            // Get the service Vdb from the source dataservice
            Vdb srcServiceVdb = srcDataservice.getServiceVdb(uow1);
                        
            // Create the new dataservice by cloning the source dataservice
            final Dataservice newDataservice = getWorkspaceManager(uow1).createDataservice( uow1, null, newDataserviceName);
            newDataservice.setDescription(uow1, srcDescription);
            newDataservice.setServiceVdb(uow1, null);
            String tgtServiceVdbName = newDataserviceName+SERVICE_VDB_SUFFIX;
            if(getWorkspaceManager(uow1).hasChild(uow1, tgtServiceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                KomodoObject svcVdbObj = getWorkspaceManager(uow1).getChild(uow1, tgtServiceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                svcVdbObj.remove(uow1);
            }

            // Create a new service VDB for the target dataservice
            KomodoObject tgtServiceVdbObj = getWorkspaceManager(uow1).createVdb(uow1, null, tgtServiceVdbName, tgtServiceVdbName);
            // Set owner property on the service vdb
            tgtServiceVdbObj.setProperty(uow1, DSB_PROP_OWNER, uow1.getUserName());
            Vdb tgtServiceVdb = Vdb.RESOLVER.resolve(uow1, tgtServiceVdbObj);
            
            // Copy source dataservice model info to the new target
            Model[] sourceModels = srcServiceVdb.getModels(uow1);
            for(Model sourceModel : sourceModels) {
                if( sourceModel.getModelType(uow1) == Model.Type.VIRTUAL ) {
                    // Add to the ServiceVdb a virtual model for the View
                    Model viewModel = tgtServiceVdb.addModel(uow1, SERVICE_VDB_VIEW_MODEL);
                    viewModel.setModelType(uow1, Type.VIRTUAL);
                    
                    // Get source model DDL
                    byte[] ddlBytes = sourceModel.export(uow1, new Properties());
                    String viewDdl = new String(ddlBytes);
                    
                    String srcViewName = dataserviceName+SERVICE_VDB_VIEW_SUFFIX;
                    String tgtViewName = newDataserviceName+SERVICE_VDB_VIEW_SUFFIX;
                    
                    viewDdl = viewDdl.replaceFirst(srcViewName, tgtViewName);

                    // Set DDL on new View Model
                    viewModel.setModelDefinition(uow1, viewDdl);
                } else if ( sourceModel.getModelType(uow1) == Model.Type.PHYSICAL ) {
                    String modelName = sourceModel.getName(uow1);
                    // Add model to target
                    Model targetModel = tgtServiceVdb.addModel(uow1, modelName);
                    targetModel.setModelType(uow1, Type.PHYSICAL);

                    // set the model DDL
                    Properties exportProps = new Properties();
                    byte[] bytes = sourceModel.export(uow1, exportProps);
                    String sourceDdl = new String(bytes);
                    targetModel.setModelDefinition(uow1, sourceDdl);
                    
                    ModelSource[] srcModelSources = sourceModel.getSources(uow1);
                    ModelSource srcModelSource = srcModelSources[0];
                    if(srcModelSource!=null) {
                        ModelSource tgtModelSource = targetModel.addSource(uow1, srcModelSource.getName(uow1));
                        tgtModelSource.setJndiName(uow1, srcModelSource.getJndiName(uow1));
                        tgtModelSource.setTranslatorName(uow1, srcModelSource.getTranslatorName(uow1));
                    }
                }
                
            }
            
            // -------------------------------------------------------------
            // Must commit the transaction before setting the service VDB
            // -------------------------------------------------------------
            final SynchronousCallback callback = ( SynchronousCallback )uow1.getCallback();
            uow1.commit();

            if ( !callback.await( 30, TimeUnit.SECONDS ) ) {
                // callback timeout occurred
                String errorMessage = Messages.getString( COMMIT_TIMEOUT, uow1.getName(), 30, TimeUnit.SECONDS );
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status( Status.INTERNAL_SERVER_ERROR )
                               .entity(responseEntity)
                               .build();
            }

            // -------------------------------------------------------------
            // Start a new transaction to complete the operation
            // -------------------------------------------------------------
            uow2 = createTransaction(principal, "cloneDataservice", false ); //$NON-NLS-1$

            // Set the service VDB for the cloned dataservice to the new targetServiceVdb
            newDataservice.setServiceVdb(uow2, tgtServiceVdb);
            
            final RestDataservice entity = entityFactory.create(newDataservice, uriInfo.getBaseUri(), uow2 );
            final Response response = commit( uow2, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow1 != null) && (uow1.getState() != State.ROLLED_BACK)) {
                uow1.rollback();
            }
            if ((uow2 != null) && (uow2.getState() != State.ROLLED_BACK)) {
                uow2.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_DATASERVICE_ERROR);
        }
    }

    /**
     * Sets the service VDB for the specified Dataservice using the specified table and sourceModel
     * The supplied table is used to generate the view DDL for the service vdb's view
     * The supplied modelSource is used to generate the sourceModel for the service vdb
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceUpdateAttributes
     *        the attributes for the update (cannot be empty)
     * @return a JSON representation of the updated dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VDB_FOR_SINGLE_TABLE )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Sets the data service's service vdb")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response setServiceVdbForSingleTable( final @Context HttpHeaders headers,
    		final @Context UriInfo uriInfo,
    		@ApiParam(
                      value = "" + 
                              "JSON specifying the service vdb:<br>" +
                              OPEN_PRE_TAG +
                              OPEN_BRACE + BR +
                              NBSP + "dataserviceName: \"name of the data service\"" + COMMA + BR +
                              NBSP + "tablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "modelSourcePath: \"/path/to/modelSource\"" + COMMA + BR +
                              NBSP + "columnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                              NBSP + "viewDdl: \"DDL for the service view\" [OPTIONAL]" + BR +
                              CLOSE_BRACE +
                              CLOSE_PRE_TAG,
                      required = true
            )
    		final String dataserviceUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the service update for single table view
        KomodoDataserviceUpdateAttributes attr;
        try {
        	attr = KomodoJsonMarshaller.unmarshall(dataserviceUpdateAttributes, KomodoDataserviceUpdateAttributes.class);
            Response response = checkDataserviceUpdateAttributesSingleTableView(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.DATASERVICE_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        // Inputs for constructing the Service VDB.  The paths should be obtained from the Attributes passed in.
        String dataserviceName = attr.getDataserviceName();
        // Error if the dataservice name is missing 
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_NAME);
        }
        String serviceVdbName = dataserviceName+SERVICE_VDB_SUFFIX;

        // Determine if viewDdl was supplied.  If so, it will override the supplied table info.
        String viewDdl = attr.getViewDdl();
        boolean viewDdlSupplied = false;
        if( !StringUtils.isBlank( viewDdl ) ) {
            viewDdlSupplied = true;
        }
        
        String absTablePath = attr.getTablePath();
        // Error if the viewTablePath is missing 
        if (StringUtils.isBlank( absTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH, dataserviceName);
        }
        
        String absServiceModelSourcePath = attr.getModelSourcePath();
        // Error if the modelSourcePath is missing 
        if (StringUtils.isBlank( absServiceModelSourcePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_MODELSOURCE_PATH, dataserviceName);
        }

        // Desired column names for the service (may be empty)
        List<String> columnNames = attr.getColumnNames();
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "setDataserviceServiceVDB", false ); //$NON-NLS-1$

            // Check for existence of Dataservice, Table and ModelSource before continuing...
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            
            // Check for existence of DataService
            final boolean exists = wkspMgr.hasChild( uow, dataserviceName );
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
            }
            final KomodoObject kobject = wkspMgr.getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
            final Dataservice dataservice = wkspMgr.resolve( uow, kobject, Dataservice.class );

            // Check for existence of Table
            List<KomodoObject> tableObjs = wkspMgr.getRepository().searchByPath(uow, absTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absTablePath);
            }
            Table sourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Check for existence of ModelSource
            List<KomodoObject> modelObjs = wkspMgr.getRepository().searchByPath(uow, absServiceModelSourcePath);
            if( modelObjs.isEmpty() || !ModelSource.RESOLVER.resolvable(uow, modelObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MODEL_SOURCE_DNE, absServiceModelSourcePath);
            }
            ModelSource svcModelSource = ModelSource.RESOLVER.resolve(uow, modelObjs.get(0));

            // Find the service VDB definition for this Dataservice.  If one exists already, it is replaced.
            dataservice.setServiceVdb(uow,null);
            if(wkspMgr.hasChild(uow, serviceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            	KomodoObject svcVdbObj = wkspMgr.getChild(uow, serviceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            	svcVdbObj.remove(uow);
            }
            KomodoObject vdbObj = wkspMgr.createVdb(uow, null, serviceVdbName, serviceVdbName);
            // Set owner property on the service vdb
            vdbObj.setProperty(uow, DSB_PROP_OWNER, uow.getUserName());
            Vdb serviceVdb = Vdb.RESOLVER.resolve(uow, vdbObj);
            
            // Add to the ServiceVdb a virtual model for the View
            Model viewModel = serviceVdb.addModel(uow, SERVICE_VDB_VIEW_MODEL);
            viewModel.setModelType(uow, Type.VIRTUAL);
            
            // If viewDdl wasn't supplied, generate it using the specified table, then set the viewModel content.
            if(!viewDdlSupplied) {
                viewDdl = ViewDdlBuilder.getODataViewDdl(uow, dataserviceName+SERVICE_VDB_VIEW_SUFFIX, sourceTable, columnNames);
            }
            viewModel.setModelDefinition(uow, viewDdl);

        	// Add a physical model to the VDB for the sources
        	// physicalModelName ==> sourceVDBName
        	// physicalModelSourceName ==> sourceVDBModelName
        	String physicalModelName = svcModelSource.getParent(uow).getParent(uow).getName(uow);
        	String physicalModelSourceName = svcModelSource.getParent(uow).getName(uow);
        	
        	Model sourceModel = serviceVdb.addModel(uow, physicalModelName);
        	sourceModel.setModelType(uow, Type.PHYSICAL);

            // The source model DDL contains the table DDL only.  This limits the source metadata which is loaded on deployment.
            Properties exportProps = new Properties();
            exportProps.put( ExportConstants.EXCLUDE_TABLE_CONSTRAINTS_KEY, true );
        	byte[] bytes = sourceTable.export(uow, exportProps);
        	String tableString = new String(bytes);
            sourceModel.setModelDefinition(uow, tableString);
            
        	// Add a ModelSource of same name to the physical model and set its Jndi and translator
        	ModelSource modelSource = sourceModel.addSource(uow, physicalModelSourceName);
        	modelSource.setJndiName(uow, svcModelSource.getJndiName(uow));
        	modelSource.setTranslatorName(uow, svcModelSource.getTranslatorName(uow));     
            
            // Set the service VDB on the dataservice
            dataservice.setServiceVdb(uow, serviceVdb);

            KomodoStatusObject kso = new KomodoStatusObject("Update DataService Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully updated"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_SET_SERVICE_ERROR);
        }
    }
    
    /**
     * Get the generated View DDL for the supplied attributes
     * The supplied table is used to generate the view DDL
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceUpdateAttributes
     *        the attributes for generating the DDL (cannot be empty)
     * @return a JSON representation of the DDL (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error generating the DDL
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Gets generated View DDL using parameters provided in the request body")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getServiceViewDdlForSingleTable( final @Context HttpHeaders headers,
                                                     final @Context UriInfo uriInfo,
                                                     @ApiParam(
                                                               value = "" + 
                                                                       "JSON parameters:<br>" +
                                                                       OPEN_PRE_TAG +
                                                                       OPEN_BRACE + BR +
                                                                       NBSP + "dataserviceName: \"name of the data service\"" + COMMA + BR +
                                                                       NBSP + "tablePath: \"/path/to/table\"" + COMMA + BR +
                                                                       NBSP + "modelSourcePath: \"/path/to/modelSource\"" + COMMA + BR +
                                                                       NBSP + "columnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                                                                       NBSP + "viewDdl: \"DDL for the service view\" [OPTIONAL]" + BR +
                                                                       CLOSE_BRACE +
                                                                       CLOSE_PRE_TAG,
                                                               required = true
                                                     )
                                                     final String dataserviceUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the service update for single table view
        KomodoDataserviceUpdateAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(dataserviceUpdateAttributes, KomodoDataserviceUpdateAttributes.class);
            Response response = checkDataserviceUpdateAttributesGetSingleTableDdl(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.DATASERVICE_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        // Inputs for constructing the Service VDB.  The paths should be obtained from the Attributes passed in.
        String dataserviceName = attr.getDataserviceName();
        // Error if the dataservice name is missing 
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_NAME);
        }
        
        String absTablePath = attr.getTablePath();
        // Error if the viewTablePath is missing 
        if (StringUtils.isBlank( absTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH, dataserviceName);
        }
        
        // Desired column names for the service (may be empty)
        List<String> columnNames = attr.getColumnNames();
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "getDataserviceServiceVDB", true ); //$NON-NLS-1$

            // Check for existence of Dataservice, Table and ModelSource before continuing...
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            
            // Check for existence of Table
            List<KomodoObject> tableObjs = wkspMgr.getRepository().searchByPath(uow, absTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absTablePath);
            }
            Table sourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));
            
            // Generate the ViewDDL using the specified table, then set the viewModel content.
            String viewDdl = ViewDdlBuilder.getODataViewDdl(uow, dataserviceName+SERVICE_VDB_VIEW_SUFFIX, sourceTable, columnNames);

            // Add info for the raw view ddl
            RestDataserviceViewInfo viewInfo = new RestDataserviceViewInfo();
            viewInfo.setInfoType(RestDataserviceViewInfo.DDL_INFO);
            viewInfo.setViewDdl(viewDdl);

            return commit(uow, mediaTypes, viewInfo);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_SET_SERVICE_ERROR);
        }
    }
    
    /**
     * Sets the service VDB for the specified Dataservice using the specified tables and sourceModels
     * The supplied table is used to generate the view DDL for the service vdb's view
     * The supplied modelSource is used to generate the sourceModel for the service vdb
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceUpdateAttributes
     *        the attributes for the update (cannot be empty)
     * @return a JSON representation of the updated dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VDB_FOR_JOIN_TABLES )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Sets the dataservice vdb using parameters provided in the request body")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response setServiceVdbForTwoTables( final @Context HttpHeaders headers,
            final @Context UriInfo uriInfo,
            @ApiParam(
                      value = "" + 
                              "JSON parameters:<br>" +
                              OPEN_PRE_TAG +
                              OPEN_BRACE + BR +
                              NBSP + "dataserviceName: \"name of the data service\"" + COMMA + BR +
                              NBSP + "tablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "rhTablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "modelSourcePath: \"/path/to/modelSource\"" + COMMA + BR +
                              NBSP + "rhModelSourcePath: \"/path/to/modelSource\"" + COMMA + BR +
                              NBSP + "columnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                              NBSP + "rhColumnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                              NBSP + "viewDdl: \"DDL for the service view\" [OPTIONAL]" + COMMA + BR +
                              NBSP + "joinType: \"Type of join\"" + COMMA + BR +
                              NBSP + "criteriaPredicates: []" + BR +
                              CLOSE_BRACE +
                              CLOSE_PRE_TAG,
                      required = true
            )
            final String dataserviceUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the service update
        KomodoDataserviceUpdateAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(dataserviceUpdateAttributes, KomodoDataserviceUpdateAttributes.class);
            Response response = checkDataserviceUpdateAttributesJoinView(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.DATASERVICE_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        // Inputs for constructing the Service VDB.  The paths should be obtained from the Attributes passed in.
        String dataserviceName = attr.getDataserviceName();
        // Error if the dataservice name is missing 
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_NAME);
        }
        String serviceVdbName = dataserviceName+SERVICE_VDB_SUFFIX;
        
        String absLhTablePath = attr.getTablePath();
        String absRhTablePath = attr.getRhTablePath();
        // Error if the viewTablePath is missing 
        if (StringUtils.isBlank( absLhTablePath ) || StringUtils.isBlank( absRhTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH, dataserviceName);
        }
        
        String absLhModelSourcePath = attr.getModelSourcePath();
        String absRhModelSourcePath = attr.getRhModelSourcePath();
        // Error if the modelSourcePath is missing 
        if (StringUtils.isBlank( absLhModelSourcePath ) || StringUtils.isBlank( absRhModelSourcePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_MODELSOURCE_PATH, dataserviceName);
        }
        // Determine if the tables are from the same model
        boolean sameLeftAndRightModel = absLhModelSourcePath.trim().equals(absRhModelSourcePath.trim());

        // Determine if viewDdl was supplied.  If so, it will override the supplied table info.
        String viewDdl = attr.getViewDdl();
        boolean viewDdlSupplied = false;
        if( !StringUtils.isBlank( viewDdl ) ) {
            viewDdlSupplied = true;
        }
        
        // JoinType
        String joinType = attr.getJoinType();
        // Error if the join type is missing
        if (!viewDdlSupplied && StringUtils.isBlank( joinType )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_JOIN_TYPE, dataserviceName);
        }

        List<Map<String,String>> predicateMaps = attr.getCriteriaPredicates();
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>(predicateMaps.size());
        for(Map<String,String> predicateMap : predicateMaps) {
            criteriaPredicates.add(new ViewBuilderCriteriaPredicate(predicateMap));
        }

        // Desired column names for the service (may be empty)
        List<String> lhColumnNames = attr.getColumnNames();
        List<String> rhColumnNames = attr.getRhColumnNames();
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "setDataserviceServiceVDB", false ); //$NON-NLS-1$

            // -------------------------------------------------------------------------------------
            // Check for existence of necessary objects
            // -------------------------------------------------------------------------------------
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            
            // Check for existence of DataService
            final boolean exists = wkspMgr.hasChild( uow, dataserviceName );
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
            }
            final KomodoObject kobject = wkspMgr.getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
            final Dataservice dataservice = wkspMgr.resolve( uow, kobject, Dataservice.class );

            // Check for existence of LH Table
            List<KomodoObject> tableObjs = wkspMgr.getRepository().searchByPath(uow, absLhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absLhTablePath);
            }
            Table lhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Check for existence of RH Table
            tableObjs = wkspMgr.getRepository().searchByPath(uow, absRhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absRhTablePath);
            }
            Table rhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Check for existence of LH ModelSource
            List<KomodoObject> modelObjs = wkspMgr.getRepository().searchByPath(uow, absLhModelSourcePath);
            if( modelObjs.isEmpty() || !ModelSource.RESOLVER.resolvable(uow, modelObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MODEL_SOURCE_DNE, absLhModelSourcePath);
            }
            ModelSource lhModelSource = ModelSource.RESOLVER.resolve(uow, modelObjs.get(0));

            ModelSource rhModelSource = lhModelSource;
            if(!sameLeftAndRightModel) {
                // Check for existence of RH ModelSource
                modelObjs = wkspMgr.getRepository().searchByPath(uow, absRhModelSourcePath);
                if( modelObjs.isEmpty() || !ModelSource.RESOLVER.resolvable(uow, modelObjs.get(0)) ) {
                    return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MODEL_SOURCE_DNE, absRhModelSourcePath);
                }
                rhModelSource = ModelSource.RESOLVER.resolve(uow, modelObjs.get(0));
            }

            // ----------------------------------------------------------------------------------------------
            // Find the service VDB definition for this Dataservice.  If one exists already, it is replaced.
            // ----------------------------------------------------------------------------------------------
            dataservice.setServiceVdb(uow,null);
            if(wkspMgr.hasChild(uow, serviceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                KomodoObject svcVdbObj = wkspMgr.getChild(uow, serviceVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                svcVdbObj.remove(uow);
            }
            KomodoObject vdbObj = wkspMgr.createVdb(uow, null, serviceVdbName, serviceVdbName);
            // Set owner property on the service vdb
            vdbObj.setProperty(uow, DSB_PROP_OWNER, uow.getUserName());
            Vdb serviceVdb = Vdb.RESOLVER.resolve(uow, vdbObj);
            
            // ----------------------------------------------------------------
            // Add to the ServiceVdb a virtual model for the View
            // ----------------------------------------------------------------
            Model viewModel = serviceVdb.addModel(uow, SERVICE_VDB_VIEW_MODEL);
            viewModel.setModelType(uow, Type.VIRTUAL);
            
            // If view DDL not supplied, generate it using the specified tables and info, then set the viewModel content.
            if(!viewDdlSupplied) {
                viewDdl = ViewDdlBuilder.getODataViewJoinDdl(uow, dataserviceName+SERVICE_VDB_VIEW_SUFFIX, 
                                                                  lhSourceTable, LH_TABLE_ALIAS, lhColumnNames, 
                                                                  rhSourceTable, RH_TABLE_ALIAS, rhColumnNames, 
                                                                  joinType, criteriaPredicates);
            }
            viewModel.setModelDefinition(uow, viewDdl);

            // --------------------------------------------------
            // Add physical model for lh modelSource
            // --------------------------------------------------
            // Add a physical model to the VDB for the sources
            // physicalModelName ==> sourceVDBName
            // physicalModelSourceName ==> sourceVDBModelName
            String lhPhysicalModelName = lhModelSource.getParent(uow).getParent(uow).getName(uow);
            String lhPhysicalModelSourceName = lhModelSource.getParent(uow).getName(uow);

            Model lhPhysicalModel = serviceVdb.addModel(uow, lhPhysicalModelName);
            lhPhysicalModel.setModelType(uow, Type.PHYSICAL);

            // The source model DDL contains the table DDL only.  This limits the source metadata which is loaded on deployment.
            Properties exportProps = new Properties();
            exportProps.put( ExportConstants.EXCLUDE_TABLE_CONSTRAINTS_KEY, true );
            // LH Table DDL
            byte[] bytes = lhSourceTable.export(uow, exportProps);
            String lhTableDdl = new String(bytes);
            // RH Table DDL
            bytes = rhSourceTable.export(uow, exportProps);
            String rhTableDdl = new String(bytes);
            
            // If same LH and RH Model, add the RH table DDL as well
            if(sameLeftAndRightModel) {
                lhPhysicalModel.setModelDefinition(uow, lhTableDdl+rhTableDdl);
            } else {
                lhPhysicalModel.setModelDefinition(uow, lhTableDdl);
            }
            
            // Add a ModelSource of same name to the physical model and set its Jndi and translator
            ModelSource lhPhysicalModelSource = lhPhysicalModel.addSource(uow, lhPhysicalModelSourceName);
            lhPhysicalModelSource.setJndiName(uow, lhModelSource.getJndiName(uow));
            lhPhysicalModelSource.setTranslatorName(uow, lhModelSource.getTranslatorName(uow));
            
            // --------------------------------------------------
            // Add physical model for rh modelSource
            // - (dont add duplicate if same as left)
            // --------------------------------------------------
            if(!sameLeftAndRightModel) {
                // Add a physical model to the VDB for the sources
                // physicalModelName ==> sourceVDBName
                // physicalModelSourceName ==> sourceVDBModelName
                String rhPhysicalModelName = rhModelSource.getParent(uow).getParent(uow).getName(uow);
                String rhPhysicalModelSourceName = rhModelSource.getParent(uow).getName(uow);

                Model rhPhysicalModel = serviceVdb.addModel(uow, rhPhysicalModelName);
                rhPhysicalModel.setModelType(uow, Type.PHYSICAL);

                rhPhysicalModel.setModelDefinition(uow, rhTableDdl);
                
                // Add a ModelSource of same name to the physical model and set its Jndi and translator
                ModelSource rhPhysicalModelSource = rhPhysicalModel.addSource(uow, rhPhysicalModelSourceName);
                rhPhysicalModelSource.setJndiName(uow, rhModelSource.getJndiName(uow));
                rhPhysicalModelSource.setTranslatorName(uow, rhModelSource.getTranslatorName(uow));     
            }
            
            // -------------------------------------------
            // Set the service VDB on the dataservice
            // -------------------------------------------
            dataservice.setServiceVdb(uow, serviceVdb);

            KomodoStatusObject kso = new KomodoStatusObject("Update DataService Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully updated"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_SET_SERVICE_ERROR);
        }
    }
    
    /**
     * Get the generated View DDL for the supplied attributes
     * The supplied tables and join details are used to generate the view DDL
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceUpdateAttributes
     *        the attributes for generating the DDL (cannot be empty)
     * @return a JSON representation of the generated DDL (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error generating the DDL
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Gets generated View DDL using parameters provided in the request body")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getServiceViewDdlForTwoTables( final @Context HttpHeaders headers,
            final @Context UriInfo uriInfo,
            @ApiParam(
                      value = "" + 
                              "JSON parameters:<br>" +
                              OPEN_PRE_TAG +
                              OPEN_BRACE + BR +
                              NBSP + "dataserviceName: \"name of the data service\"" + COMMA + BR +
                              NBSP + "tablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "rhTablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "columnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                              NBSP + "rhColumnNames: {name1, name2, ...} [OPTIONAL]" + COMMA + BR +
                              NBSP + "joinType: \"Type of join\"" + COMMA + BR +
                              NBSP + "criteriaPredicates: []" + BR +
                              CLOSE_BRACE +
                              CLOSE_PRE_TAG,
                      required = true
            )
            final String dataserviceUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the service update
        KomodoDataserviceUpdateAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(dataserviceUpdateAttributes, KomodoDataserviceUpdateAttributes.class);
            Response response = checkDataserviceUpdateAttributesGetJoinDdl(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.DATASERVICE_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        // Inputs for constructing the Service VDB.  The paths should be obtained from the Attributes passed in.
        String dataserviceName = attr.getDataserviceName();
        // Error if the dataservice name is missing 
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_NAME);
        }

        String absLhTablePath = attr.getTablePath();
        String absRhTablePath = attr.getRhTablePath();
        // Error if the viewTablePath is missing 
        if (StringUtils.isBlank( absLhTablePath ) || StringUtils.isBlank( absRhTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH, dataserviceName);
        }
        
        // JoinType
        String joinType = attr.getJoinType();
        // Error if the join type is missing
        if (StringUtils.isBlank( joinType )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_JOIN_TYPE, dataserviceName);
        }

        List<Map<String,String>> predicateMaps = attr.getCriteriaPredicates();
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>(predicateMaps.size());
        for(Map<String,String> predicateMap : predicateMaps) {
            criteriaPredicates.add(new ViewBuilderCriteriaPredicate(predicateMap));
        }

        // Desired column names for the service (may be empty)
        List<String> lhColumnNames = attr.getColumnNames();
        List<String> rhColumnNames = attr.getRhColumnNames();
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "getViewDdl", true ); //$NON-NLS-1$

            // -------------------------------------------------------------------------------------
            // Check for existence of necessary objects
            // -------------------------------------------------------------------------------------
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            
            // Check for existence of LH Table
            List<KomodoObject> tableObjs = wkspMgr.getRepository().searchByPath(uow, absLhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absLhTablePath);
            }
            Table lhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Check for existence of RH Table
            tableObjs = wkspMgr.getRepository().searchByPath(uow, absRhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absRhTablePath);
            }
            Table rhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Generate the ViewDDL using the specified tables, then set the viewModel content.
            String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(uow, dataserviceName+SERVICE_VDB_VIEW_SUFFIX, 
                                                                lhSourceTable, LH_TABLE_ALIAS, lhColumnNames, 
                                                                rhSourceTable, RH_TABLE_ALIAS, rhColumnNames, 
                                                                joinType, criteriaPredicates);

            // Add info for the raw view ddl
            RestDataserviceViewInfo viewInfo = new RestDataserviceViewInfo();
            viewInfo.setInfoType(RestDataserviceViewInfo.DDL_INFO);
            viewInfo.setViewDdl(viewDdl);

            return commit(uow, mediaTypes, viewInfo);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_SET_SERVICE_ERROR);
        }
    }
    
    /**
     * Get the criteria predicates for the supplied tables
     * The supplied tables are examined for PK-FK relationships.  If a relationship is found, it is used to generate criteria.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceUpdateAttributes
     *        the attributes for generating the criteria predicates (cannot be empty)
     * @return a JSON representation of the generated criteria predicates (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error generating the criteria
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CRITERIA_FOR_JOIN_TABLES )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Generates join criteria using table parameters provided in the request body")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getJoinCriteriaForTables( final @Context HttpHeaders headers,
            final @Context UriInfo uriInfo,
            @ApiParam(
                      value = "" + 
                              "JSON parameters:<br>" +
                              OPEN_PRE_TAG +
                              OPEN_BRACE + BR +
                              NBSP + "tablePath: \"/path/to/table\"" + COMMA + BR +
                              NBSP + "rhTablePath: \"/path/to/table\"" + COMMA + BR +
                              CLOSE_BRACE +
                              CLOSE_PRE_TAG,
                      required = true
            )
            final String dataserviceUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the service update
        KomodoDataserviceUpdateAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(dataserviceUpdateAttributes, KomodoDataserviceUpdateAttributes.class);
            Response response = checkDataserviceUpdateAttributesGetJoinCriteria(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.DATASERVICE_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        String absLhTablePath = attr.getTablePath();
        String absRhTablePath = attr.getRhTablePath();
        // Error if the either tablePath is missing 
        if (StringUtils.isBlank( absLhTablePath ) || StringUtils.isBlank( absRhTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_GET_JOIN_MISSING_TABLEPATH);
        }
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "getJoinCriteria", true ); //$NON-NLS-1$

            // -------------------------------------------------------------------------------------
            // Check for existence of necessary objects
            // -------------------------------------------------------------------------------------
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            
            // Check for existence of LH Table
            List<KomodoObject> tableObjs = wkspMgr.getRepository().searchByPath(uow, absLhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absLhTablePath);
            }
            Table lhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Check for existence of RH Table
            tableObjs = wkspMgr.getRepository().searchByPath(uow, absRhTablePath);
            if( tableObjs.isEmpty() || !Table.RESOLVER.resolvable(uow, tableObjs.get(0)) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SOURCE_TABLE_DNE, absRhTablePath);
            }
            Table rhSourceTable = Table.RESOLVER.resolve(uow, tableObjs.get(0));

            // Generate join criteria predicates given the left and right source tables.  (Examines the tables for PK-FK relationships).
            RestDataserviceViewInfo viewInfo = buildJoinCriteria(uow, lhSourceTable, rhSourceTable);

            return commit(uow, mediaTypes, viewInfo);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_SET_SERVICE_ERROR);
        }
    }
    
    /**
     * Update a Dataservice in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the dataservice name (cannot be empty)
     * @param dataserviceJson
     *        the dataservice JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @PUT
    @Path( StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a dataservice in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateDataservice( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(
                                                 value = "Name of the data service to be updated",
                                                 required = true
                                       )
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
                                       @ApiParam(
                                                 value = "" + 
                                                         "JSON of the data service properties to update:<br>" +
                                                         OPEN_PRE_TAG +
                                                         OPEN_BRACE + BR +
                                                         NBSP + "keng\\_\\_id: \"id of the data service\"" + COMMA + BR +
                                                         NBSP + OPEN_PRE_CMT + "(identical to dataserviceName parameter)" + CLOSE_PRE_CMT + BR + BR +
                                                         NBSP + "tko__description: \"the description\"" + BR +
                                                         CLOSE_BRACE +
                                                         CLOSE_PRE_TAG,
                                                 required = true
                                       )
                                       final String dataserviceJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing 
        if (StringUtils.isBlank( dataserviceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_MISSING_NAME);
        }


        final RestDataservice restDataservice = KomodoJsonMarshaller.unmarshall( dataserviceJson, RestDataservice.class );
        final String jsonDataserviceName = restDataservice.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDataserviceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_JSON_MISSING_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateDataservice", false ); //$NON-NLS-1$

            final boolean exists = getWorkspaceManager(uow).hasChild( uow, dataserviceName );
            // Error if the specified service does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
            }

            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
            final Dataservice dataservice = getWorkspaceManager(uow).resolve( uow, kobject, Dataservice.class );

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, dataservice, restDataservice);

            // rename if names did not match
            final boolean namesMatch = dataserviceName.equals( jsonDataserviceName );
            if ( !namesMatch ) {
                dataservice.rename( uow, jsonDataserviceName );
            }

            KomodoProperties properties = new KomodoProperties();
            final RestDataservice entity = entityFactory.create(dataservice, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateDataservice: dataservice '{0}' entity was updated", dataservice.getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_UPDATE_DATASERVICE_ERROR);
        }
    }

    private Response doAddDataservice( final UnitOfWork uow,
                                       final URI baseUri,
                                       final List<MediaType> mediaTypes,
                                       final RestDataservice restDataservice ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restDataservice != null );

        final String dataserviceName = restDataservice.getId();
        try {
            final Dataservice dataservice = getWorkspaceManager(uow).createDataservice( uow, null, dataserviceName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, dataservice, restDataservice);

            final RestDataservice entity = entityFactory.create(dataservice, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR, dataserviceName ), e );
        }
    }

    // Sets Dataservice properties using the supplied RestDataservice object
    private void setProperties(final UnitOfWork uow, Dataservice dataService, RestDataservice restDataService) throws KException {
        // 'New' = requested RestDataservice properties
        String newDescription = restDataService.getDescription();

        // 'Old' = current Dataservice properties
        String oldDescription = dataService.getDescription(uow);

        // Description
        if ( !StringUtils.equals(newDescription, oldDescription) ) {
            dataService.setDescription( uow, newDescription );
        }
    }

    /**
     * Delete the specified Dataservice from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the name of the data service to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{dataserviceName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a dataservice from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteDataservice( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(
                                                 value = "Name of the data service to be deleted",
                                                 required = true
                                       )
                                       final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeDataserviceFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            final boolean exists = wkspMgr.hasChild( uow, dataserviceName );
            // Error if the specified service does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
            }

            KomodoObject dsKobject = wkspMgr.getChild(uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE);
            final Dataservice dataservice = wkspMgr.resolve( uow, dsKobject, Dataservice.class );

            // Delete the Dataservice serviceVDB if found
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            if(serviceVdb!=null) {
                wkspMgr.delete(uow, serviceVdb);
            }

            // Delete the Dataservice
            wkspMgr.delete(uow, dataservice);
            
            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR);
        }
    }
    
    /*
     * Checks the supplied attributes for single table view
     *  - dataserviceName, table path and modelSource path are required
     *  - columnNames is optional
     */
    private Response checkDataserviceUpdateAttributesSingleTableView(KomodoDataserviceUpdateAttributes attr,
                                                                     List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getDataserviceName() == null || attr.getTablePath() == null || attr.getModelSourcePath() == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }

    /*
     * Checks the supplied attributes for getting the single table DDL
     *  - dataserviceName and table path are required
     *  - columnNames is optional
     */
    private Response checkDataserviceUpdateAttributesGetSingleTableDdl(KomodoDataserviceUpdateAttributes attr,
                                                                       List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getDataserviceName() == null || attr.getTablePath() == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }

    /*
     * Checks the supplied attributes for join table view
     *  - dataserviceName, modelSourcePath, rhModelSourcePath, tablePath, rhTablePath are required
     *  - Either joinType, lhJoinColumn, rhJoinColumn must be supplied -OR- view Ddl
     *  - columnNames and rhColumnNames are optional
     */
    private Response checkDataserviceUpdateAttributesJoinView(KomodoDataserviceUpdateAttributes attr,
                                                              List<MediaType> mediaTypes) throws Exception {

        // service name, modelSourcePath, rhModelSourcePath must not be null
        if (attr == null || attr.getDataserviceName() == null || attr.getModelSourcePath() == null
                         || attr.getRhModelSourcePath() == null || attr.getTablePath() == null || attr.getRhTablePath() == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }
        
        // Either the join tables and info must be non null, or the view Ddl must be supplied
        if( ( attr.getJoinType() == null || 
              attr.getCriteriaPredicates() == null ) && (attr.getViewDdl() == null) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }
    
    /*
     * Checks the supplied attributes for getting the join DDL
     *  - dataserviceName, tablePath, rhTablePath, joinType are required
     *  - columnNames and rhColumnNames are optional
     */
    private Response checkDataserviceUpdateAttributesGetJoinDdl(KomodoDataserviceUpdateAttributes attr,
                                                                List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getDataserviceName() == null || attr.getJoinType() == null 
                         || attr.getTablePath() == null || attr.getRhTablePath() == null ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }

    /*
     * Checks the supplied attributes for getting the join criteria for two tables.
     *  - tablePath, rhTablePath are required
     */
    private Response checkDataserviceUpdateAttributesGetJoinCriteria(KomodoDataserviceUpdateAttributes attr,
                                                                     List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getTablePath() == null || attr.getRhTablePath() == null ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice of the connections being retrieved (cannot be empty)
     * @return the JSON representation of the Connections (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Dataservice connections
     *         or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER +
                   StringConstants.FORWARD_SLASH + V1Constants.CONNECTIONS_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find a dataservice's connections ", response = RestDataservice.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnections( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(
                                              value = "Name of the data service containing the required connections",
                                              required = true
                                    )
                                    final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataservice", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            Connection[] connections = dataservice.getConnections(uow);
            List<RestConnection> restConnections = new ArrayList<>(connections.length);
            for (Connection connection : connections) {
                RestConnection entity = entityFactory.create(connection, uriInfo.getBaseUri(), uow);
                restConnections.add(entity);
                LOGGER.debug("getConnections:Connections from Dataservice '{0}' entity was constructed", dataserviceName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restConnections);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_CONNECTIONS_ERROR, dataserviceName);
        }
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice (cannot be empty)
     * @return the JSON representation of the Drivers (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Dataservice drivers
     *         or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER +
                   StringConstants.FORWARD_SLASH + V1Constants.DRIVERS_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Find a dataservice's drivers ", response = RestConnectionDriver.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDrivers( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(
                                              value = "Name of the data service containing the required drivers",
                                              required = true
                                    )
                                    final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataservice", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            Driver[] drivers = dataservice.getDrivers(uow);
            List<RestConnectionDriver> restDrivers = new ArrayList<>(drivers.length);
            for (Driver driver : drivers) {
                ConnectionDriver aDriver = new ConnectionDriver(driver.getName(uow),null);
                RestConnectionDriver entity = new RestConnectionDriver(aDriver);
                restDrivers.add(entity);
                LOGGER.debug("getDrivers:Drivers from Dataservice '{0}' entity was constructed", dataserviceName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restDrivers);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_DRIVERS_ERROR, dataserviceName);
        }
    }
    
    /**
     * Find source VDBs in the workspace whose jndi matches the dataservice source model jndi
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER + StringConstants.FORWARD_SLASH + V1Constants.SOURCE_VDB_MATCHES)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find workspace source VDB matches for a Dataservice", response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSourceVdbsForDataService( final @Context HttpHeaders headers,
                                                 final @Context UriInfo uriInfo,
                                                 @ApiParam(
                                                           value = "Name of the data service containing the required source vdbs",
                                                           required = true
                                                 )
                                                 final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getSourceVdbsForDataservice", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            // They physical source models are named with the name of the service source VDB
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            List<String> sourceVdbNames = new ArrayList<String>();
            Model[] models = serviceVdb.getModels(uow);
            for(Model model : models) {
                if(model.getModelType(uow) == Model.Type.PHYSICAL) {
                    sourceVdbNames.add(model.getName(uow));
                }
            }
            
            List<Vdb> sourceVdbs = new ArrayList<Vdb>();
            if(!sourceVdbNames.isEmpty()) {
                // Add Vdbs with the source vdb names
                WorkspaceManager wsMgr = getWorkspaceManager(uow);
                Vdb[] wsVdbs = wsMgr.findVdbs(uow);
                for(Vdb wsVdb : wsVdbs) {
                    if(sourceVdbNames.contains(wsVdb.getName(uow))) {
                        sourceVdbs.add(wsVdb);
                    }
                }
            }
            
            final List< RestVdb > entities = new ArrayList< >();
            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
            for ( final Vdb vdb : sourceVdbs ) {
                RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
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

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_FIND_SOURCE_VDB_ERROR, dataserviceName);
        }
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice (cannot be empty)
     * @return the info response (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Dataservice or constructing the info response.
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER + StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VIEW_INFO)
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "retrieve the service view information for a dataservice")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getServiceViewInfoForDataService( final @Context HttpHeaders headers,
                                                      final @Context UriInfo uriInfo,
                                                      @ApiParam(
                                                                value = "Name of the required data service",
                                                                required = true
                                                      )
                                                      final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getServiceViewInfoForDataService", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            // Get the View Query Expression and extract the table names from the query
            String viewSql = null;
            String viewDdl = null;
            
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            Model[] models = serviceVdb.getModels(uow);
            List<Table> srcTables = new ArrayList<Table>();
            for(Model model : models) {
                // Get the view DDL from the virtual model
                if(model.getModelType(uow) == Model.Type.VIRTUAL) {
                    View[] views = model.getViews(uow);
                    viewSql = views[0].getQueryExpression(uow);
                    byte[] ddlBytes = model.export(uow, new Properties());
                    if (ddlBytes == null)
                        viewDdl = EMPTY_STRING;
                    else
                        viewDdl = new String(ddlBytes);
                // get the tables for each physical model
                } else if(model.getModelType(uow) == Model.Type.PHYSICAL) {
                    // saves mapping of table to source vdb name (physical models were named using ServiceSourceVdb name)
                    Table[] tables = model.getTables(uow);
                    for(Table table : tables) {
                        srcTables.add(table);
                    }
                }
            }
            
            // Generate source table info for each view table
            final List< RestDataserviceViewInfo > viewInfos = new ArrayList<RestDataserviceViewInfo>();
            Map<String, List<String>> tableColumnMap = getTableColumnNameMap(viewSql);
            
            // Determine LHS vs RHS for SQL tables (the map keys are aliased for joins)
            Set<String> sqlTables = tableColumnMap.keySet();
            String leftSqlTableName = StringConstants.EMPTY_STRING;
            String rightSqlTableName = StringConstants.EMPTY_STRING;
            boolean leftSqlTableAliased = false;
            boolean rightSqlTableAliased = false;
            for(String sqlTable : sqlTables) {
                // Left aliased
                if(sqlTable.endsWith(StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+LH_TABLE_ALIAS)) {
                    int aliasIndx = sqlTable.indexOf(StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+LH_TABLE_ALIAS);
                    leftSqlTableName = sqlTable.substring(0,aliasIndx);
                    leftSqlTableAliased = true;
                // Right aliased
                } else if(sqlTable.endsWith(StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+RH_TABLE_ALIAS)) {
                    int aliasIndx = sqlTable.indexOf(StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+RH_TABLE_ALIAS);
                    rightSqlTableName = sqlTable.substring(0,aliasIndx);
                    rightSqlTableAliased = true;
                // No alias - left
                } else {
                    leftSqlTableName = sqlTable;
                }
            }
            
            // Create the view infos for the left and right tables
            int nTable = 0;
            boolean hasUnmatchedTable = false;
            Table lhTable = null;
            Table rhTable = null;
            for (Table srcTable : srcTables) {
                RestDataserviceViewInfo viewInfo = new RestDataserviceViewInfo();
                
                String tblName = srcTable.getName(uow);
                String tblSrc = srcTable.getParent(uow).getName(uow);
                String qualifiedTblName = tblSrc + StringConstants.DOT + srcTable.getName(uow);
                // Set LH vs RH on info
                if(qualifiedTblName.equals(leftSqlTableName)) {
                    viewInfo.setInfoType(RestDataserviceViewInfo.LH_TABLE_INFO);
                    lhTable = srcTable;
                } else if(qualifiedTblName.equals(rightSqlTableName)){
                    viewInfo.setInfoType(RestDataserviceViewInfo.RH_TABLE_INFO);
                    rhTable = srcTable;
                // If sql table names could not be matched to dataservice source, just set LH/RH by index and track unmatchedTable state
                } else if(nTable==0) {
                    viewInfo.setInfoType(RestDataserviceViewInfo.LH_TABLE_INFO);
                    lhTable = srcTable;
                    hasUnmatchedTable = true;
                } else if(nTable==1) {
                    viewInfo.setInfoType(RestDataserviceViewInfo.RH_TABLE_INFO);
                    rhTable = srcTable;
                    hasUnmatchedTable = true;
                }
                // Source VDB and table
                viewInfo.setSourceVdbName(tblSrc);
                viewInfo.setTableName(tblName);
                
                String mapKey = qualifiedTblName;
                if(viewInfo.getInfoType()!=null) {
                    if(viewInfo.getInfoType().equals(RestDataserviceViewInfo.LH_TABLE_INFO) && leftSqlTableAliased) {
                        mapKey = mapKey+StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+LH_TABLE_ALIAS;
                    } else if(viewInfo.getInfoType().equals(RestDataserviceViewInfo.RH_TABLE_INFO) && rightSqlTableAliased) {
                        mapKey = mapKey+StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+RH_TABLE_ALIAS;
                    } 
                }
                List<String> colsForTable = tableColumnMap.get(mapKey);
                if(colsForTable!=null && !colsForTable.isEmpty()) {
                    viewInfo.setColumnNames(colsForTable);
                }
                viewInfos.add(viewInfo);
                nTable++;
            }
            
            // Get criteria info for the view - if two tables were found
            RestDataserviceViewInfo criteriaInfo = getCriteriaInfo(viewSql);
            if(viewInfos.size()==2) {
                if( criteriaInfo != null ) {
                    viewInfos.add(criteriaInfo);
                }
            }
            
            // Add info for the raw view ddl
            RestDataserviceViewInfo viewInfo = new RestDataserviceViewInfo();
            viewInfo.setInfoType(RestDataserviceViewInfo.DDL_INFO);
            viewInfo.setViewDdl(viewDdl);
            
            // Determine whether the DDL is 'editable' - whether it has enough info for editor wizard.
            // Problem if either the srcTable list or tableColumnMap is empty
            if( srcTables.isEmpty() || tableColumnMap.isEmpty() ) {
                viewInfo.setViewEditable(false);
            // Problem if the srcTables and tableColumnMap sizes dont match
            } else if ( srcTables.size() != tableColumnMap.size() ) {
                viewInfo.setViewEditable(false);
            // Problem if 2 tables (join) and there were no criteria found
            } else if ( tableColumnMap.size() == 2 && criteriaInfo == null ) {
                viewInfo.setViewEditable(false);
            // If column ordering in the DDL is different than the standard ordering - set editable false
            } else if (columnOrderingChanged(uow, lhTable, rhTable, viewDdl)) {
                viewInfo.setViewEditable(false);
            } else if ( hasUnmatchedTable ) {
                viewInfo.setViewEditable(false);
            } else {
                viewInfo.setViewEditable(true);
            }
            viewInfos.add(viewInfo);

            return commit(uow, mediaTypes, viewInfos);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_FIND_VIEW_INFO_ERROR, dataserviceName);
        }
    }
    
    /*
     * Generate a mapping of tableName to columnName list from the SQL.  
     *    - if the SQL cannot be fully parsed, an empty map is returned.
     *    - TODO: replace the current string parsing with an appropriate SQL parser when available
     */
    private Map<String,List<String>> getTableColumnNameMap(String viewSql) {
        Map<String,List<String>> tableColumnMap = new HashMap<String,List<String>>();
        
        List<String> colNames = new ArrayList<String>();

        if(!StringUtils.isEmpty(viewSql)) {
            // Collect the ColumnNames - after the SELECT and before the FROM
            List<String> cols = getColumnNamesFromSQL(viewSql);
            for(String cName : cols ) {
                if(!cName.startsWith("ROW_NUMBER()")) { //$NON-NLS-1$
                    colNames.add(cName);
                }
            }
            
            // Get SQL after the FROM keyword to the end.
            String fromStr = StringConstants.EMPTY_STRING;
            int fromStartIndex = viewSql.indexOf(SQLConstants.Reserved.FROM+StringConstants.SPACE);
            if(fromStartIndex > -1) {
                fromStr = viewSql.substring(fromStartIndex + (SQLConstants.Reserved.FROM+StringConstants.SPACE).length());
            }
            
            // Handle JOIN SQL
            if(fromStr.contains(INNER_JOIN) || fromStr.contains(LEFT_OUTER_JOIN) || fromStr.contains(RIGHT_OUTER_JOIN) || fromStr.contains(FULL_OUTER_JOIN)) {
                int indxStart = 0;
                int indxEnd = fromStr.indexOf(SQLConstants.Reserved.AS+StringConstants.SPACE+LH_TABLE_ALIAS); 
                String lhTable = null;
                if(indxEnd > -1) {
                    lhTable = fromStr.substring(indxStart, indxEnd).trim();
                }

                indxStart = fromStr.indexOf(SQLConstants.Reserved.JOIN+StringConstants.SPACE);
                indxEnd = fromStr.indexOf(SQLConstants.Reserved.AS+StringConstants.SPACE+RH_TABLE_ALIAS); 
                String rhTable = null;
                if(indxStart > -1 && indxEnd > -1) {
                    rhTable = fromStr.substring(indxStart+(SQLConstants.Reserved.JOIN+StringConstants.SPACE).length(), indxEnd).trim();
                }
                
                // Now the table aliases have been determined - separate the original column names with the appropriate alias
                List<String> lhCols = new ArrayList<>();
                List<String> rhCols = new ArrayList<>();
                
                for(String colName : colNames) {
                    if(colName.startsWith(LH_TABLE_ALIAS_DOT)) {
                        lhCols.add(colName.substring(LH_TABLE_ALIAS_DOT.length()));
                    } else if(colName.startsWith(RH_TABLE_ALIAS_DOT)) {
                        rhCols.add(colName.substring(RH_TABLE_ALIAS_DOT.length()));
                    }
                }
                // If either of the tables is blank, there was a problem - leave the map empty
                // For joins the table alias is included to determine left and right
                if(!StringUtils.isBlank(lhTable) && !StringUtils.isBlank(rhTable)) {
                    tableColumnMap.put(lhTable+StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+LH_TABLE_ALIAS, lhCols);
                    tableColumnMap.put(rhTable+StringConstants.SPACE+SQLConstants.Reserved.AS+StringConstants.SPACE+RH_TABLE_ALIAS, rhCols);
                }
            // Handle single source SQL
            } else {
                String tableName = fromStr.trim();
                if(!StringUtils.isBlank(tableName)) {
                    tableColumnMap.put(tableName, colNames);
                }
            }
            
        }

        return tableColumnMap;
    }
    
    /**
     * Determines if the viewSql column ordering has changed from what the editor would generate.
     * @param lhTable the left table
     * @param rhTable the right table
     * @param viewSql the supplied view SQL
     * @return 'true' if the view SQL columns are ordered differently than lhTable then rhTable column ordering
     */
    private boolean columnOrderingChanged(UnitOfWork uow, Table lhTable, Table rhTable, String viewSql) throws KException {
        boolean isDifferent = false;
        
        // Compile list of all columns for LH and RH tables in order.  Add aliases to distinguish
        List<String> tableColNameOrdered = new ArrayList();
        Column[] columns = lhTable.getColumns(uow);
        for(Column column : columns) {
            tableColNameOrdered.add( LH_TABLE_ALIAS_DOT + column.getName(uow) );
        }
        if(rhTable!=null) {
            columns = rhTable.getColumns(uow);
            for(Column column : columns) {
                tableColNameOrdered.add( RH_TABLE_ALIAS_DOT + column.getName(uow) );
            }
        }
        
        // Collect the ColumnNames - after the SELECT and before the FROM
        List<String> sqlColNames = new ArrayList<String>();
        List<String> cols = getColumnNamesFromSQL(viewSql);
        for(String cName : cols ) {
            if(!cName.startsWith("ROW_NUMBER()")) { //$NON-NLS-1$
                // If name is not aliased at all, then it is a LH column.  Alias for comparison with whole list
                if( !cName.startsWith(LH_TABLE_ALIAS_DOT) && !cName.startsWith(RH_TABLE_ALIAS_DOT) ) {
                    sqlColNames.add( LH_TABLE_ALIAS_DOT + cName );
                } else {
                    sqlColNames.add( cName );
                }
            }
        }
        
        // Now compare the ordering of the sqlColNames to the entire tableA / tableB list
        Iterator<String> tcIter = tableColNameOrdered.iterator();
        while(tcIter.hasNext()) {
            String tcName = tcIter.next();
            if(!sqlColNames.contains(tcName)) {
                tcIter.remove();
            }
        }
        
        // The remaining lists should match exactly, otherwise the column ordering is different.
        if( sqlColNames.size() != tableColNameOrdered.size() ) {
            isDifferent = true;
        } else {
            for( int i = 0; i < sqlColNames.size(); i++ ) {
                if( !sqlColNames.get(i).equalsIgnoreCase(tableColNameOrdered.get(i)) ) {
                    isDifferent = true;
                    break;
                }
            }
        }
        
        return isDifferent;
    }
    
    /*
     * Get the list of column names from the provided sql.  This simply gets the string between the SELECT and FROM,
     * and splits the names between commas.
     * @param sql the SQL 
     * @return the parsed list of column names
     */
    private List<String> getColumnNamesFromSQL(String sql) {
        ArrayList<String> columnNames = new ArrayList();
        
        int startIndex = sql.indexOf(SQLConstants.Reserved.SELECT)+(SQLConstants.Reserved.SELECT).length();
        int endIndex = sql.indexOf(SQLConstants.Reserved.FROM+StringConstants.SPACE);
        // If SELECT or FROM not found, assume empty columns
        String columnsStr = StringConstants.EMPTY_STRING;
        String[] cols = new String[0];
        if(startIndex > -1 && endIndex > startIndex) {
            columnsStr = sql.substring(startIndex,endIndex);
        }
        if(!columnsStr.trim().isEmpty()) {
            cols = columnsStr.split(COMMA);
        }
        // remove any leading or trailing spaces
        for(String col : cols) {
            columnNames.add(col.trim());
        }
        return columnNames;
    }
    
    /*
     * Generates join criteria, given a left and right table.
     * The tables are examined for PK-FK relationship.  
     *   - If a relationship is found, returns criteria info with predicates.
     *   - If no relationships are found, returns criteria info with no predicates.
     */
    private RestDataserviceViewInfo buildJoinCriteria(UnitOfWork uow, Table lhTable, Table rhTable) throws KException {
        // Use left table PK.  Look for FK match on the right table
        PrimaryKey pk = lhTable.getPrimaryKey(uow);
        ForeignKey fk = null;
        if(pk != null) {
            fk = findFkMatch(uow, pk, rhTable);
        }
        
        // If a FK match was not found, use the right table PK.  Look for FK match on the left table.
        if(fk==null) {
            pk = rhTable.getPrimaryKey(uow);
            if(pk != null) {
                fk = findFkMatch(uow, pk, lhTable);
            }
        }

        RestDataserviceViewInfo criteriaInfo = new RestDataserviceViewInfo();
        criteriaInfo.setInfoType(RestDataserviceViewInfo.CRITERIA_INFO);
        // If PK-FK match was found, generate join predicates.
        if(pk!=null && fk!=null) {
            // Create the predicates based on pk and fk columns.
            List<ViewBuilderCriteriaPredicate> predicates = new ArrayList<ViewBuilderCriteriaPredicate>();
            
            Column[] pkColumns = pk.getColumns(uow);
            Column[] fkColumns = fk.getColumns(uow);
            
            for(int i=0; i<pkColumns.length; i++) {
                ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
                predicate.setOperator(EQ);
                predicate.setCombineKeyword(AND); 
                predicate.setLhColumn(pkColumns[i].getName(uow));
                predicate.setRhColumn(fkColumns[i].getName(uow));
                predicates.add(predicate);
            }

            criteriaInfo.setCriteriaPredicates(predicates);
        } else {
            criteriaInfo.setCriteriaPredicates(Collections.emptyList());
        }
        
        return criteriaInfo;
    }
    
    /*
     * Find a ForeignKey in the supplied table that matches the supplied PrimaryKey.
     *   - The FK references the PK table
     *   - The FK referenced columns are the same as the PK columns
     */
    private ForeignKey findFkMatch(UnitOfWork uow, PrimaryKey pk, Table table) throws KException {
        Table pkTable = pk.getTable(uow);
        ForeignKey matchingFK = null;
        
        // Get the primary key columns
        Column[] pkColumns = pk.getColumns(uow);
        List<Column> pkColumnList = Arrays.asList(pkColumns);
        
        // Examine FKs on the supplied table.
        ForeignKey[] fks = table.getForeignKeys(uow);
        for(ForeignKey fk : fks) {
            // Get table referenced by the FK - if references the PK table, examine columns
            Table fkTableRef = fk.getReferencesTable(uow);
            if(fkTableRef!=null && fkTableRef.equals(pkTable)) {
                // Get FK referenced columns.
                Column[] fkRefColumns = fk.getReferencesColumns(uow);
                if(fkRefColumns.length != pkColumns.length) {
                    continue;
                } else {
                    // make sure all fk columns exist in the pk column list
                    boolean columnsMatch = true;
                    for(Column fkRefColumn : fkRefColumns) {
                        if(!pkColumnList.contains(fkRefColumn)) {
                            columnsMatch = false;
                            break;
                        }
                    }
                    // stop if fk match found
                    if(columnsMatch) {
                        matchingFK = fk;
                        break;
                    }
                }
            }
        }
        return matchingFK;
    }
    
    /*
     * create the criteria info
     */
    private RestDataserviceViewInfo getCriteriaInfo(String viewSql) {
        // If sql does not have a join, return null info
        if(!hasJoin(viewSql)) {
            return null;
        }
        RestDataserviceViewInfo criteriaInfo = new RestDataserviceViewInfo();
        criteriaInfo.setInfoType(RestDataserviceViewInfo.CRITERIA_INFO);
        
        if(!StringUtils.isEmpty(viewSql)) {
            if(viewSql.indexOf(INNER_JOIN) != -1) {
                criteriaInfo.setJoinType(RestDataserviceViewInfo.JOIN_INNER);
            } else if(viewSql.indexOf(LEFT_OUTER_JOIN) != -1) {
                criteriaInfo.setJoinType(RestDataserviceViewInfo.JOIN_LEFT_OUTER);
            } else if(viewSql.indexOf(RIGHT_OUTER_JOIN) != -1) {
                criteriaInfo.setJoinType(RestDataserviceViewInfo.JOIN_RIGHT_OUTER);
            } else if(viewSql.indexOf(FULL_OUTER_JOIN) != -1) {
                criteriaInfo.setJoinType(RestDataserviceViewInfo.JOIN_FULL_OUTER);
            }
            
            // Find the criteria string
            String asRhAliasOn = SQLConstants.Reserved.AS + StringConstants.SPACE + RH_TABLE_ALIAS + StringConstants.SPACE + SQLConstants.Reserved.ON;
            int startIndex = viewSql.indexOf(asRhAliasOn);
            String criteriaStr = StringConstants.EMPTY_STRING;
            if(startIndex > -1) {
                criteriaStr = viewSql.substring(startIndex+(asRhAliasOn).length());
            }
            
            List<ViewBuilderCriteriaPredicate> predicates = new ArrayList<ViewBuilderCriteriaPredicate>();
            if(!StringUtils.isEmpty(criteriaStr)) {
                // Process the criteriaStr predicates
                while ( !StringUtils.isEmpty(criteriaStr) ) {
                    int orIndex = criteriaStr.indexOf(StringConstants.SPACE+OR+StringConstants.SPACE);
                    int andIndex = criteriaStr.indexOf(StringConstants.SPACE+AND+StringConstants.SPACE);
                    // No OR or AND.  Either a single predicate or the last predicate
                    if( orIndex == -1 && andIndex == -1 ) {
                        ViewBuilderCriteriaPredicate predicate = parsePredicate(criteriaStr, AND);
                        if(predicate.isComplete()) {
                            predicates.add(predicate);
                        }
                        criteriaStr = StringConstants.EMPTY_STRING;
                    // Has OR but no AND.
                    } else if( orIndex > -1 && andIndex == -1 ) {
                        String predicateStr = criteriaStr.substring(0, orIndex);
                        ViewBuilderCriteriaPredicate predicate = parsePredicate(predicateStr, OR);
                        if(predicate.isComplete()) {
                            predicates.add(predicate);
                        }
                        criteriaStr = criteriaStr.substring(orIndex + (StringConstants.SPACE+OR+StringConstants.SPACE).length());
                    // Has AND but no OR.
                    } else if( orIndex == -1 && andIndex > -1 ) {
                        String predicateStr = criteriaStr.substring(0, andIndex);
                        ViewBuilderCriteriaPredicate predicate = parsePredicate(predicateStr, AND);
                        if(predicate.isComplete()) {
                            predicates.add(predicate);
                        }
                        criteriaStr = criteriaStr.substring(andIndex + (StringConstants.SPACE+AND+StringConstants.SPACE).length());
                    // Has both - OR is first.
                    } else if( orIndex < andIndex ) {
                        String predicateStr = criteriaStr.substring(0, orIndex);
                        ViewBuilderCriteriaPredicate predicate = parsePredicate(predicateStr, OR);
                        if(predicate.isComplete()) {
                            predicates.add(predicate);
                        }
                        criteriaStr = criteriaStr.substring(orIndex + (StringConstants.SPACE+OR+StringConstants.SPACE).length());
                    // Has both - AND is first.
                    } else {
                        String predicateStr = criteriaStr.substring(0, andIndex);
                        ViewBuilderCriteriaPredicate predicate = parsePredicate(predicateStr, AND);
                        if(predicate.isComplete()) {
                            predicates.add(predicate);
                        }
                        criteriaStr = criteriaStr.substring(andIndex + (StringConstants.SPACE+AND+StringConstants.SPACE).length());
                    }
                }
                criteriaInfo.setCriteriaPredicates(predicates);                
            }
            // Check that at least one predicate was found, and it is complete
            if( predicates.size() == 0 ) {
                criteriaInfo = null;
            } else {
                for(ViewBuilderCriteriaPredicate predicate : predicates) {
                    if(!predicate.isComplete()) {
                        criteriaInfo = null;
                        break;
                    }
                }
            }
        }
        
        return criteriaInfo;
    }
    
    private ViewBuilderCriteriaPredicate parsePredicate(String predicateStr, String combineKeyword) {
        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
        predicate.setCombineKeyword(combineKeyword);

        String[] criteriaCols = null;
        if( predicateStr.indexOf(StringConstants.SPACE+EQ+StringConstants.SPACE) > -1 ) {
            predicate.setOperator(EQ);
            criteriaCols = predicateStr.split(EQ);
        } else if( predicateStr.indexOf(StringConstants.SPACE+LT+StringConstants.SPACE) > -1) {
            predicate.setOperator(LT);
            criteriaCols = predicateStr.split(LT);
        } else if( predicateStr.indexOf(StringConstants.SPACE+GT+StringConstants.SPACE) > -1) {
            predicate.setOperator(GT);
            criteriaCols = predicateStr.split(GT);
        } else if( predicateStr.indexOf(StringConstants.SPACE+NE+StringConstants.SPACE) > -1) {
            predicate.setOperator(NE);
            criteriaCols = predicateStr.split(NE);
        } else if( predicateStr.indexOf(StringConstants.SPACE+LE+StringConstants.SPACE) > -1) {
            predicate.setOperator(LE);
            criteriaCols = predicateStr.split(LE);
        } else if( predicateStr.indexOf(StringConstants.SPACE+GE+StringConstants.SPACE) > -1) {
            predicate.setOperator(GE);
            criteriaCols = predicateStr.split(GE);
        }
        if(criteriaCols!=null) {
            for(int i=0; i<criteriaCols.length; i++ ) {
                String cCol = criteriaCols[i].trim();
                if(!StringUtils.isBlank(cCol)) {
                    if(cCol.startsWith(LH_TABLE_ALIAS_DOT)) {
                        predicate.setLhColumn(cCol.substring(LH_TABLE_ALIAS_DOT.length()));
                    } else if(cCol.startsWith(RH_TABLE_ALIAS_DOT)) {
                        predicate.setRhColumn(cCol.substring(RH_TABLE_ALIAS_DOT.length()));
                    }
                }
            }
        }
        return predicate;
    }
    
    /*
     * Determine if the ddl has a join
     */
    private boolean hasJoin(String viewDdl) {
        if( !StringUtils.isEmpty(viewDdl) && 
            ( viewDdl.indexOf(INNER_JOIN)!=-1 || viewDdl.indexOf(LEFT_OUTER_JOIN)!=-1 || viewDdl.indexOf(RIGHT_OUTER_JOIN)!=-1 || viewDdl.indexOf(FULL_OUTER_JOIN)!=-1 ) ) {
            return true;
        }
        return false;
    }

	/**
	 * @param headers
	 *            the request headers (never <code>null</code>)
	 * @param uriInfo
	 *            the request URI information (never <code>null</code>)
	 * @param dataserviceName
	 *            the data service name being validated (cannot be empty)
	 * @return the response (never <code>null</code>) with an entity that is
	 *         either an empty string, when the name is valid, or an error
	 *         message
	 * @throws KomodoRestException
	 *             if there is a problem validating the data service name or
	 *             constructing the response
	 */
    @GET
    @Path( V1Constants.NAME_VALIDATION_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER )
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the data service name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 500, message = "The dataservice name cannot be empty." )
    } )
    public Response validateDataserviceName( final @Context HttpHeaders headers,
                                             final @Context UriInfo uriInfo,
                                             @ApiParam( value = "The dataservice name being checked", required = true )
                                             final @PathParam( "dataserviceName" ) String dataserviceName ) throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName( dataserviceName );
        
        // a name validation error occurred
        if ( errorMsg != null ) {
        	final Response response = Response.ok().entity( errorMsg ).build();
            return response;
        }

        // check for duplicate name
        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "validateDataserviceName", true ); //$NON-NLS-1$
            final Dataservice service = findDataservice( uow, dataserviceName );

            if ( service == null ) {
                final Vdb vdb = findVdb( uow, dataserviceName );

                // name is valid
                if ( vdb == null) {
                    return Response.ok().build();
                }
            }

            // name is a duplicate
            return Response.ok()
                           .entity( RelationalMessages.getString( DATASERVICE_SERVICE_NAME_EXISTS ) )
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
                                                     DATASERVICE_SERVICE_NAME_VALIDATION_ERROR );
        }
    }
}
