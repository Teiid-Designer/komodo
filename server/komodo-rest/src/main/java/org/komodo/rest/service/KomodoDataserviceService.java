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

import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_FIND_SOURCE_TABLE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_FIND_SOURCE_VDB_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_CONNECTIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DRIVERS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_NAME_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_DATASERVICE_ERROR;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
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
import org.komodo.relational.ViewDdlBuilder;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoDataserviceUpdateAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestDataSourceDriver;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import io.swagger.annotations.Api;
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
    @ApiOperation(value = "Display the collection of data services",
                            response = RestDataservice[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
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
    @ApiOperation(value = "Find dataservice by name", response = RestDataservice.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataservice( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(value = "Id of the dataservice to be fetched", required = true)
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
    @ApiOperation(value = "Create a dataservice in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createDataservice( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
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
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
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

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "cloneDataservice", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a dataservice with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, newDataserviceName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CLONE_ALREADY_EXISTS);
            }

            // create new Dataservice
            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
            final Dataservice oldDataservice = getWorkspaceManager(uow).resolve( uow, kobject, Dataservice.class );
            final RestDataservice oldEntity = entityFactory.create(oldDataservice, uriInfo.getBaseUri(), uow );
            
            final Dataservice dataservice = getWorkspaceManager(uow).createDataservice( uow, null, newDataserviceName);

            setProperties( uow, dataservice, oldEntity );

            final RestDataservice entity = entityFactory.create(dataservice, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
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
    @ApiOperation(value = "Sets the dataservice vdb using parameters provided in the request body",
                  notes = "Syntax of the json request body is of the form " +
                          "{ dataserviceName='serviceName', viewTablePath='path/to/table', modelSourcePath='path/to/modelSource' }")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response setServiceVdbForSingleTable( final @Context HttpHeaders headers,
    		final @Context UriInfo uriInfo,
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
            Response response = checkDataserviceUpdateAttributes(attr, mediaTypes);
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
        
        String absTablePath = attr.getViewTablePath();
        // Error if the viewTablePath is missing 
        if (StringUtils.isBlank( absTablePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH, dataserviceName);
        }
        
        String absServiceModelSourcePath = attr.getModelSourcePath();
        // Error if the modelSourcePath is missing 
        if (StringUtils.isBlank( absServiceModelSourcePath )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_SET_SERVICE_MISSING_MODELSOURCE_PATH, dataserviceName);
        }
        
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
            Vdb serviceVdb = Vdb.RESOLVER.resolve(uow, vdbObj);
            
            // Add to the ServiceVdb a virtual model for the View
            Model viewModel = serviceVdb.addModel(uow, SERVICE_VDB_VIEW_MODEL);
            viewModel.setModelType(uow, Type.VIRTUAL);
            
            // Generate the ViewDDL using the specified table, then set the viewModel content.
            String viewDdl = ViewDdlBuilder.getODataViewDdl(uow, dataserviceName+SERVICE_VDB_VIEW_SUFFIX, sourceTable);
        	viewModel.setModelDefinition(uow, viewDdl);

        	// Add a physical model of same name to the service VDB
        	String svcModelSourceName = svcModelSource.getName(uow);
        	Model sourceModel = serviceVdb.addModel(uow, svcModelSourceName);
        	sourceModel.setModelType(uow, Type.PHYSICAL);

            // The source model DDL contains the table DDL only.  This limits the source metadata which is loaded on deployment.
            Properties exportProps = new Properties();
            exportProps.put( ExportConstants.EXCLUDE_TABLE_CONSTRAINTS_KEY, true );
        	byte[] bytes = sourceTable.export(uow, exportProps);
        	String tableString = new String(bytes);
            sourceModel.setModelDefinition(uow, tableString);
            
        	// Add a ModelSource of same name to the physical model and set its Jndi and translator
        	ModelSource modelSource = sourceModel.addSource(uow, svcModelSourceName);
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
                                       final @PathParam( "dataserviceName" ) String dataserviceName,
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

            KomodoObject dataservice = wkspMgr.getChild(uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE);
            
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
    
    private Response checkDataserviceUpdateAttributes(KomodoDataserviceUpdateAttributes attr,
    		List<MediaType> mediaTypes) throws Exception {
    	
    	if (attr == null || attr.getDataserviceName() == null || attr.getViewTablePath() == null || attr.getModelSourcePath() == null) {
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
                                    @ApiParam(value = "Id of the dataservice connections to be fetched", required = true)
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

            Datasource[] connections = dataservice.getConnections(uow);
            List<RestDataSource> restConnections = new ArrayList<>(connections.length);
            for (Datasource connection : connections) {
                RestDataSource entity = entityFactory.create(connection, uriInfo.getBaseUri(), uow);
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
    @ApiOperation(value = "Find a dataservice's drivers ", response = RestDataSourceDriver.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDrivers( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(value = "Id of the dataservice drivers to be fetched", required = true)
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
            List<RestDataSourceDriver> restDrivers = new ArrayList<>(drivers.length);
            for (Driver driver : drivers) {
                DataSourceDriver aDriver = new DataSourceDriver(driver.getName(uow),null);
                RestDataSourceDriver entity = new RestDataSourceDriver(aDriver);
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
                                                 @ApiParam(value = "Id of the dataservice", required = true)
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

            // Get JNDIs for the service physical sources
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            List<String> serviceJndis = new ArrayList<String>();
            Model[] models = serviceVdb.getModels(uow);
            for(Model model : models) {
                if(model.getModelType(uow) == Model.Type.PHYSICAL) {
                    ModelSource[] modelSources = model.getSources(uow);
                    if(modelSources.length>0) {
                        serviceJndis.add(modelSources[0].getJndiName(uow));
                    }
                }
            }
            
            // Get list of source VDBs that have matching jndis
            List<Vdb> sourceVdbs = new ArrayList<Vdb>();
            for(String serviceJndi : serviceJndis) {
                Vdb sourceVdb = getSourceVdbWithJndi(uow, serviceJndi);
                if(sourceVdb!=null) sourceVdbs.add(sourceVdb);
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
    
    /*
     * Returns first service source vdb that has source jndi matching the supplied jndi
     */
    private Vdb getSourceVdbWithJndi(final UnitOfWork uow, final String jndiName) throws KException {
        // Look for source vdb that has physical source with matching jndi
        WorkspaceManager wsMgr = getWorkspaceManager(uow);
        Vdb[] wsVdbs = wsMgr.findVdbs(uow);
        for(Vdb wsVdb : wsVdbs) {
            if(!isServiceSourceVdb(uow, wsVdb)) continue;
            Model[] models = wsVdb.getModels(uow);
            for(Model model : models) {
                if(model.getModelType(uow) == Model.Type.PHYSICAL) {
                    ModelSource[] modelSources = model.getSources(uow);
                    for(ModelSource modelSource : modelSources) {
                        String sourceJndi = modelSource.getJndiName(uow);
                        if(sourceJndi.equals(jndiName)) {
                            return wsVdb;
                        }
                    }
                }
            }
        }
        
        return null;
    }
    
    private boolean isServiceSourceVdb(final UnitOfWork uow, final Vdb vdb) throws KException {
        return (vdb.hasProperty(uow, DSB_PROP_SERVICE_SOURCE)) ? true : false;
    }
    
    /**
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
    @Path( V1Constants.DATA_SERVICE_PLACEHOLDER + StringConstants.FORWARD_SLASH + V1Constants.SERVICE_VIEW_TABLES)
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "retrieve the service view tableNames for a dataservice")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getServiceViewTablesForDataService( final @Context HttpHeaders headers,
                                                        final @Context UriInfo uriInfo,
                                                        @ApiParam(value = "Id of the dataservice", required = true)
                                                        final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getServiceViewTablesForDataService", true ); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            // Get the View Query Expression and extract the table names
            String[] viewTables = null;
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            Model[] models = serviceVdb.getModels(uow);
            for(Model model : models) {
                if(model.getModelType(uow) == Model.Type.VIRTUAL) {
                    View[] views = model.getViews(uow);
                    String viewDdl = views[0].getQueryExpression(uow);
                    if(!StringUtils.isEmpty(viewDdl)) {
                        // View source table name is after the FROM, to the end of the query.
                        int startIndex = viewDdl.indexOf("FROM ")+("FROM ").length(); //$NON-NLS-1$ //$NON-NLS-2$
                        String viewTableStr = viewDdl.substring(startIndex);
                        viewTables = viewTableStr.split(COMMA);
                    }
                    break;
                }
            }
                        
            // Return a status object with the sourceTable
            KomodoStatusObject kso = new KomodoStatusObject();
            if(viewTables!=null) {
                for (int i = 0; i < viewTables.length; ++i) {
                    kso.addAttribute("SourceTable" + (i + 1), viewTables[i]); //$NON-NLS-1$
                }
            }

            return commit(uow, mediaTypes, kso);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_FIND_SOURCE_TABLE_ERROR, dataserviceName);
        }
    }
    
}
