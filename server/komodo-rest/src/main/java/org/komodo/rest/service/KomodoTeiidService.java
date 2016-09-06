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

import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.CallbackTimeoutException;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestDataSourceDriver;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.rest.relational.response.RestTeiidStatus;
import org.komodo.rest.relational.response.RestTeiidVdbStatus;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.ExecutionAdmin;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.FileUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from a teiid server.
 */
@Path( V1Constants.TEIID_SEGMENT )
@Api( tags = {V1Constants.TEIID_SEGMENT} )
public class KomodoTeiidService extends KomodoService {

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoTeiidService(final KEngine engine) throws WebApplicationException {
        super(engine);
    }

    private synchronized Teiid getDefaultTeiid() throws KException {
        ServerManager serverManager = ServerManager.getInstance(repo);
        UnitOfWork uow = null;

        try {
            uow = createTransaction(SYSTEM_USER, "getTeiidStatus", false); //$NON-NLS-1$
            Teiid teiid = serverManager.getDefaultServer(uow);
            uow.commit();

            return teiid;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private CachedTeiid importContent() throws KException, CallbackTimeoutException {
        Teiid teiid = getDefaultTeiid();
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = createTransaction(SYSTEM_USER, "import-teiid-content", false, callback); //$NON-NLS-1$
        CachedTeiid cachedTeiid = teiid.importContent(uow);

        // Commit the transaction to allow the sequencers to run
        uow.commit();

        try {
            if (! callback.await(3, TimeUnit.MINUTES)) {
                throw new CallbackTimeoutException();
            }
        } catch (Exception ex) {
            throw new KException(ex);
        }

        return cachedTeiid;
    }

    private Response createTimeoutResponse(List<MediaType> mediaTypes) {
        Object responseEntity = createErrorResponseEntity(mediaTypes,
                                                                  RelationalMessages.getString(
                                                                                               RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT));
        return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
    }

    private Response checkTeiidAttributes(String adminUser, String adminPasswd,
                                                                           String jdbcUser, String jdbcPasswd,
                                                                           Boolean adminSecure, Boolean jdbcSecure,
                                                                           List<MediaType> mediaTypes) {
        if (adminUser == null && adminPasswd == null && adminSecure == null &&
            jdbcUser == null && jdbcPasswd == null && jdbcSecure == null) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_EMPTY_CREDENTIAL_ERROR);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        return Response.ok().build();
    }

    private Response checkFileAttributes(KomodoFileAttributes kfa, List<MediaType> mediaTypes) throws Exception {
        String errorMessage = null;
        if (kfa == null || (kfa.getName() == null && kfa.getContent() == null))
            errorMessage = RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_PARAMETERS);

        if (kfa.getName() == null)
            errorMessage = RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_NAME);

        if (kfa.getContent() == null)
            errorMessage = RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_CONTENT);

        if (errorMessage != null) {
            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        return Response.ok().build();
    }

    private boolean hasDataSourceDriver(String driverName, TeiidInstance instance) throws Exception {
        Collection<DataSourceDriver> drivers = instance.getDataSourceDrivers();
        for (DataSourceDriver driver : drivers) {
            if (driver.getName().startsWith(driverName))
                return true;
        }

        return false;
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the status of the local teiid server (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.STATUS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the status of the teiid instance",
                            response = RestTeiidStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response status(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiid = getDefaultTeiid();

            uow = createTransaction(principal, "getTeiidStatus", true); //$NON-NLS-1$
            RestTeiidStatus status = new RestTeiidStatus(uriInfo.getBaseUri(), teiid, uow);

            // create response
            return commit(uow, mediaTypes, status);

        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_STATUS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document the status of the VDBs in the local teiid server (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.STATUS_SEGMENT + StringConstants.FORWARD_SLASH + 
                  V1Constants.VDBS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the status of the vdbs of the teiid instance",
                            response = RestTeiidVdbStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response vdbs(final @Context HttpHeaders headers,
                                             final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        Teiid teiid = null;

        try {
            teiid = getDefaultTeiid();

            uow = createTransaction(principal, "getTeiidVdbs", true); //$NON-NLS-1$
            RestTeiidVdbStatus status = new RestTeiidVdbStatus(uriInfo.getBaseUri(), teiid, uow);

            // create response
            return commit(uow, mediaTypes, status);

        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_VDBS_STATUS_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.TEIID_CREDENTIALS)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Modify the credentials of the teiid server",
                             response = RestTeiid.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response setCredentials(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   final String credentialAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoTeiidAttributes teiidAttrs;
        try {
            teiidAttrs = KomodoJsonMarshaller.unmarshall(credentialAttributes, KomodoTeiidAttributes.class);
            Response response = checkTeiidAttributes(teiidAttrs.getAdminUser(),
                                                                                        teiidAttrs.getAdminPasswd(),
                                                                                        teiidAttrs.getJdbcUser(),
                                                                                        teiidAttrs.getJdbcPasswd(),
                                                                                        teiidAttrs.isAdminSecure(),
                                                                                        teiidAttrs.isJdbcSecure(),
                                                                                        mediaTypes);

            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponse(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "teiidSetCredentials", false); //$NON-NLS-1$

            if (teiidAttrs.getAdminUser() != null)
                teiidNode.setAdminUser(uow, teiidAttrs.getAdminUser());

            if (teiidAttrs.getAdminPasswd() != null)
                teiidNode.setAdminPassword(uow, teiidAttrs.getAdminPasswd());

            if (teiidAttrs.isAdminSecure() != null)
                teiidNode.setAdminSecure(uow, teiidAttrs.isAdminSecure());

            if (teiidAttrs.getJdbcUser() != null)
                teiidNode.setJdbcUsername(uow, teiidAttrs.getJdbcUser());

            if (teiidAttrs.getJdbcPasswd() != null)
                teiidNode.setJdbcPassword(uow, teiidAttrs.getJdbcPasswd());

            if (teiidAttrs.isJdbcSecure() != null)
                teiidNode.setJdbcSecure(uow, teiidAttrs.isJdbcSecure());

            RestBasicEntity teiidEntity = entityFactory.create(teiidNode, uriInfo.getBaseUri(), uow);
            return commit(uow, mediaTypes, teiidEntity);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_SET_CREDENTIALS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the VDBs deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.VDBS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of vdbs",
                            response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbs(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find VDBs
            uow = createTransaction(principal, "getVdbs", true); //$NON-NLS-1$

            Vdb[] vdbs = cachedTeiid.getVdbs(uow);
            LOGGER.debug("getVdbs:found '{0}' VDBs", vdbs.length); //$NON-NLS-1$

            final List<RestVdb> entities = new ArrayList<>();

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
            for (final Vdb vdb : vdbs) {
                RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
                LOGGER.debug("getVdbs:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_VDBS_ERROR);
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
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                  V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
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

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        
        UnitOfWork uow = null;
        try {
            CachedTeiid cachedTeiid = importContent();

            // find VDB
            uow = createTransaction(principal, "getVdb-" + vdbName, true); //$NON-NLS-1$
            Vdb vdb = cachedTeiid.getVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, mediaTypes.contains(MediaType.APPLICATION_XML_TYPE));
            final RestVdb restVdb = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restVdb );

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
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
     * @return a JSON document representing all the translators deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.TRANSLATORS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of translators",
                            response = RestVdbTranslator[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslators(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find translators
            uow = createTransaction(principal, "getTranslators", true); //$NON-NLS-1$

            Translator[] translators = cachedTeiid.getTranslators(uow);
            LOGGER.debug("getTranslators:found '{0}' Translators", translators.length); //$NON-NLS-1$

            final List<RestVdbTranslator> entities = new ArrayList<>();

            KomodoProperties properties = new KomodoProperties();
            for (final Translator translator : translators) {
                RestVdbTranslator entity = entityFactory.create(translator, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
                LOGGER.debug("getTranslators:Translator '{0}' entity was constructed", translator.getName(uow)); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TRANSLATORS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the data sources deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.DATA_SOURCES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of data sources",
                            response = RestDataSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataSources(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find translators
            uow = createTransaction(principal, "getDataSources", true); //$NON-NLS-1$

            Datasource[] dataSources = cachedTeiid.getDataSources(uow);
            LOGGER.debug("getDataSources:found '{0}' DataSources", dataSources.length); //$NON-NLS-1$

            final List<RestDataSource> entities = new ArrayList<>();

            KomodoProperties properties = new KomodoProperties();
            for (final Datasource dataSource : dataSources) {
                RestDataSource entity = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
                LOGGER.debug("getDataSources:Data Source '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCES_ERROR);
        }
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param datasourceName
     *        the id of the DataSource being retrieved (cannot be empty)
     * @return the JSON representation of the DataSource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace DataSource or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.DATA_SOURCES_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find dataSource by name", response = RestDataSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No dataSource could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataSource( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Id of the data source to be fetched", required = true)
                                   final @PathParam( "datasourceName" ) String datasourceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        
        UnitOfWork uow = null;
        try {
            CachedTeiid cachedTeiid = importContent();

            // find DataSource
            uow = createTransaction(principal, "getDataSource-" + datasourceName, true); //$NON-NLS-1$
            Datasource dataSource = cachedTeiid.getDataSource(uow, datasourceName);
            if (dataSource == null)
                return commitNoDatasourceFound(uow, mediaTypes, datasourceName);

            KomodoProperties properties = new KomodoProperties();
            final RestDataSource restDataSource = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getDataSource:Datasource '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restDataSource );

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_ERROR, datasourceName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the drivers deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.DRIVERS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of drivers",
                            response = RestDataSourceDriver[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDrivers(final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find drivers
            uow = createTransaction(principal, "getDrivers", true); //$NON-NLS-1$

            Driver[] drivers = cachedTeiid.getDrivers(uow);
            LOGGER.debug("getDrivers:found '{0}' Drivers", drivers.length); //$NON-NLS-1$

            final List<RestDataSourceDriver> entities = new ArrayList<>();

            for (final Driver driver : drivers) {
                RestDataSourceDriver entity = new RestDataSourceDriver();
                entity.setName(driver.getName(uow));
                entities.add(entity);
                LOGGER.debug("getDrivers:Driver '{0}' entity was constructed", driver.getName(uow)); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DRIVERS_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.TEIID_DRIVER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Add a driver to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addDriver(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   final String fileAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoFileAttributes kfa;
        try {
            kfa = KomodoJsonMarshaller.unmarshall(fileAttributes, KomodoFileAttributes.class);
            Response response = checkFileAttributes(kfa, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            String errorMessage = RelationalMessages.getString(
                                                               RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR, ex.getMessage());

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidDriver", false); //$NON-NLS-1$

            byte[] content = decode(kfa.getContent());
            String tempDir = FileUtils.tempDirectory();
            String fileName = content.hashCode() + DOT + kfa.getName();
            File driverFile = new File(tempDir, fileName);

            FileUtils.write(content, driverFile);

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.deployDriver(kfa.getName(), driverFile);

            // Await the deployment to end
            Thread.sleep(2000);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (hasDataSourceDriver(kfa.getName(), teiidInstance))
                status.addAttribute(kfa.getName(),
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED));
            else
                status.addAttribute(kfa.getName(),
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UPLOADED));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DRIVER_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @DELETE
    @Path(V1Constants.TEIID_DRIVER + StringConstants.FORWARD_SLASH +
                  V1Constants.TEIID_DRIVER_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a driver from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeDriver(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Name of the driver to be removed", required = true)
                                    final @PathParam( "driverName" ) String driverName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "unDeployTeiidDriver", true); //$NON-NLS-1$

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.undeployDriver(driverName);

            // Await the undeployment to end
            Thread.sleep(2000);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (! hasDataSourceDriver(driverName, teiidInstance))
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED));
            else
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_UNDEPLOYMENT_REQUEST_SENT));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DRIVER_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.DATA_SERVICE_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the data service to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addDataservice(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   final String pathAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no path attribute defined
        //
        KomodoPathAttribute kpa;
        try {
            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
            if (kpa.getPath() == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_DATA_SERVICE_MISSING_PATH);
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }
        } catch (Exception ex) {
            String errorMessage = RelationalMessages.getString(
                                                               RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR, ex.getMessage());

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidDataservice", false); //$NON-NLS-1$

            List<KomodoObject> dataServices = this.repo.searchByPath(uow, kpa.getPath());
            if (dataServices.size() == 0) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            Dataservice dataService = getWorkspaceManager(uow).resolve(uow, dataServices.get(0), Dataservice.class);
            if (dataService == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            //
            // Deploy the data service
            //
            DeployStatus deployStatus = dataService.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(500);

            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(dataService.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(dataService.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DRIVER_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.DATA_SOURCE_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the data source to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addDatasource(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   final String pathAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no path attribute defined
        //
        KomodoPathAttribute kpa;
        try {
            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
            if (kpa.getPath() == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_DATA_SOURCE_MISSING_PATH);
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }
        } catch (Exception ex) {
            String errorMessage = RelationalMessages.getString(
                                                               RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR, ex.getMessage());

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidDatasource", false); //$NON-NLS-1$

            List<KomodoObject> dataSources = this.repo.searchByPath(uow, kpa.getPath());
            if (dataSources.size() == 0) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            Datasource dataSource = getWorkspaceManager(uow).resolve(uow, dataSources.get(0), Datasource.class);
            if (dataSource == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);

                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            //
            // Deploy the data source
            //
            DeployStatus deployStatus = dataSource.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(500);

            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(dataSource.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(dataSource.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DRIVER_ERROR);
        }
    }

    private String extractServiceVdbName(UnitOfWork uow, WorkspaceManager mgr, String dsPath) throws KException {
        KomodoObject dsObject = repo.getFromWorkspace(uow, dsPath);
        if (dsObject == null)
            return null; // Not a path in the workspace

        Dataservice dService = mgr.resolve(uow, dsObject, Dataservice.class);
        if (dService == null)
            return null; // Not a data service

        Vdb vdb = dService.getServiceVdb(uow);
        if (vdb == null)
            return null;

        return vdb.getVdbName(uow);
    }

    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.QUERY_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Pass a query to the teiid server",
                                notes = "Syntax of the json request body is of the form " +
                                "{ query : 'SELECT * ...', target : vdb name on teiid | dataservice path in the workspace, " +
                                "limit : the limit on records to be returned, offset : the record number to begin with }")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response query(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   final String queryAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no query attribute defined
        //
        KomodoQueryAttribute kqa;
        try {
            kqa = KomodoJsonMarshaller.unmarshall(queryAttribute, KomodoQueryAttribute.class);
            if (kqa.getQuery() == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_QUERY_MISSING_QUERY);
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            if (kqa.getTarget() == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_QUERY_MISSING_TARGET);
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }
        } catch (Exception ex) {
            String errorMessage = RelationalMessages.getString(
                                                               RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR, ex.getMessage());

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "queryTeiidservice", true); //$NON-NLS-1$
            Teiid teiidNode = getDefaultTeiid();
            WorkspaceManager mgr = getWorkspaceManager(uow);
            String target = kqa.getTarget();
            String query = kqa.getQuery();

            //
            // Is target a deployed vdb or a dataservice in the workspace that has had its vdbs deployed?
            //
            String vdbName = extractServiceVdbName(uow, mgr, target);
            if (vdbName == null) {
                //
                // The target does not reference a data service in the workspace
                // or the data service has no service vdb. Either way target should
                // be applied directly to the query.
                //
                vdbName = target;
            }

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            TeiidVdb vdb = teiidInstance.getVdb(vdbName);
            if (vdb == null) {
                String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_QUERY_TARGET_NOT_DEPLOYED);
                Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
                return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
            }

            LOGGER.debug("Establishing query service for query {0} on vdb {1}", query, vdbName);
            QueryService queryService = teiidNode.getQueryService(uow);

            QSResult result = queryService.query(vdbName, query, kqa.getOffset(), kqa.getLimit());
            RestQueryResult restResult = new RestQueryResult(result);

           return commit(uow, mediaTypes, restResult);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_QUERY_ERROR);
        }
    }

    @SuppressWarnings( "nls" )
    @GET
    @Path(V1Constants.PING_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Ping the connection to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response ping(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Execute either an admin or jdbc ping request", required = true)
                                    @QueryParam(value = PING_TYPE_PARAMETER) String pingType)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        ConnectivityType pingKind = ExecutionAdmin.ConnectivityType.findType(pingType);
        if (pingKind == null) {
            String errorMessage = RelationalMessages.getString(
                                                                   RelationalMessages.Error.TEIID_SERVICE_PING_MISSING_TYPE);
            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "pingTeiidservice", true); //$NON-NLS-1$
            Teiid teiidNode = getDefaultTeiid();

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            Outcome outcome = teiidInstance.ping(pingKind);
            System.out.println("KomodoTeiidService: pingJdbc: " + outcome.isOK() + "  " + outcome.getMessage());

            KomodoStatusObject status = new KomodoStatusObject("Status");
            status.addAttribute("OK", Boolean.toString(outcome.isOK()));
            status.addAttribute("Message", outcome.getMessage());
            if (outcome.getException() != null) {

                //
                // Find the narrowest cause of the failure
                //
                Throwable ex = outcome.getException();
                String msg = ex.getLocalizedMessage();
                while (ex.getCause() != null) {
                    ex = ex.getCause();
                    if (ex.getLocalizedMessage() != null)
                        msg = ex.getLocalizedMessage();
                }

                status.addAttribute("Exception", msg);
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_QUERY_ERROR);
        }
    }
}
