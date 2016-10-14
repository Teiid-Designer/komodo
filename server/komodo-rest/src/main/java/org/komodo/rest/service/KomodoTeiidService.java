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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
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
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.Messages;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.model.Model;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.relational.workspace.WorkspaceManager;
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
import org.komodo.rest.relational.request.KomodoVdbUpdateAttributes;
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
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining information from a teiid server.
 */
@Path( V1Constants.TEIID_SEGMENT )
@Api( tags = {V1Constants.TEIID_SEGMENT} )
public class KomodoTeiidService extends KomodoService {

    /**
     * Time to wait after deploying/undeploying an artifact from the teiid instance
     */
    private final static int DEPLOYMENT_WAIT_TIME = 2000;

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
            uow = systemTx("getTeiidStatus", false); //$NON-NLS-1$
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

    private synchronized CachedTeiid importContent(Teiid teiid) throws KException {
        UnitOfWork uow = null;
        try {
            uow = systemTx("import-teiid-content", false);
            CachedTeiid cachedTeiid = teiid.importContent(uow);

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            // Await the sequencers to finish
            awaitCallback(uow);

            return cachedTeiid;
        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private synchronized void refreshCachedDataSources(Teiid teiid, String... dataSourceNames) throws KException {
        CachedTeiid cachedTeiid = importContent(teiid);

        UnitOfWork uow = null;
        try {
            uow = systemTx("refresh-teiid-content", false);
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            cachedTeiid.refreshDataSources(uow, teiidInstance, dataSourceNames);

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            // Await the sequencers to finish
            awaitCallback(uow);

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private synchronized void refreshCachedDrivers(Teiid teiid, String... driverNames) throws KException {
        CachedTeiid cachedTeiid = importContent(teiid);

        UnitOfWork uow = null;
        try {
            uow = systemTx("refresh-teiid-content", false);
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            cachedTeiid.refreshDrivers(uow, teiidInstance, driverNames);

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            // Await the sequencers to finish
            awaitCallback(uow);

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private synchronized void refreshCachedVdbs(Teiid teiid, String... vdbNames) throws KException {
        CachedTeiid cachedTeiid = importContent(teiid);

        UnitOfWork uow = null;
        try {
            uow = systemTx("refresh-teiid-content", false);
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            cachedTeiid.refreshVdbs(uow, teiidInstance, vdbNames);

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            // Await the sequencers to finish
            awaitCallback(uow);

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private synchronized void refreshCachedFromDataService(Teiid teiid, Dataservice dataService) throws KException {
        CachedTeiid cachedTeiid = importContent(teiid);

        UnitOfWork uow = null;
        try {
            uow = systemTx("refresh-teiid-content", false);
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            // DataSources
            Datasource[] dataSources = dataService.getConnections(uow);
            String[] dataSourceNames = new String[dataSources.length];
            for (int i = 0; i < dataSources.length; i++) {
                dataSourceNames[i] = dataSources[i].getJndiName(uow);
            }
            cachedTeiid.refreshDataSources(uow, teiidInstance, dataSourceNames);

            // Drivers
            Driver[] drivers = dataService.getDrivers(uow);
            String[] driverNames = new String[drivers.length];
            for (int i = 0; i < drivers.length; i++) {
                driverNames[i] = drivers[i].getName(uow);
            }
            cachedTeiid.refreshDrivers(uow, teiidInstance, driverNames);

            // VDBs
            Vdb[] vdbs = dataService.getVdbs(uow);
            String[] vdbNames = new String[vdbs.length];
            for (int i = 0; i < vdbs.length; i++) {
                vdbNames[i] = vdbs[i].getName(uow);
            }
            cachedTeiid.refreshVdbs(uow, teiidInstance, vdbNames);

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            // Await the sequencers to finish
            awaitCallback(uow);

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private String getSchema(UnitOfWork uow, String vdbName, String modelName) throws Exception {
        Teiid teiidNode = getDefaultTeiid();
        
        TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
        if (teiidInstance == null) {
            throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_ERROR));
        }

        teiidInstance.connect();
        if (! teiidInstance.isConnected()) {
            throw new KException(Messages.getString(Messages.Relational.TEIID_INSTANCE_CONNECTION_ERROR));
        }
        
        return teiidInstance.getSchema(vdbName, 1, modelName);
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
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_EMPTY_CREDENTIAL_ERROR);
        }

        return Response.ok().build();
    }

    private Response checkFileAttributes(KomodoFileAttributes kfa, List<MediaType> mediaTypes) throws Exception {
        if (kfa == null || (kfa.getName() == null && kfa.getContent() == null))
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_PARAMETERS);

        if (kfa.getName() == null)
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_NAME);

        if (kfa.getContent() == null)
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_FILE_ATTRIB_NO_CONTENT);

        return Response.ok().build();
    }

    private boolean hasDataSourceDriver(String driverName, Teiid teiidNode) throws Exception {

        UnitOfWork uow = null;
        boolean hasDriver = false;

        try {
            uow = systemTx("refresh-teiid-content", true);
            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.reconnect();

            Collection<DataSourceDriver> drivers = teiidInstance.getDataSourceDrivers();
            for (DataSourceDriver driver : drivers) {
                if (driver.getName().startsWith(driverName)) {
                    hasDriver = true;
                    break;
                }
            }

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            return hasDriver;

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }
    
    private boolean hasDynamicVdb(String vdbName, Teiid teiidNode) throws Exception {
        UnitOfWork uow = null;
        boolean hasVdb = false;

        try {
            uow = systemTx("refresh-teiid-content", true);
            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.reconnect();

            Collection<TeiidVdb> vdbs = teiidInstance.getVdbs();
            for (TeiidVdb vdb : vdbs) {
                if (vdb.getName().startsWith(vdbName)) {
                    hasVdb = true;
                    break;
                }
            }

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            return hasVdb;

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
    }

    private boolean hasDataSource(String dataSourceName, Teiid teiidNode) throws Exception {
        UnitOfWork uow = null;
        boolean hasDataSource = false;

        try {
            uow = systemTx("refresh-teiid-content", true);
            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.reconnect();

            Collection<TeiidDataSource> datasources = teiidInstance.getDataSources();
            for (TeiidDataSource datasource : datasources) {
                if (datasource.getName().startsWith(dataSourceName)) {
                    hasDataSource = true;
                    break;
                }
            }

            // Commit the transaction to allow the sequencers to run
            uow.commit();

            return hasDataSource;

        } catch (KException ex) {
            KEngine.getInstance().getErrorHandler().error(ex);
            if (uow != null)
                uow.rollback();

            throw ex;
        }
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
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "getTeiidStatus", true); //$NON-NLS-1$
            RestTeiidStatus status = new RestTeiidStatus(uriInfo.getBaseUri(), teiidNode, uow);

            // create response
            return commit(uow, mediaTypes, status);

        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_STATUS_ERROR);
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_VDBS_STATUS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param credentialAttributes
     *        the credential attributes (never <code>null</code>)
     * @return a JSON document representing results (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem setting credentials
     */
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
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
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

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_SET_CREDENTIALS_ERROR);
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_VDBS_ERROR);
        }
    }

    /**
     * Get the specified VDB
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

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

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_VDB_ERROR, vdbName);
        }
    }
    
    /**
     * Copy a VDBs from the server into the workspace that are not present in the workspace 
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON representation of the new datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataSource
     */
    @POST
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDBS_FROM_TEIID )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Copy VDBs from the server into the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response copyVdbsIntoRepo( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find VDB
            uow = createTransaction(principal, "vdbsFromTeiid", false); //$NON-NLS-1$
            Vdb[] serverVdbs = cachedTeiid.getVdbs(uow);
            
            boolean importError = false;
            if(serverVdbs.length>0) {
                // Get current list of workspace Vdb names
                Vdb[] workspaceVdbs = getWorkspaceManager(uow).findVdbs( uow );
                List<String> workspaceVdbNames = new ArrayList<String>(workspaceVdbs.length);
                for(Vdb workspaceVdb : workspaceVdbs) {
                    workspaceVdbNames.add(workspaceVdb.getName(uow));
                }

                // Copy the server VDB into the workspace, if no workspace VDB with the same name
                for(Vdb serverVdb : serverVdbs) {
                    if(!workspaceVdbNames.contains(serverVdb.getName(uow))) {
                        // Get server VDB content
                        byte[] vdbXml = serverVdb.export(uow, null);
                        InputStream vdbStream = new ByteArrayInputStream(vdbXml);

                        // Import to create a new Vdb in the workspace
                        VdbImporter importer = new VdbImporter(this.repo);
                        ImportOptions options = new ImportOptions();
                        ImportMessages importMessages = new ImportMessages();
                        importer.importVdb(uow, vdbStream, this.repo.komodoWorkspace(uow), options, importMessages);

                        if(importMessages.hasError()) {
                            LOGGER.debug("importVDB for '{0}' failed", serverVdb.getName(uow)); //$NON-NLS-1$
                            importError = true;
                        }
                    }
                }
            }
            
            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_TO_REPO_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if(!importError) 
                status.addAttribute("copyVdbsToRepo", RelationalMessages.getString(RelationalMessages.Info.VDB_TO_REPO_SUCCESS)); //$NON-NLS-1$
            else
                status.addAttribute("copyVdbsToRepo", RelationalMessages.getString(RelationalMessages.Error.VDB_TO_REPO_IMPORT_ERROR)); //$NON-NLS-1$

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_TO_REPO_IMPORT_ERROR);
        }
    }
    
    /**
     * Creates or updates a workspace VDB model using DDL from the teiid VDB model.
     * If the target VDB does not exist, it is created.  If the specified model already exists, it is replaced - otherwise a new model is created.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbUpdateAttributes
     *        the attributes for the update (cannot be empty)
     * @return a JSON representation of the updated dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @POST
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.MODEL_FROM_TEIID_DDL )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Creates or updates a workspace vdb model using teiid model ddl",
                  notes = "Syntax of the json request body is of the form " +
                          "{ vdbName='workspace Vdb', modelName='workspace Model', teiidVdb='teiid VDB', teiidModel='teiid Model' }")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateModelFromDdl( final @Context HttpHeaders headers,
            final @Context UriInfo uriInfo,
            final String vdbUpdateAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Get the attributes for doing the vdb update
        KomodoVdbUpdateAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(vdbUpdateAttributes, KomodoVdbUpdateAttributes.class);
            Response response = checkVdbUpdateAttributes(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_UPDATE_REQUEST_PARSING_ERROR);
        }

        // Inputs for updating.  The update info is obtained from the Attributes passed in.
        String vdbName = attr.getVdbName();
        // Error if the Vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_MISSING_VDBNAME);
        }

        String modelName = attr.getModelName();
        // Error if the Model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_MISSING_MODELNAME);
        }

        String teiidVdbName = attr.getTeiidVdbName();
        // Error if the Teiid Vdb name is missing
        if (StringUtils.isBlank( teiidVdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_MISSING_TEIID_VDBNAME);
        }

        String teiidModelName = attr.getTeiidModelName();
        // Error if the Teiid Model name is missing
        if (StringUtils.isBlank( teiidModelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_MISSING_TEIID_MODELNAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateVdb", false ); //$NON-NLS-1$

            // Get the DDL from the Teiid Model
            String modelDdl;
            try {
                modelDdl = getSchema(uow, teiidVdbName, teiidModelName);
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_DDL_FETCH_ERROR, teiidVdbName, teiidModelName);
            }
            // Error if the Model DDL is missing
            if (StringUtils.isBlank( modelDdl )) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_DDL_DNE);
            }
            
            // Check for existence of Dataservice, Table and ModelSource before continuing...
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);

            // Check for existence of VDB.  If VDB does not exist, create it.
            Vdb vdb = null;
            if ( !wkspMgr.hasChild( uow, vdbName ) ) {
                vdb = wkspMgr.createVdb(uow, null, vdbName, vdbName);
            } else {
                KomodoObject kobject = wkspMgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
                vdb = wkspMgr.resolve( uow, kobject, Vdb.class );
            }

            // Check for existence of Model and replace if found
            Model[] models = vdb.getModels(uow, modelName);
            for(Model model : models) {
                model.remove(uow);
            }
            Model newModel = vdb.addModel(uow, modelName);
            newModel.setModelDefinition(uow, modelDdl);

            KomodoStatusObject kso = new KomodoStatusObject("Update Vdb Status"); //$NON-NLS-1$
            kso.addAttribute(vdbName, "Successfully updated"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_UPDATE_ERROR);
        }
    }

    private Response checkVdbUpdateAttributes(KomodoVdbUpdateAttributes attr,
                                              List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getVdbName() == null || attr.getModelName() == null || attr.getVdbName() == null || attr.getModelName() == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_UPDATE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }
        
    /**
     * Remove a VDB from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the dynamic VDB name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the VDB
     */
    @DELETE
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a Vdb from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeVdb(final @Context HttpHeaders headers,
                              final @Context UriInfo uriInfo,
                              @ApiParam(value = "Name of the VDB to be removed", required = true)
                              final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "unDeployTeiidDriver", false); //$NON-NLS-1$

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.undeployDynamicVdb(vdbName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (! hasDynamicVdb(vdbName, teiidNode)) {
                // Make sure Vdb state is current in the cachedTeiid
                refreshCachedVdbs(teiidNode, vdbName);

                status.addAttribute(vdbName,
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_UNDEPLOYED));
            } else
                status.addAttribute(vdbName,
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_UNDEPLOYMENT_REQUEST_SENT));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_UNDEPLOY_VDB_ERROR, vdbName);
        }
    }
    
    /**
     * Remove a DataSource from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataSourceName
     *        the DataSource name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the DataSource
     */
    @DELETE
    @Path( V1Constants.DATA_SOURCES_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SOURCE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a DataSource from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeDataSource(final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam(value = "Name of the DataSource to be removed", required = true)
                                     final @PathParam( "dataSourceName" ) String dataSourceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "removeDataSource", false); //$NON-NLS-1$

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.deleteDataSource(dataSourceName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (! hasDataSource(dataSourceName, teiidNode)) {
                // Make sure DataSource state is current in cachedTeiid
                refreshCachedDataSources(teiidNode, dataSourceName);

                status.addAttribute(dataSourceName,
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_SUCCESSFULLY_UNDEPLOYED));
            } else
                status.addAttribute(dataSourceName,
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_UNDEPLOYMENT_REQUEST_SENT));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_UNDEPLOY_VDB_ERROR, dataSourceName);
        }
    }
    
    /**
     * Get the schema for a model in a deployed VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB  (cannot be empty)
     * @param modelName
     *        the id of the Model (cannot be empty)
     * @return the VDB model ddl (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem retrieving the schema
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.SCHEMA_SEGMENT )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get schema for a VDB Model")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbModelSchema( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(value = "Id of the vdb", required = true)
                                       final @PathParam( "vdbName" ) String vdbName,
                                       @ApiParam(value = "Id of the model", required = true)
                                       final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        
        UnitOfWork uow = null;
        try {
            // Get the model schema
            uow = createTransaction(principal, "getModelSchema", true); //$NON-NLS-1$

            String schema = getSchema(uow, vdbName, modelName);

            KomodoStatusObject kso = new KomodoStatusObject("VdbModelSchema"); //$NON-NLS-1$
            kso.addAttribute("schema", schema); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_VDB_ERROR, vdbName);
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TRANSLATORS_ERROR);
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find data sources
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCES_ERROR);
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_ERROR, datasourceName);
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
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DRIVERS_ERROR);
        }
    }

    /**
     * Adds (deploys) a Driver to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param fileAttributes
     *        the file attributes (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Driver
     */
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
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
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
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            // Make sure Driver state is current in the cachedTeiid
            refreshCachedDrivers(teiidNode, kfa.getName());

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (hasDataSourceDriver(kfa.getName(), teiidNode))
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

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_DRIVER_ERROR, kfa.getName());
        }
    }

    /**
     * Remove a Driver from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param driverName
     *        the driver name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the Driver
     */
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

            uow = createTransaction(principal, "unDeployTeiidDriver", false); //$NON-NLS-1$

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.undeployDriver(driverName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (! hasDataSourceDriver(driverName, teiidNode)) {
                refreshCachedDrivers(teiidNode, driverName);

                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED));
            } else
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_UNDEPLOY_DRIVER_ERROR, driverName);
        }
    }

    /**
     * Adds (deploys) a Dataservice to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Dataservice
     */
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
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_DATA_SERVICE_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidDataservice", false); //$NON-NLS-1$

            List<KomodoObject> dataServices = this.repo.searchByPath(uow, kpa.getPath());
            if (dataServices.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND);
            }

            Dataservice dataService = getWorkspaceManager(uow).resolve(uow, dataServices.get(0), Dataservice.class);
            if (dataService == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND);
            }

            //
            // Deploy the data service
            //
            DeployStatus deployStatus = dataService.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            // Make sure Dataservice constituents are current in the cachedTeiid
            refreshCachedFromDataService(teiidNode, dataService);

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

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_DATA_SERVICE_ERROR);
        }
    }

    /**
     * Adds (deploys) a DataSource to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the DataSource
     */
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
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_DATA_SOURCE_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidDatasource", false); //$NON-NLS-1$

            List<KomodoObject> dataSources = this.repo.searchByPath(uow, kpa.getPath());
            if (dataSources.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);
            }

            Datasource dataSource = getWorkspaceManager(uow).resolve(uow, dataSources.get(0), Datasource.class);
            if (dataSource == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);
            }

            //
            // Deploy the data source
            //
            DeployStatus deployStatus = dataSource.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            // Make sure Datasource is current in the CachedTeiid
            refreshCachedDataSources(teiidNode, dataSource.getJndiName(uow));
            
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

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_DATA_SOURCE_ERROR);
        }
    }
    
    /**
     * Adds (deploys) a VDB to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path attribute (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the VDB
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.VDB_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the Vdb to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addVdb(final @Context HttpHeaders headers,
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
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_VDB_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployVdb", false); //$NON-NLS-1$

            List<KomodoObject> vdbs = this.repo.searchByPath(uow, kpa.getPath());
            if (vdbs.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_VDB_FOUND);
            }

            Vdb vdb = getWorkspaceManager(uow).resolve(uow, vdbs.get(0), Vdb.class);
            if (vdb == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_VDB_FOUND);
            }

            //
            // Deploy the VDB
            //
            DeployStatus deployStatus = vdb.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);
            
            // Make sure Vdb is current in the CachedTeiid
            refreshCachedVdbs(teiidNode, vdb.getName(uow));

            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(vdb.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(vdb.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_VDB_ERROR);
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

    /**
     * Query the teiid server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param queryAttribute
     *        the query attribute (never <code>null</code>)
     * @return a JSON representation of the Query results (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error getting results
     */
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
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_QUERY_MISSING_QUERY);
            }

            if (kqa.getTarget() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_QUERY_MISSING_TARGET);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
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
                return createErrorResponse(Status.FORBIDDEN, mediaTypes, RelationalMessages.Error.TEIID_SERVICE_QUERY_TARGET_NOT_DEPLOYED);
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

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, RelationalMessages.Error.TEIID_SERVICE_QUERY_ERROR, e.getLocalizedMessage());
        }
    }

    /**
     * Ping the connection to the teiid server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pingType
     *        the ping type (never <code>null</code>)
     * @return a JSON representation of the outcome (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error with the ping request
     */
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
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_PING_MISSING_TYPE);
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_QUERY_ERROR);
        }
    }
}
