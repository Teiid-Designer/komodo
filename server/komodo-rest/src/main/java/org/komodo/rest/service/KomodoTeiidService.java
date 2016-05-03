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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
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
import org.komodo.relational.datasource.Datasource;
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
import org.komodo.rest.relational.KomodoTeiidAttributes;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.RestTeiid;
import org.komodo.rest.relational.RestTeiidStatus;
import org.komodo.rest.relational.RestTeiidVdbStatus;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
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
     * @throws ServerErrorException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoTeiidService(final KEngine engine) throws ServerErrorException {
        super(engine);
    }

    private synchronized Teiid getDefaultTeiid() throws KException {
        ServerManager serverManager = ServerManager.getInstance(repo);
        UnitOfWork uow = null;

        try {
            uow = createTransaction("getTeiidStatus", false); //$NON-NLS-1$
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
        UnitOfWork uow = createTransaction("import-teiid-content", false, callback); //$NON-NLS-1$
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
                                                                           List<MediaType> mediaTypes) {
        if (adminUser == null || adminPasswd == null || jdbcUser == null || jdbcPasswd == null) {
            String errorMessage = RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_EMPTY_CREDENTIAL_ERROR);

            Object responseEntity = createErrorResponseEntity(mediaTypes, errorMessage);
            return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
        }

        return Response.ok().build();
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

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiid = getDefaultTeiid();

            uow = createTransaction("getTeiidStatus", true); //$NON-NLS-1$
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

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        Teiid teiid = null;

        try {
            teiid = getDefaultTeiid();

            uow = createTransaction("getTeiidVdbs", true); //$NON-NLS-1$
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
                                                                                        mediaTypes);

            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponse(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction("teiidSetCredentials", true); //$NON-NLS-1$

            teiidNode.setAdminUser(uow, teiidAttrs.getAdminUser());
            teiidNode.setAdminPassword(uow, teiidAttrs.getAdminPasswd());
            teiidNode.setJdbcUsername(uow, teiidAttrs.getJdbcUser());
            teiidNode.setJdbcPassword(uow, teiidAttrs.getJdbcPasswd());

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
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Display the collection of vdbs",
                            response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbs(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find VDBs
            uow = createTransaction("getVdbs", true); //$NON-NLS-1$

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
            CachedTeiid cachedTeiid = importContent();

            // find VDB
            uow = createTransaction("getVdb-" + vdbName, true); //$NON-NLS-1$
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
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Display the collection of translators",
                            response = RestVdbTranslator[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslators(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find translators
            uow = createTransaction("getTranslators", true); //$NON-NLS-1$

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
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Display the collection of data sources",
                            response = RestDataSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataSources(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            CachedTeiid cachedTeiid = importContent();

            // find translators
            uow = createTransaction("getDataSources", true); //$NON-NLS-1$

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

            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TRANSLATORS_ERROR);
        }
    }
}
