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
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.naming.InitialContext;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
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
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.relational.vdb.ModelSource;
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
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.connection.RestConnectionJdbcInfo;
import org.komodo.rest.relational.connection.RestTemplate;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoDataSourceJdbcTableAttributes;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.request.KomodoVdbUpdateAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.rest.relational.response.RestTeiidDataSourceJdbcCatalogSchemaInfo;
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
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.ExecutionAdmin;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
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

    private static final String TABLE_NAME = "TABLE_NAME"; //$NON-NLS-1$
    private static final String CATALOG = "Catalog"; //$NON-NLS-1$
    private static final String SCHEMA = "Schema"; //$NON-NLS-1$

    private InitialContext initialContext;

    /**
     * Default translator mappings for different drivers
     */
    private final static String DRIVER_TRANSLATOR_MAPPING_FILE = "driverTranslatorMappings.xml"; //$NON-NLS-1$

    /**
     * Default translator mappings for connection URL content
     */
    private final static String URLCONTENT_TRANSLATOR_MAPPING_FILE = "urlContentTranslatorMappings.xml"; //$NON-NLS-1$

    /**
     * Translator mapping file elements and attributes
     */
    private final static String ELEM_TRANSLATOR = "translator"; //$NON-NLS-1$
    private final static String ATTR_DRIVER = "driver"; //$NON-NLS-1$
    private final static String ATTR_URLCONTENT = "urlcontent"; //$NON-NLS-1$

    /**
     * Unknown translator
     */
    private final static String UNKNOWN_TRANSLATOR = "unknown"; //$NON-NLS-1$

    /**
     * Time to wait after deploying/undeploying an artifact from the teiid instance
     */
    private final static int DEPLOYMENT_WAIT_TIME = 10000;

    private static final String[] PRIORITY_TEMPLATE_NAMES = {"connection-url", "user-name", "password", "port"};

    private static class TemplateEntryComparator implements Comparator<TemplateEntry> {

        private UnitOfWork transaction;

        public TemplateEntryComparator(UnitOfWork transaction) {
            this.transaction = transaction;
        }

        @Override
        public int compare(TemplateEntry entry1, TemplateEntry entry2) {
            String entry1Name = null;
            boolean entry1Advanced = false;
            String entry2Name = null;
            boolean entry2Advanced = false;

            try {
                entry1Name = entry1.getName(transaction);
                entry1Advanced = entry1.isAdvanced(transaction);

                entry2Name = entry2.getName(transaction);
                entry2Advanced = entry2.isAdvanced(transaction);
            } catch (KException e) {
                // Ignore exception. Will be dealt with below
            }

            if (entry1Name == null && entry2Name == null)
                return 0; // Not a lot
            else if (entry1Name == null)
                return -1;
            else if (entry2Name == null)
                return 1;

            if (entry1Name.equals(entry2Name))
                return 0;

            for (String name : PRIORITY_TEMPLATE_NAMES) {
                if (name.equals(entry1Name))
                    return -1;

                if (name.equals(entry2Name))
                    return 1;
            }

            if (entry1Advanced && !entry2Advanced)
                return 1; // De-prioritise advanced
            else if (! entry1Advanced && entry2Advanced)
                return -1; // De-prioritise advanced    

            return entry1Name.compareTo(entry2Name);
        }
    }

    /**
     * Mapping of driverName to default translator
     */
    private Map<String, String> driverTranslatorMap = new HashMap<String,String>();

    /**
     * Mapping of urlContent to default translator
     */
    private Map<String, String> urlContentTranslatorMap = new HashMap<String,String>();

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoTeiidService(final KEngine engine) throws WebApplicationException {
        super(engine);
        // Loads default translator mappings
        loadDriverTranslatorMap();
        loadUrlContentTranslatorMap();
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

    private synchronized void refreshCachedConnections(Teiid teiid, String... connectionNames) throws KException {
        CachedTeiid cachedTeiid = importContent(teiid);

        UnitOfWork uow = null;
        try {
            uow = systemTx("refresh-teiid-content", false);
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            cachedTeiid.refreshConnections(uow, teiidInstance, connectionNames);

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

            //
            // Templates are dependent on which drivers are present
            // so these should be refreshed as well
            //
            cachedTeiid.refreshTemplates(uow, teiidInstance);

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
            Connection[] dataSources = dataService.getConnections(uow);
            String[] dataSourceNames = new String[dataSources.length];
            for (int i = 0; i < dataSources.length; i++) {
                dataSourceNames[i] = dataSources[i].getJndiName(uow);
            }
            cachedTeiid.refreshConnections(uow, teiidInstance, dataSourceNames);

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
        
        return teiidInstance.getSchema(vdbName, "1", modelName);
    }

    private Response createTimeoutResponse(List<MediaType> mediaTypes) {
        Object responseEntity = createErrorResponseEntity(mediaTypes,
                                                                  RelationalMessages.getString(
                                                                                               RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT));
        return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
    }

    private Response checkTeiidAttributes(String adminUser, String adminPasswd, int adminPort,
                                                                           String jdbcUser, String jdbcPasswd, int jdbcPort,
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

            Collection<ConnectionDriver> drivers = teiidInstance.getDataSourceDrivers();
            for (ConnectionDriver driver : drivers) {
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
                                   @ApiParam(
                                             value = "" + 
                                                     "JSON of credentials:" + BR +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "adminUser: \"admin user name\"" + COMMA + BR +
                                                     NBSP + "adminPasswd: \"admin password\"" + COMMA + BR +
                                                     NBSP + "adminPort: \"admin port\"" + COMMA + BR +
                                                     NBSP + "adminSecure: \"true if admin uses secure connection\"" + COMMA + BR +
                                                     NBSP + "jdbcUser: \"jdbc user name\"" + COMMA + BR +
                                                     NBSP + "jdbcPasswd: \"jdbc password\"" + COMMA + BR +
                                                     NBSP + "jdbcPort: \"jdbc port\"" + COMMA + BR +
                                                     NBSP + "jdbcSecure: \"true if jdbc uses secure connection\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
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
                                                                                        teiidAttrs.getAdminPort(),
                                                                                        teiidAttrs.getJdbcUser(),
                                                                                        teiidAttrs.getJdbcPasswd(),
                                                                                        teiidAttrs.getJdbcPort(),
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

            String adminUser = teiidAttrs.getAdminUser();
            if (adminUser != null && ! adminUser.equals(teiidNode.getAdminUser(uow)))
                teiidNode.setAdminUser(uow, adminUser);

            String adminPasswd = teiidAttrs.getAdminPasswd();
            if (adminPasswd != null && ! adminPasswd.equals(teiidNode.getAdminPassword(uow)))
                teiidNode.setAdminPassword(uow, adminPasswd);

            int adminPort = teiidAttrs.getAdminPort();
            if (adminPort > -1 && adminPort != (teiidNode.getAdminPort(uow)))
                teiidNode.setAdminPort(uow, adminPort);

            Boolean adminSecure = teiidAttrs.isAdminSecure();
            if (adminSecure != null && ! adminSecure.equals(teiidNode.isAdminSecure(uow)))
                teiidNode.setAdminSecure(uow, adminSecure);

            String jdbcUser = teiidAttrs.getJdbcUser();
            if (jdbcUser != null && ! jdbcUser.equals(teiidNode.getJdbcUsername(uow)))
                teiidNode.setJdbcUsername(uow, jdbcUser);

            String jdbcPasswd = teiidAttrs.getJdbcPasswd();
            if (jdbcPasswd != null && ! jdbcPasswd.equals(teiidNode.getJdbcPassword(uow)))
                teiidNode.setJdbcPassword(uow, jdbcPasswd);

            int jdbcPort = teiidAttrs.getJdbcPort();
            if (jdbcPort > -1 && jdbcPort != (teiidNode.getJdbcPort(uow)))
                teiidNode.setJdbcPort(uow, jdbcPort);

            Boolean jdbcSecure = teiidAttrs.isJdbcSecure();
            if (teiidAttrs.isJdbcSecure() != null && ! jdbcSecure.equals(teiidNode.isJdbcSecure(uow)))
                teiidNode.setJdbcSecure(uow, jdbcSecure);

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
     * @return a JSON representation of the status of the copy (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error copying the vdbs
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
            refreshCachedVdbs(teiidNode);
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find VDB
            uow = createTransaction(principal, "vdbsFromTeiid", false); //$NON-NLS-1$
            Vdb[] serverVdbs = cachedTeiid.getVdbs(uow);
            
            boolean importError = false;
            if(serverVdbs.length>0) {
                // Get current list of workspace Vdbs
                final WorkspaceManager mgr = getWorkspaceManager(uow);
                Vdb[] workspaceVdbs = mgr.findVdbs( uow );
                List<String> workspaceVdbNames = new ArrayList<String>(workspaceVdbs.length);
                
                // Remove any service source vdbs that dont belong to user.  Compile list of remaining workspace vdbs
                for(Vdb workspaceVdb : workspaceVdbs) {
                    // Source VDB not belonging to user are removed
                    if(workspaceVdb.hasProperty(uow, DSB_PROP_SERVICE_SOURCE)) {
                        String owner = workspaceVdb.getProperty(uow, DSB_PROP_SERVICE_SOURCE).getStringValue(uow);
                        if(!uow.getUserName().equals(owner)) {
                            mgr.delete(uow, workspaceVdb);
                        } else {
                            workspaceVdbNames.add(workspaceVdb.getName(uow));
                        }
                    } else {
                        workspaceVdbNames.add(workspaceVdb.getName(uow));
                    }
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
     * Update workspace VDBs with latest Teiid status.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON representation of the new datasource (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the DataSource
     */
    @PUT
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDBS_FROM_TEIID )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update workspace VDBs with teiid status")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateWorkspaceVdbsFromTeiid( final @Context HttpHeaders headers,
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

            // Goes directly to the server to get vdb status
            uow = createTransaction(principal, "vdbUpdateFromTeiid", false); //$NON-NLS-1$
            
            // Get list of Teiid VDBs directly from the server
            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            Collection<TeiidVdb> teiidVdbs = teiidInstance.getVdbs();
            
            // Get list of workspace VDBs
            WorkspaceManager wsMgr = getWorkspaceManager(uow);
            Vdb[] workspaceVdbs = wsMgr.findVdbs( uow );
            
            // Set status properties on the workspace VDBs, based on the matching Teiid VDB.
            for( Vdb wkspVdb : workspaceVdbs) {
                updateVdbProperties(uow, wkspVdb, teiidVdbs);
            }
            
            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_TO_REPO_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            status.addAttribute("success", "true"); //$NON-NLS-1$ //$NON-NLS-2$

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
    
    /*
     * Updates workspace vdb properties based on the corresponding teiid VDB state
     */
    private void updateVdbProperties(final UnitOfWork uow, Vdb workspaceVdb, Collection<TeiidVdb> teiidVdbs) throws KException {
        // Find server VDB which corresponds to the workspace VDB
        TeiidVdb serverVdbMatch = null;
        String wkspVdbName = workspaceVdb.getName(uow);
        for( TeiidVdb teiidVdb : teiidVdbs) {
            if(teiidVdb.getName().equals(wkspVdbName)) {
                serverVdbMatch = teiidVdb;
                break;
            }
        }
        
        // Update workspace VDB properties based on server status
        String status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_NEW);
        String statusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_MSG_NEW);
        if(serverVdbMatch!=null) {
            List<String> errors = serverVdbMatch.getValidityErrors();
            if(errors!=null && errors.size() > 0) {
                status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_ERROR);
                statusMessage = errors.get(0);
            } else if(serverVdbMatch.hasFailed()) {
                status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_ERROR);
                statusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_MSG_UNKNOWN);   
            } else if(serverVdbMatch.isActive()) {
                status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_ACTIVE);
                statusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_MSG_ACTIVE);
            } else if(serverVdbMatch.isLoading()) {
                status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_LOADING);
                statusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_MSG_LOADING);   
            } else {
                status = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_UNKNOWN);
                statusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_STATUS_MSG_UNKNOWN);
            }
        }

        // Sets sourceConnection property for serviceSource VDBs
        if(workspaceVdb.hasProperty(uow, DSB_PROP_SERVICE_SOURCE)) {
            Model[] models = workspaceVdb.getModels(uow);
            for(Model model : models) {
                if(model.getModelType(uow).equals(Type.PHYSICAL)) {
                    ModelSource[] modelSources = model.getSources(uow);
                    for(ModelSource modelSource : modelSources) {
                        workspaceVdb.setProperty(uow, DSB_PROP_SOURCE_CONNECTION, modelSource.getName(uow));
                        workspaceVdb.setProperty(uow, DSB_PROP_SOURCE_TRANSLATOR, modelSource.getTranslatorName(uow));
                        break;
                    }
                }
            }
        }
        
        workspaceVdb.setProperty(uow, DSB_PROP_TEIID_STATUS, status);
        workspaceVdb.setProperty(uow, DSB_PROP_TEIID_STATUS_MSG, statusMessage);
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
    @ApiOperation(value = "Creates or updates a workspace vdb model using teiid model ddl")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateModelFromDdl( final @Context HttpHeaders headers,
            final @Context UriInfo uriInfo,
            @ApiParam(
                      value = "" + 
                              "JSON of update attributes:" + BR +
                              OPEN_PRE_TAG +
                              OPEN_BRACE + BR +
                              NBSP + "vdbName: \"The destination workspace vdb name\"" + COMMA + BR +
                              NBSP + "modelName: \"The destination model name\"" + COMMA + BR +
                              NBSP + "teiidVdb: \"The source teiid vdb name\"" + COMMA + BR +
                              NBSP + "teiidModel: \"The source teiid model name containing required ddl\"" + BR +
                              NBSP + CLOSE_BRACE + BR +
                              CLOSE_BRACE +
                              CLOSE_PRE_TAG,
                      required = true
            )
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

            // Check for existence of Model.  If it exists, reset its modelDefinition.  If doesnt exist, create a new model.
            Model[] models = vdb.getModels(uow, modelName);
            Model modelToUpdate = null;
            if(models.length>0) {
                modelToUpdate = models[0];
            }
            if(modelToUpdate==null) {
                modelToUpdate = vdb.addModel(uow, modelName);
            }
            modelToUpdate.setModelDefinition(uow, modelDdl);

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

    private Response checkJdbcTableAttributes(KomodoDataSourceJdbcTableAttributes attr,
                                              List<MediaType> mediaTypes) throws Exception {

        if (attr == null || attr.getDataSourceName() == null || attr.getCatalogFilter() == null || attr.getSchemaFilter() == null || attr.getTableFilter() == null) {
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
            // updates cache for the removed vdb
            refreshCachedVdbs(teiidNode, vdbName);
            if (! hasDynamicVdb(vdbName, teiidNode)) {
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
     * Remove a Connection from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the Connection name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the Connection
     */
    @DELETE
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a Connection from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeConnection(final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam(value = "Name of the connection to be removed", required = true)
                                     final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYMENT_STATUS_TITLE);
        KomodoStatusObject status = new KomodoStatusObject(title);

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "removeConnection", false); //$NON-NLS-1$

            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);

            if (! teiidInstance.dataSourceExists(connectionName)) {
                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_NO_CONNECTION_FOUND, connectionName));
                return commit(uow, mediaTypes, status);
            }

            teiidInstance.deleteDataSource(connectionName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            if (! hasDataSource(connectionName, teiidNode)) {
                // Make sure DataSource state is current in cachedTeiid
                refreshCachedConnections(teiidNode);

                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_SUCCESSFULLY_UNDEPLOYED, connectionName));
            } else
                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_UNDEPLOYMENT_REQUEST_SENT, connectionName));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_UNDEPLOY_CONNECTION_ERROR, connectionName);
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
                                       @ApiParam(value = "Name of the vdb", required = true)
                                       final @PathParam( "vdbName" ) String vdbName,
                                       @ApiParam(value = "Name of the model", required = true)
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
     * @return a JSON document representing all the connections deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.CONNECTIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of connections",
                            response = RestConnection[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnections(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            refreshCachedConnections(teiidNode);
            CachedTeiid cachedTeiid = importContent(teiidNode);

            uow = createTransaction(principal, "getConnections", true); //$NON-NLS-1$

            // Get teiid datasources
            Connection[] dataSources = cachedTeiid.getConnections(uow);
            LOGGER.debug("getConnections:found '{0}' DataSources", dataSources.length); //$NON-NLS-1$

            final List<RestConnection> entities = new ArrayList<>();

            KomodoProperties properties = new KomodoProperties();
            for (final Connection dataSource : dataSources) {
                RestConnection entity = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
                LOGGER.debug("getConnections:Data Source '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
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
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the JSON representation of the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified connection or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find connection by name", response = RestConnection.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Name of the connection", required = true)
                                   final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        
        UnitOfWork uow = null;
        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find DataSource
            uow = createTransaction(principal, "getConnection-" + connectionName, true); //$NON-NLS-1$
            Connection dataSource = cachedTeiid.getConnection(uow, connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            KomodoProperties properties = new KomodoProperties();
            final RestConnection restDataSource = entityFactory.create(dataSource, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getConnection:Datasource '{0}' entity was constructed", dataSource.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_ERROR, connectionName);
        }
    }
    
    /**
     * Return the default translator to be used for a Connection
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the translator for the connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified connection 
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER 
           + StringConstants.FORWARD_SLASH + V1Constants.TRANSLATOR_DEFAULT_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get the default translator recommended for a connection")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionDefaultTranslator( final @Context HttpHeaders headers,
                                                    final @Context UriInfo uriInfo,
                                                    @ApiParam(value = "Id of the connection", required = true)
                                                    final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find Connection
            uow = createTransaction(principal, "getConnectionDefaultTranslator-" + connectionName, true); //$NON-NLS-1$
            Connection dataSource = cachedTeiid.getConnection(uow, connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            // Get the driver name for the source
            String driverName = dataSource.getDriverName(uow);
            
            // Get the translator name from the default driver - translator mappings
            String translatorName = driverTranslatorMap.get(driverName);
            
            // If translator not found using driver mappings, use the connection url if available.
            // The urlContentTranslatorMap keys are unique strings within the connection url which would identify the required translator
            if(translatorName==null) {
                String connectionUrl = null;
                Property urlProp = dataSource.getProperty(uow, "connection-url"); //$NON-NLS-1$
                if(urlProp!=null) {
                    connectionUrl = urlProp.getStringValue(uow);
                }
                // No connection url property - unknown translator
                if(connectionUrl==null || connectionUrl.isEmpty()) {
                    translatorName = UNKNOWN_TRANSLATOR;
                // Connection url property found - use mappings to get translator, if possible
                } else {
                    for(String contentKey : urlContentTranslatorMap.keySet()) {
                        if(connectionUrl.contains(contentKey)) {
                            translatorName = urlContentTranslatorMap.get(contentKey);
                            break;
                        }
                    }
                    if(translatorName==null) {
                        translatorName = UNKNOWN_TRANSLATOR;
                    }
                }
            }
            
            // Return a status object with the translator
            KomodoStatusObject kso = new KomodoStatusObject();
            kso.addAttribute("Translator", translatorName); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_TRANSLATOR_ERROR, connectionName);
        }
    }

    /**
     * Copy  connections from the server into the workspace that are not present in the workspace 
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON representation of the status of the copying (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error copying the connections
     */
    @POST
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTIONS_FROM_TEIID )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Copy Connections from the server into the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response copyConnectionsIntoRepo( final @Context HttpHeaders headers,
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
            refreshCachedConnections(teiidNode);
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find Connections
            uow = createTransaction(principal, "connectionsFromTeiid", false); //$NON-NLS-1$
            Connection[] teiidConns = cachedTeiid.getConnections(uow);
            
            // Get current list of workspace Connections
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            Connection[] workspaceConns = mgr.findConnections( uow );
            List<String> workspaceConnNames = new ArrayList<String>(workspaceConns.length);

            String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_TO_REPO_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            // Remove any connections that dont belong to user.  Compile list of remaining workspace connections
            for(Connection workspaceConn : workspaceConns) {
                // Connections not belonging to user are removed
                if(workspaceConn.hasProperty(uow, DSB_PROP_SERVICE_SOURCE)) {
                    String owner = workspaceConn.getProperty(uow, DSB_PROP_SERVICE_SOURCE).getStringValue(uow);
                    if(! uow.getUserName().equals(owner)) {
                        mgr.delete(uow, workspaceConn);
                        continue;
                    }
                }

                workspaceConnNames.add(workspaceConn.getName(uow));
            }
                
            // Copy the teiid connection into the workspace, if no workspace connection with the same name
            for(Connection teiidConn : teiidConns) {
                String name = teiidConn.getName(uow);

                if(workspaceConnNames.contains(name))
                    continue;

                final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, name);
                final RestConnection teiidConnEntity = entityFactory.create(teiidConn, uriInfo.getBaseUri(), uow);

                setProperties(uow, connection, teiidConnEntity);
            }

            status.addAttribute("copyConnsToRepo", RelationalMessages.getString(RelationalMessages.Info.CONNECTION_TO_REPO_SUCCESS)); //$NON-NLS-1$
           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_TO_REPO_IMPORT_ERROR);
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
    @ApiOperation(value = "Display the collection of drivers available in teiid",
                            response = RestConnectionDriver[].class)
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

            final List<RestConnectionDriver> entities = new ArrayList<>();

            for (final Driver driver : drivers) {
                RestConnectionDriver entity = new RestConnectionDriver();
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
     * @param driverAttributes
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
                                   @ApiParam(
                                             value = "" + 
                                                     "JSON of the properties of the driver to add:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "name: \"name of the driver\"" + COMMA + BR +
                                                     NBSP + "content: \"Base64-encoded byte data of the" + COMMA + BR +
                                                     NBSP + "driver file\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
                                   final String driverAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;
        String driverName = null;
        byte[] driverContent = null;

        try {
            uow = createTransaction(principal, "deployTeiidDriver", false); //$NON-NLS-1$

            if (driverAttributes.contains(KomodoPathAttribute.PATH_LABEL)) {
                // Is a workspace path to a driver
                try {
                    KomodoPathAttribute kpa = KomodoJsonMarshaller.unmarshall(driverAttributes, KomodoPathAttribute.class);
                    if (kpa.getPath() == null) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_DRIVER_MISSING_PATH);
                    }

                    List<KomodoObject> results = this.repo.searchByPath(uow, kpa.getPath());
                    if (results.size() == 0) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DRIVER_FOUND_IN_WKSP, kpa.getPath());
                    }

                    Driver driver = getWorkspaceManager(uow).resolve(uow, results.get(0), Driver.class);
                    if (driver == null) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DRIVER_FOUND_IN_WKSP, kpa.getPath());
                    }

                    driverName = driver.getName(uow);
                    driverContent = FileUtils.streamToByteArray(driver.getContent(uow));

                } catch (Exception ex) {
                    return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
                }

            } else {
                // Is a set of file attributes for file-based with content encoded
                try {
                    KomodoFileAttributes kfa = KomodoJsonMarshaller.unmarshall(driverAttributes, KomodoFileAttributes.class);
                    Response response = checkFileAttributes(kfa, mediaTypes);
                    if (response.getStatus() != Status.OK.getStatusCode())
                        return response;

                    driverName = kfa.getName();
                    driverContent = decode(kfa.getContent());

                } catch (Exception ex) {
                    return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
                }
            }

            if (driverName == null || driverContent == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_DRIVER_ATTRIBUTES_MISSING);
            }

            String tempDir = FileUtils.tempDirectory();
            String fileName = driverContent.hashCode() + DOT + driverName;
            File driverFile = new File(tempDir, fileName);
            FileUtils.write(driverContent, driverFile);

            Teiid teiidNode = getDefaultTeiid();
            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
            teiidInstance.deployDriver(driverName, driverFile);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            // Make sure Driver state is current in the cachedTeiid
            refreshCachedDrivers(teiidNode, driverName);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());

            if (hasDataSourceDriver(driverName, teiidNode)) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED));
            } else
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UPLOADED));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_DRIVER_ERROR, driverName);
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
     * Return Dataservice deployability status.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice being requested for deployment (cannot be empty)
     * @return the deployable status for the Dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem determining the status 
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER 
           + StringConstants.FORWARD_SLASH + V1Constants.DEPLOYABLE_STATUS_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get deployable status for a dataservice", response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataserviceDeployableStatus( final @Context HttpHeaders headers,
                                                    final @Context UriInfo uriInfo,
                                                    @ApiParam(value = "Id of the data service", required = true)
                                                    final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        java.sql.Connection connection = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            uow = createTransaction(principal, "getDeployableStatus", true); //$NON-NLS-1$

            // Get the dataservice from the users repo
            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);
            

            // Get the serviceVDB name associated with the dataservice
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            String serviceVdbName = serviceVdb.getName(uow);

            // Get the already-deployed VDB on the server if available
            Vdb serverVdb = cachedTeiid.getVdb(uow, serviceVdbName);

            String deployableStatusMessage = "OK"; //$NON-NLS-1$
            // If found, determine if there is a conflict with the VDB
            if( serverVdb != null ) {
                if(serverVdb.hasProperty(uow, DSB_PROP_OWNER)) {
                    String serverVdbOwner = serverVdb.getProperty(uow, DSB_PROP_OWNER).getStringValue(uow);
                    // server vdb owner is different than the current user.
                    if(!serverVdbOwner.equals(uow.getUserName())) {
                        deployableStatusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_ALREADY_DEPLOYED_OWNER, serviceVdbName, serverVdbOwner);    
                    }
                } else {
                    deployableStatusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_ALREADY_DEPLOYED, serviceVdbName);    
                }
            }

            // The KomodoStatusObject returns 'OK' if OK to deploy, otherwise a message describing the issue
            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYABLE_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            status.addAttribute("deployableStatus", deployableStatusMessage); //$NON-NLS-1$

           return commit(uow, mediaTypes, status);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SERVICE_DEPLOYABLE_ERROR, dataserviceName);
        } finally {
            try {
                if(connection!=null) {
                    connection.close();
                }
            } catch (SQLException ex) {
            }
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
                                   @ApiParam(
                                             value = "" + 
                                                     "JSON of the properties of the data service:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "path: \"location of the data service in the workspace\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
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
                return createErrorResponseWithForbidden(mediaTypes,
                                                        RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND,
                                                        StringUtils.getLastToken(kpa.getPath(), FORWARD_SLASH));
            }

            Dataservice dataService = getWorkspaceManager(uow).resolve(uow, dataServices.get(0), Dataservice.class);
            if (dataService == null) {
                return createErrorResponseWithForbidden(mediaTypes,
                                                        RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SERVICE_FOUND,
                                                        StringUtils.getLastToken(kpa.getPath(), FORWARD_SLASH));
            }

            //
            // Deploy the data service
            //
            DeployStatus deployStatus = dataService.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

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

            try {
                // Make sure Dataservice constituents are current in the cachedTeiid
                refreshCachedFromDataService(teiidNode, dataService);
            } catch (Exception ex) {
                status.addAttribute("RefreshError", ex.getLocalizedMessage());
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
     * Adds (deploys) a Connection to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Connection
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.CONNECTION_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the connection to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" + 
                                                     "JSON of the properties of the connection:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "path: \"location of the connection in the workspace\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
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
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_CONNECTION_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();

            uow = createTransaction(principal, "deployTeiidConnection", false); //$NON-NLS-1$

            List<KomodoObject> connections = this.repo.searchByPath(uow, kpa.getPath());
            if (connections.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_CONNECTION_FOUND);
            }

            Connection connection = getWorkspaceManager(uow).resolve(uow, connections.get(0), Connection.class);
            if (connection == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_CONNECTION_FOUND);
            }

            //
            // Deploy the connection
            //
            DeployStatus deployStatus = connection.deploy(uow, teiidNode);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            // Make sure Connection is current in the CachedTeiid
            refreshCachedConnections(teiidNode, connection.getJndiName(uow));

            String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(connection.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(connection.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_CONNECTION_ERROR, kpa.getPath());
        }
    }

//
//    TODO
//
//    Cannot actually use this at the moment since the removal and addition
//    of a datasource during the same runtime fails with the error:
//
//    TEIID70006 {"WFLYCTL0062: Composite operation failed and was rolled back
//    Steps that failed:" => {"Operation step-1" => "WFLYCTL0158: Operation handler
//    failed: org.jboss.msc.service.DuplicateServiceException: Service
//    org.wildfly.data-source.{DatasourceName} is already registered"}}
//
//    Cannot refresh teiid either since its not part of the API
//    see https://issues.jboss.org/browse/TEIID-4592
//    
//    /**
//     * Updates a Connection on the server (deletes then adds)
//     * @param headers
//     *        the request headers (never <code>null</code>)
//     * @param uriInfo
//     *        the request URI information (never <code>null</code>)
//     * @param pathAttribute
//     *        the path (never <code>null</code>)
//     * @return a JSON representation of the status (never <code>null</code>)
//     * @throws KomodoRestException
//     *         if there is an error adding the Connection
//     */
//    @SuppressWarnings( "nls" )
//    @PUT
//    @Path(V1Constants.CONNECTION_SEGMENT)
//    @Produces( MediaType.APPLICATION_JSON )
//    @Consumes ( { MediaType.APPLICATION_JSON } )
//    @ApiOperation(value = "Updates the connection on the teiid server")
//    @ApiResponses(value = {
//        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
//        @ApiResponse(code = 403, message = "An error has occurred.")
//    })
//    public Response updateConnection( final @Context HttpHeaders headers,
//                                   final @Context UriInfo uriInfo,
//                                   @ApiParam(
//                                             value = "" + 
//                                                     "JSON of the properties of the connection:<br>" +
//                                                     OPEN_PRE_TAG +
//                                                     OPEN_BRACE + BR +
//                                                     NBSP + "path: \"location of the connection in the workspace\"" + BR +
//                                                     CLOSE_BRACE +
//                                                     CLOSE_PRE_TAG,
//                                             required = true
//                                   )
//                                   final String pathAttribute)
//                                   throws KomodoRestException {
//
//        SecurityPrincipal principal = checkSecurityContext(headers);
//        if (principal.hasErrorResponse())
//            return principal.getErrorResponse();
//
//        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
//        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
//            return notAcceptableMediaTypesBuilder().build();
//
//        //
//        // Error if there is no path attribute defined
//        //
//        KomodoPathAttribute kpa;
//        try {
//            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
//            if (kpa.getPath() == null) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_DATA_SOURCE_MISSING_PATH);
//            }
//        } catch (Exception ex) {
//            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_REQUEST_PARSING_ERROR);
//        }
//
//        UnitOfWork uow = null;
//        try {
//            Teiid teiidNode = getDefaultTeiid();
//
//            uow = createTransaction(principal, "updateTeiidConnection", false); //$NON-NLS-1$
//
//            TeiidInstance teiidInstance = teiidNode.getTeiidInstance(uow);
//
//            List<KomodoObject> dataSources = this.repo.searchByPath(uow, kpa.getPath());
//            if (dataSources.size() == 0) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);
//            }
//
//            Connection dataSource = getWorkspaceManager(uow).resolve(uow, dataSources.get(0), Connection.class);
//            if (dataSource == null) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_NO_DATA_SOURCE_FOUND);
//            }
//
//            //
//            // If connection exists then remove it first
//            //
//            String connectionName = dataSource.getName(uow);
//            if (teiidInstance.dataSourceExists(connectionName)) {
//                teiidInstance.deleteDataSource(connectionName);
//                Thread.sleep(DEPLOYMENT_WAIT_TIME);
//            }
//
//            //
//            // Deploy the data source
//            //
//            DeployStatus deployStatus = dataSource.deploy(uow, teiidNode);
//
//            // Await the deployment to end
//            Thread.sleep(DEPLOYMENT_WAIT_TIME);
//
//            // Make sure Datasource is current in the CachedTeiid
//            refreshCachedDataSources(teiidNode);
//
//            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYMENT_STATUS_TITLE);
//            KomodoStatusObject status = new KomodoStatusObject(title);
//
//            List<String> progressMessages = deployStatus.getProgressMessages();
//            for (int i = 0; i < progressMessages.size(); ++i) {
//                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
//            }
//
//            if (deployStatus.ok()) {
//                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
//                status.addAttribute(dataSource.getName(uow),
//                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_SUCCESSFULLY_DEPLOYED));
//            } else {
//                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
//                List<String> errorMessages = deployStatus.getErrorMessages();
//                for (int i = 0; i < errorMessages.size(); ++i) {
//                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
//                }
//
//                status.addAttribute(dataSource.getName(uow),
//                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYED_WITH_ERRORS));
//            }
//
//           return commit(uow, mediaTypes, status);
//
//        } catch (final Exception e) {
//            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
//                uow.rollback();
//            }
//
//            if (e instanceof KomodoRestException) {
//                throw (KomodoRestException)e;
//            }
//
//            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_DEPLOY_DATA_SOURCE_ERROR);
//        }
//    }

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
                           @ApiParam(
                                     value = "" + 
                                             "JSON of the properties of the vdb:<br>" +
                                             OPEN_PRE_TAG +
                                             OPEN_BRACE + BR +
                                             NBSP + "path: \"location of the data service in the workspace\"" + BR +
                                             CLOSE_BRACE +
                                             CLOSE_PRE_TAG,
                                     required = true
                           )
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
    @ApiOperation(value = "Pass a query to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response query(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" + 
                                                     "JSON of the properties of the query:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "query: \"SQL formatted query to interrogate the target\"" + COMMA + BR +
                                                     NBSP + "target: \"The name of the target to be queried\"" + BR +
                                                     NBSP + OPEN_PRE_CMT + "(The target can be a vdb or data service. If the latter " +
                                                     NBSP + "then the name of the service vdb is extracted and " +
                                                     NBSP + "replaces the data service)" + CLOSE_PRE_CMT + COMMA + BR +
                                                     NBSP + "limit: Add a limit on number of results to be returned" + COMMA + BR +
                                                     NBSP + "offset: The index of the result to begin the results with" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
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
                                   @ApiParam(
                                             value = "Ping request of type 'admin' or 'jdbc'",
                                             required = true
                                   )
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
    
    /**
     * Return the table names for a teiid JDBC connection 
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param jdbcTableAttributes
     *        the attributes for fetching the tables (cannot be empty)
     * @return the JDBC table names for the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified connection 
     */
    @POST
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.TABLES_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Get table names for a jdbc connection")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionJdbcTables( final @Context HttpHeaders headers,
                                             final @Context UriInfo uriInfo,
                                             @ApiParam(
                                                       value = "" + 
                                                               "JSON of the properties of the data source jdbc tables:<br>" +
                                                               OPEN_PRE_TAG +
                                                               OPEN_BRACE + BR +
                                                               NBSP + "dataSourceName: \"data source name\"" + COMMA + BR +
                                                               NBSP + "catalogFilter: \"catalog filter\"" + COMMA + BR +
                                                               NBSP + "schemaFilter: \"schema filter\"" + COMMA + BR +
                                                               NBSP + "tableFilter: \"table filter\"" + COMMA + BR +
                                                               CLOSE_BRACE +
                                                               CLOSE_PRE_TAG,
                                                       required = true
                                             )
                                             final String jdbcTableAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        java.sql.Connection connection = null;

        // Get the attributes for fetching the tables
        KomodoDataSourceJdbcTableAttributes attr;
        try {
            attr = KomodoJsonMarshaller.unmarshall(jdbcTableAttributes, KomodoDataSourceJdbcTableAttributes.class);
            Response response = checkJdbcTableAttributes(attr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_UPDATE_REQUEST_PARSING_ERROR);
        }

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            uow = createTransaction(principal, "getConnectionJdbcTables", true); //$NON-NLS-1$

            // Get the data source (connection) from teiid
            Connection dataSource = cachedTeiid.getConnection(uow, attr.getDataSourceName());
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, attr.getDataSourceName());

            // Ensure the datasource is jdbc
            if(!dataSource.isJdbc(uow)) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_NOT_JDBC_ERROR);
            }

            // Get a connection to the jdbc source
            try {
                connection = getJdbcConnection(dataSource.getJndiName(uow));
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_CONNECTION_ERROR);
            }

            // Get the table names
            KomodoStatusObject kso = new KomodoStatusObject();
            try {
                String catFilter = attr.getCatalogFilter().isEmpty() ? null : attr.getCatalogFilter();
                String schemaFilter = attr.getSchemaFilter().isEmpty() ? null : attr.getSchemaFilter();
                String tableFilter = attr.getTableFilter();
                List<String> tableNames = getTableNames(connection, catFilter, schemaFilter, tableFilter);

                // Return a status object with the table names
                for (int i = 0; i < tableNames.size(); ++i) {
                    kso.addAttribute("Table" + (i + 1), tableNames.get(i)); //$NON-NLS-1$
                }
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_TABLE_FETCH_ERROR);
            }

            return commit(uow, mediaTypes, kso);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_TABLES_ERROR);
        } finally {
            try {
                if(connection!=null) {
                    connection.close();
                }
            } catch (SQLException ex) {
            }
        }
    }
        
    /**
     * Return the catalog and schema info for a teiid JDBC connection
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the JDBC catalog names for the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Connection 
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER 
           + StringConstants.FORWARD_SLASH + V1Constants.JDBC_CATALOG_SCHEMA_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get catalog and schema info for a jdbc connection",
                  response = RestTeiidDataSourceJdbcCatalogSchemaInfo[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDatasourceJdbcCatalogSchemaInfo( final @Context HttpHeaders headers,
                                                        final @Context UriInfo uriInfo,
                                                        @ApiParam(value = "Id of the connection", required = true)
                                                        final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        java.sql.Connection connection = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            uow = createTransaction(principal, "getConnectionJdbcTables", true); //$NON-NLS-1$

            // Get the data source (connection) from teiid
            Connection dataSource = cachedTeiid.getConnection(uow, connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            // Ensure the datasource is jdbc
            if(!dataSource.isJdbc(uow)) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_NOT_JDBC_ERROR);
            }

            // Get a connection to the jdbc source
            try {
                connection = getJdbcConnection(dataSource.getJndiName(uow));
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_CONNECTION_ERROR);
            }

            // Generate the Catalog Schema Info
            final List< RestTeiidDataSourceJdbcCatalogSchemaInfo > entities = generateCatalogSchemaInfos(connection);

            return commit(uow, mediaTypes, entities);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_CATALOG_SCHEMA_ERROR);
        } finally {
            try {
                if(connection!=null) {
                    connection.close();
                }
            } catch (SQLException ex) {
            }
        }
   }

    /*
     * Generate the list of JDBC catalog schema info using the supplied connection
     * @param connection the JDBC connection
     * @return list of Catalog Schema info
     */
    private List<RestTeiidDataSourceJdbcCatalogSchemaInfo> generateCatalogSchemaInfos(java.sql.Connection connection) throws KException {
        ArgCheck.isNotNull(connection, "connection");

        List<RestTeiidDataSourceJdbcCatalogSchemaInfo> infos = new ArrayList<RestTeiidDataSourceJdbcCatalogSchemaInfo>();
        try {
            DatabaseMetaData metaData = connection.getMetaData();
            boolean supportsCatalogs = metaData.supportsCatalogsInTableDefinitions()
                                       || metaData.supportsCatalogsInProcedureCalls()
                                       || metaData.supportsCatalogsInDataManipulation();
            boolean supportsSchemas = metaData.supportsSchemasInTableDefinitions()
                                      || metaData.supportsSchemasInDataManipulation();

            // DB supports catalogs
            if (supportsCatalogs && supportsSchemas) {
                ResultSet rs = connection.getMetaData().getCatalogs();

                // Get all the catalogs
                List<String> allCats = new ArrayList<String>();
                while (rs.next()) {
                    String catalog = rs.getString(1);
                    allCats.add(catalog);
                }
                rs.close();

                // Create mapping of catalog to schema list
                Collections.sort(allCats, String.CASE_INSENSITIVE_ORDER);
                Map<String, List<String>> catalogSchemaMap = new HashMap<String, List<String>>();
                for (String catlg : allCats) {
                    ResultSet rs2;
                    try {
                        rs2 = connection.getMetaData().getSchemas(catlg, null);
                    } catch (Exception ex) {
                        if (allCats.size() == 1) {
                            try {
                                rs2 = connection.getMetaData().getSchemas();
                            } catch (Exception ex1) {
                                continue;
                            }
                        } else {
                            continue;
                        }
                    }
                    List<String> schemaList = new ArrayList<String>();
                    while (rs2.next()) {
                        String schemaName = rs2.getString(1);
                        schemaList.add(schemaName);
                    }
                    Collections.sort(schemaList, String.CASE_INSENSITIVE_ORDER);
                    catalogSchemaMap.put(catlg, schemaList);
                    rs2.close();
                }

                // Generate the infos
                List<String> catNames = new ArrayList<String>(catalogSchemaMap.keySet());
                Collections.sort(catNames, String.CASE_INSENSITIVE_ORDER);
                for (String catName : catNames) {
                    RestTeiidDataSourceJdbcCatalogSchemaInfo info = new RestTeiidDataSourceJdbcCatalogSchemaInfo();
                    info.setItemName(catName);
                    info.setItemType(CATALOG);
                    info.setCatalogSchemaNames(catalogSchemaMap.get(catName));
                    infos.add(info);
                }
            } else if (supportsCatalogs && !supportsSchemas) {
                ResultSet resultSet = connection.getMetaData().getCatalogs();
                // Get all the catalogs
                List<String> allCats = new ArrayList<String>();
                while (resultSet.next()) {
                    String catalog = resultSet.getString(1);
                    allCats.add(catalog);
                }
                resultSet.close();
                Collections.sort(allCats, String.CASE_INSENSITIVE_ORDER);
                // Create infos
                for (String cat : allCats) {
                    RestTeiidDataSourceJdbcCatalogSchemaInfo info = new RestTeiidDataSourceJdbcCatalogSchemaInfo();
                    info.setItemName(cat);
                    info.setItemType(CATALOG);
                    infos.add(info);
                }
            } else if (supportsSchemas && !supportsCatalogs) {
                ResultSet resultSet = connection.getMetaData().getSchemas();
                // Get all the schemas
                List<String> allSchemas = new ArrayList<String>();
                while (resultSet.next()) {
                    String schema = resultSet.getString(1);
                    allSchemas.add(schema);
                }
                resultSet.close();

                Collections.sort(allSchemas, String.CASE_INSENSITIVE_ORDER);
                for (String sch : allSchemas) {
                    RestTeiidDataSourceJdbcCatalogSchemaInfo info = new RestTeiidDataSourceJdbcCatalogSchemaInfo();
                    info.setItemName(sch);
                    info.setItemType(SCHEMA);
                    infos.add(info);
                }
            }
            else {
                // Does not support either schemas or catalogues
                throw new Exception(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_UNRECOGNISED_JDBC_SOURCE));
            }
        } catch (Exception e) {
            throw new KException(e);
        }

        return infos;
    }

    /**
     * Return JDBC capabilities and info for a JDBC Connection
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the JDBC table names for the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Connection 
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER 
           + StringConstants.FORWARD_SLASH + V1Constants.JDBC_INFO_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get info for a jdbc source", response = RestConnectionJdbcInfo.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionJdbcInfo( final @Context HttpHeaders headers,
                                           final @Context UriInfo uriInfo,
                                           @ApiParam(value = "Id of the connection", required = true)
                                           final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        java.sql.Connection connection = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            uow = createTransaction(principal, "getConnectionJdbcTables", true); //$NON-NLS-1$

            // Get the data source from teiid
            Connection dataSource = cachedTeiid.getConnection(uow, connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            // Ensure the datasource is jdbc
            if(!dataSource.isJdbc(uow)) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_NOT_JDBC_ERROR);
            }

            // Get a connection to the jdbc source
            try {
                connection = getJdbcConnection(dataSource.getJndiName(uow));
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_CONNECTION_ERROR);
            }

            // Get the table names
            RestConnectionJdbcInfo info = new RestConnectionJdbcInfo();
            try {
                populateJdbcInfo(connection, info);
            } catch (Exception ex) {
                return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_JDBC_INFO_FAILURE);
            }

            return commit(uow, mediaTypes, info);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_JDBC_INFO_FAILURE);
        } finally {
            try {
                if(connection!=null) {
                    connection.close();
                }
            } catch (SQLException ex) {
            }
        }
   }
    
    /*
     * Populate the JDBC info using the supplied connection
     * @param connection the JDBC connection
     */
    private void populateJdbcInfo(java.sql.Connection connection, RestConnectionJdbcInfo jdbcInfo) throws KException {
        ArgCheck.isNotNull(connection, "connection");
        ArgCheck.isNotNull(jdbcInfo, "jdbcInfo");

        try {
            DatabaseMetaData metaData = connection.getMetaData();
            String productName = metaData.getDatabaseProductName();
            String productVersion = metaData.getDatabaseProductVersion();
            String driverName = metaData.getDriverName();
            int majorVersion = metaData.getDriverMajorVersion();
            int minorVersion = metaData.getDriverMinorVersion();
            String url = metaData.getURL();
            boolean readonly = metaData.isReadOnly();
            String userName = metaData.getUserName();
            boolean supportsCatalogs = metaData.supportsCatalogsInTableDefinitions()
                                       || metaData.supportsCatalogsInProcedureCalls()
                                       || metaData.supportsCatalogsInDataManipulation();
            boolean supportsSchemas = metaData.supportsSchemasInTableDefinitions()
                                      || metaData.supportsSchemasInDataManipulation();

            jdbcInfo.setProductName(productName);
            jdbcInfo.setProductVersion(productVersion);
            jdbcInfo.setDriverUrl(url);
            jdbcInfo.setReadonly(readonly);
            jdbcInfo.setDriverName(driverName);
            jdbcInfo.setDriverMajorVersion(majorVersion);
            jdbcInfo.setDriverMinorVersion(minorVersion);
            jdbcInfo.setUsername(userName);
            jdbcInfo.setSupportsCatalogs(supportsCatalogs);
            jdbcInfo.setSupportsSchemas(supportsSchemas);
        } catch (Exception e) {
            throw new KException(e);
        }
    }
    
    /*
     * Get List of Tables using the supplied connection
     * @param connection the JDBC connection
     * @return the list of table names
     */
    private List<String> getTableNames(java.sql.Connection connection, String catalogName, String schemaName, String tableFilter) throws KException {
        ArgCheck.isNotNull(connection, "connection");

        // Get the list of Tables
        List<String> tableNameList = new ArrayList<String>();
        try {
            ResultSet resultSet = connection.getMetaData().getTables(catalogName,
                                                                     schemaName,
                                                                     tableFilter,
                                                                     new String[] {"DOCUMENT", "TABLE", "VIEW"}); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            int columnCount = resultSet.getMetaData().getColumnCount();
            while (resultSet.next()) {
                String tableName = null;
                for (int i = 1; i <= columnCount; ++i) {
                    String colName = resultSet.getMetaData().getColumnName(i);
                    String value = resultSet.getString(i);
                    if (colName.equalsIgnoreCase(TABLE_NAME)) {
                        tableName = value;
                    }
                    if (tableName != null) {
                        break;
                    }
                }
                tableNameList.add(tableName);
            }
            resultSet.close();
        } catch (Exception e) {
            throw new KException(e);
        }

        return tableNameList;
    }

    private boolean isDataSource(String className) {
        if (className == null)
            return false;

        if (! className.endsWith("DataSource"))
            return false;

        return true;
    }

    /*
     * Get JDBC Connection for the specified jndiName
     */
    private java.sql.Connection getJdbcConnection (String jndiName) throws KException {
        String jdbcContext = jndiName.substring(0, jndiName.lastIndexOf('/')+1);

        // New Context
        if(initialContext==null) {
            try {
                initialContext = new InitialContext();
            } catch (Exception e) {
                throw new KException(e);
            }
        }

        // Get JDBC DataSource
        DataSource jdbcDataSource = null;

        NamingEnumeration<javax.naming.NameClassPair> ne = null;
        try {
            javax.naming.Context theJdbcContext = (javax.naming.Context) initialContext.lookup(jdbcContext);
            ne = theJdbcContext.list("");  //$NON-NLS-1$
            // Throws exception if provided context not found.
        } catch (NamingException e1) {
            throw new KException(e1);
        }

        while (ne!=null && ne.hasMoreElements()) {
            javax.naming.NameClassPair o = ne.nextElement();
            String className = o.getClassName();
            if (! isDataSource(className))
                continue;

            String jdbcObjectName = jdbcContext + o.getName();
            if(! jdbcObjectName.equals(jndiName))
                continue;

            try {
                Object jdbcObject = initialContext.lookup(jdbcContext + o.getName());
                if(jdbcObject != null && jdbcObject instanceof DataSource) {
                    jdbcDataSource = (DataSource)jdbcObject;
                    break;
                }
            } catch (NamingException e1) {
                throw new KException(e1);
            }
        }

        if(jdbcDataSource == null) {
            throw new KException(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_GET_DATA_SOURCE_INSTANTIATION_FAILURE));
        }

        try {
            return jdbcDataSource.getConnection();
        } catch (SQLException ex) {
            throw new KException(ex);
        }
    }
    
    /*
     * Loads driver name - translator mappings from resource file
     */
    private void loadDriverTranslatorMap() {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream( DRIVER_TRANSLATOR_MAPPING_FILE );
        
        if(inputStream==null) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_DEFAULT_TRANSLATOR_MAPPINGS_NOT_FOUND_ERROR));
            return;
        }
        
        driverTranslatorMap.clear();
        
        // Load the mappings file
        Document doc;
        try {
            String mappingXml = FileUtils.streamToString(inputStream);
            doc = FileUtils.createDocument(mappingXml);
        } catch (Exception ex) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_LOAD_DEFAULT_TRANSLATOR_MAPPINGS_ERROR, ex.getLocalizedMessage()));
            return;
        }
        
        // Single child node contains the mappings
        final Node mappingsNode = doc.getChildNodes().item(0);
        if ( mappingsNode.getNodeType() != Node.ELEMENT_NODE ) {
            return;
        }

        // Iterate the doc nodes and populate the default translator map
        final NodeList translatorNodes = ((Element)mappingsNode).getElementsByTagName( ELEM_TRANSLATOR );
        for(int i=0; i<translatorNodes.getLength(); i++) {
            final Node translatorNode = translatorNodes.item(i);
            String driver = translatorNode.getAttributes().getNamedItem( ATTR_DRIVER ).getTextContent();
            String translator = translatorNode.getTextContent();
            driverTranslatorMap.put(driver, translator);
        }
    }

    /*
     * Loads URL content - translator mappings from resource file
     */
    private void loadUrlContentTranslatorMap() {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream( URLCONTENT_TRANSLATOR_MAPPING_FILE );
        
        if(inputStream==null) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_DEFAULT_TRANSLATOR_MAPPINGS_NOT_FOUND_ERROR));
            return;
        }
        
        urlContentTranslatorMap.clear();
        
        // Load the mappings file
        Document doc;
        try {
            String mappingXml = FileUtils.streamToString(inputStream);
            doc = FileUtils.createDocument(mappingXml);
        } catch (Exception ex) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.TEIID_SERVICE_LOAD_DEFAULT_TRANSLATOR_MAPPINGS_ERROR, ex.getLocalizedMessage()));
            return;
        }
        
        // Single child node contains the mappings
        final Node mappingsNode = doc.getChildNodes().item(0);
        if ( mappingsNode.getNodeType() != Node.ELEMENT_NODE ) {
            return;
        }

        // Iterate the doc nodes and populate the default translator map
        final NodeList translatorNodes = ((Element)mappingsNode).getElementsByTagName( ELEM_TRANSLATOR );
        for(int i=0; i<translatorNodes.getLength(); i++) {
            final Node translatorNode = translatorNodes.item(i);
            String urlContent = translatorNode.getAttributes().getNamedItem( ATTR_URLCONTENT ).getTextContent();
            String translator = translatorNode.getTextContent();
            urlContentTranslatorMap.put(urlContent, translator);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the connection templates available in teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Templates JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of templates",
                            response = RestTemplate[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplates(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find templates
            uow = createTransaction(principal, "getTemplates", true); //$NON-NLS-1$

            Template[] templates = cachedTeiid.getTemplates(uow);
            LOGGER.debug("getTemplates:found '{0}' Templates", templates.length); //$NON-NLS-1$

            final List<RestTemplate> entities = new ArrayList<RestTemplate>();

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
            for (final Template template : templates) {
                RestTemplate entity = entityFactory.create(template, uriInfo.getBaseUri(), uow, properties);
                entities.add(entity);
                LOGGER.debug("getTemplates:Template '{0}' entity was constructed", template.getName(uow)); //$NON-NLS-1$
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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TEMPLATES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the connection templates available in teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Templates JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.TEMPLATE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find connection template by name",
                            response = RestTemplate.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No template could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplate(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo,
                                                   @ApiParam(value = "Name of the template", required = true)
                                                    final @PathParam( "templateName" ) String templateName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find template
            uow = createTransaction(principal, "getTemplates", true); //$NON-NLS-1$

            Template template = cachedTeiid.getTemplate(uow, templateName);
            if (template == null)
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            KomodoProperties properties = new KomodoProperties();
            final RestTemplate restTemplate = entityFactory.create(template, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getConnectionTemplate:Template '{0}' entity was constructed", template.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restTemplate );

        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TEMPLATE_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the entry properties available in a teiid template (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the TemplateEntry JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT + StringConstants.FORWARD_SLASH +
                  V1Constants.TEMPLATE_PLACEHOLDER + StringConstants.FORWARD_SLASH + 
                  V1Constants.TEMPLATE_ENTRIES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the template entries of the named template",
                            response = RestTemplateEntry[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No template could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplateEntries(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo,
                                                   @ApiParam(value = "Name of the template", required = true)
                                                    final @PathParam( "templateName" ) String templateName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            Teiid teiidNode = getDefaultTeiid();
            CachedTeiid cachedTeiid = importContent(teiidNode);

            // find template
            uow = createTransaction(principal, "getTemplateEntries", true); //$NON-NLS-1$

            Template template = cachedTeiid.getTemplate(uow, templateName);
            if (template == null)
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            final List<RestTemplateEntry> entities = new ArrayList<RestTemplateEntry>();
            List<TemplateEntry> entries = template.getEntries(uow);
            Collections.sort(entries, new TemplateEntryComparator(uow));
            List<String> priorityNames = Arrays.asList(PRIORITY_TEMPLATE_NAMES);

            for (TemplateEntry entry : entries) {

                KomodoProperties properties = new KomodoProperties();
                RestTemplateEntry restEntry = entityFactory.create(entry, uriInfo.getBaseUri(), uow, properties);

                if (priorityNames.contains(entry.getName(uow))) {
                    //
                    // Appears some properties are being flagged as not required when they really should be,
                    // eg. derbyclient.jar -> connection-url
                    //
                    restEntry.setRequired(true);
                }

                entities.add(restEntry);
            }

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

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.TEIID_SERVICE_GET_TEMPLATE_ENTRIES_ERROR);
        }
    }
}