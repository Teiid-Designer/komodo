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
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_CLEAR_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_CLEAR_TIMEOUT;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_SHUTDOWN_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_SHUTDOWN_TIMEOUT;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_TIMEOUT;
import java.io.File;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import javax.annotation.PreDestroy;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.jboss.resteasy.plugins.interceptors.CorsFilter;
import org.komodo.core.KEngine;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.service.KomodoImportExportService;
import org.komodo.rest.service.KomodoSearchService;
import org.komodo.rest.service.KomodoTeiidService;
import org.komodo.rest.service.KomodoUtilService;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.rest.swagger.RestPropertyConverter;
import org.komodo.rest.swagger.RestVdbConditionConverter;
import org.komodo.rest.swagger.RestVdbConverter;
import org.komodo.rest.swagger.RestVdbDataRoleConverter;
import org.komodo.rest.swagger.RestVdbImportConverter;
import org.komodo.rest.swagger.RestVdbMaskConverter;
import org.komodo.rest.swagger.RestVdbModelConverter;
import org.komodo.rest.swagger.RestVdbModelSourceConverter;
import org.komodo.rest.swagger.RestVdbPermissionConverter;
import org.komodo.rest.swagger.RestVdbTranslatorConverter;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import io.swagger.converter.ModelConverters;
import io.swagger.jaxrs.config.BeanConfig;

/**
 * The JAX-RS {@link Application} that provides the Komodo REST API.
 */
@ApplicationPath( V1Constants.APP_PATH )
public class KomodoRestV1Application extends Application implements RepositoryObserver, StringConstants {

    /**
     * Constants associated with version 1 of the Komodo REST application.
     */
    public static interface V1Constants extends JsonConstants {

        /**
         * Application name and context
         */
        String APP_NAME = "vdb-builder"; //$NON-NLS-1$

        /**
         * Version of the application
         */
        String APP_VERSION = "0.0.3"; //$NON-NLS-1$

        /**
         * The komodo engine's data directory system property
         */
        String KOMODO_DATA_DIR = "komodo.dataDir";

        /**
         * Jboss server base directory
         */
        String JBOSS_SERVER_BASE_DIR = "jboss.server.base.dir";

        /**
         * Location for the log file passed to {@link KLog} logger
         */
        String LOG_FILE_PATH = "log/vdb-builder.log"; //$NON-NLS-1$

        /**
         * The URI path segment for the Komodo REST application. It is included in the base URI. <strong>DO NOT INCLUDE THIS IN
         * OTHER URI SEGMENTS</strong>
         */
        String APP_PATH = FORWARD_SLASH + "v1"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the Komodo workspace.
         */
        String WORKSPACE_SEGMENT = "workspace"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the utility service.
         */
        String SERVICE_SEGMENT = "service"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the teiid service.
         */
        String TEIID_SEGMENT = "teiid"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the Komodo schema.
         */
        String SCHEMA_SEGMENT = "schema"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the VDB manifest XML resource.
         */
        String VDB_MANIFEST_SEGMENT = "manifest"; //$NON-NLS-1$

        /**
         * The about segment
         */
        String ABOUT = "about"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of VDBs in the Komodo workspace.
         */
        String VDBS_SEGMENT = "vdbs"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific vdb id
         */
        String VDB_PLACEHOLDER = "{vdbName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of models of a vdb
         */
        String MODELS_SEGMENT = "Models"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific model id
         */
        String MODEL_PLACEHOLDER = "{modelName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of sources of a model
         */
        String SOURCES_SEGMENT = "VdbModelSources"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific source id
         */
        String SOURCE_PLACEHOLDER = "{sourceName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of translators of a vdb
         */
        String TRANSLATORS_SEGMENT = "VdbTranslators"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific translator id
         */
        String TRANSLATOR_PLACEHOLDER = "{translatorName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of imports of a vdb
         */
        String IMPORTS_SEGMENT = "VdbImports"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific import id
         */
        String IMPORT_PLACEHOLDER = "{importName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of data roles of a vdb
         */
        String DATA_ROLES_SEGMENT = "VdbDataRoles"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific data role id
         */
        String DATA_ROLE_PLACEHOLDER = "{dataRoleId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of data role permissions
         */
        String PERMISSIONS_SEGMENT = "VdbPermissions"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific permission id
         */
        String PERMISSION_PLACEHOLDER = "{permissionId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of permission's conditions
         */
        String CONDITIONS_SEGMENT = "VdbConditions"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific condition id
         */
        String CONDITION_PLACEHOLDER = "{conditionId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of permission's masks
         */
        String MASKS_SEGMENT = "VdbMasks"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific mask id
         */
        String MASK_PLACEHOLDER = "{maskId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for loading of the sample vdb data
         */
        String SAMPLE_DATA = "samples"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for searching the workspace
         */
        String SEARCH_SEGMENT = "search"; //$NON-NLS-1$

        /**
         * The name of the URI search saved search parameter
         */
        String SEARCH_SAVED_NAME_PARAMETER = "searchName"; //$NON-NLS-1$

        /**
         * The name of the URI search contains parameter
         */
        String SEARCH_CONTAINS_PARAMETER = "contains"; //$NON-NLS-1$

        /**
         * The name of the URI search name parameter
         */
        String SEARCH_OBJECT_NAME_PARAMETER = "objectName"; //$NON-NLS-1$

        /**
         * The name of the URI search path parameter
         */
        String SEARCH_PATH_PARAMETER = "path"; //$NON-NLS-1$

        /**
         * The name of the URI search path parameter
         */
        String SEARCH_TYPE_PARAMETER = "type"; //$NON-NLS-1$

        /**
         * The name of the URI search parent parameter
         */
        String SEARCH_PARENT_PARAMETER = "parent"; //$NON-NLS-1$

        /**
         * The name of the URI search ancestor parameter
         */
        String SEARCH_ANCESTOR_PARAMETER = "ancestor"; //$NON-NLS-1$

        /**
         * The URI path for the collection of saved searches
         */
        String SAVED_SEARCHES_SEGMENT = "savedSearches"; //$NON-NLS-1$

        /**
         * The name of the URI vdb name parameter
         */
        String VDB_NAME_PARAMETER = "name"; //$NON-NLS-1$

        /**
         * The vdb export xml property
         */
        String VDB_EXPORT_XML_PROPERTY = "vdb-export-xml"; //$NON-NLS-1$

        /**
         * The teiid credentials property for modifying the usernames and passwords
         */
        String TEIID_CREDENTIALS = "credentials";

        /**
         * The teiid status path segment
         */
        String STATUS_SEGMENT = "status";

        /**
         * The name of the URI path segment for the collection of data sources
         */
        String DATA_SOURCES_SEGMENT = "DataSources"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific data source id
         */
        String DATA_SOURCE_PLACEHOLDER = "{dataSourceName}"; //$NON-NLS-1$

        /**
         * The name of the resource used for importing and exporting artifacts
         */
        String IMPORT_EXPORT_SEGMENT = "importexport";

        /**
         * The export operation of the import export service
         */
        String EXPORT = "export";
    }

    private static final int TIMEOUT = 1;
    private static final TimeUnit UNIT = TimeUnit.MINUTES;

    private KEngine kengine;
    private CountDownLatch latch;
    private final Set< Object > singletons;

    /**
     * Constructs a Komodo REST application.
     *
     * @throws ServerErrorException
     *         if the Komodo engine cannot be started
     */
    public KomodoRestV1Application() throws ServerErrorException {
        try {
            // Set the log path to something relative to the deployment location of this application
            // Try to use the base directory in jboss. If not in jboss this would probably be empty so
            // otherwise use "." relative to the working directory
            String baseDir = System.getProperty(V1Constants.JBOSS_SERVER_BASE_DIR, DOT) + File.separator;

            // Set the komodo data directory prior to starting the engine
            String komodoDataDir = System.getProperty(V1Constants.KOMODO_DATA_DIR);
            if (komodoDataDir == null)
                System.setProperty(V1Constants.KOMODO_DATA_DIR, baseDir + "data");

            // Set the log file path
            KLog.getLogger().setLogPath(baseDir + V1Constants.LOG_FILE_PATH);

            // Ensure server logging level is reduced to something sane!
            KLog.getLogger().setLevel(Level.INFO);
        } catch (Exception ex) {
            throw new ServerErrorException(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), ex);
        }

        this.latch = new CountDownLatch( 1 );
        this.kengine = start();

        final Set< Object > objs = new HashSet< >();
        objs.add( new KomodoExceptionMapper() );
        objs.add( new KomodoUtilService( this.kengine ) );
        objs.add( new KomodoVdbService( this.kengine ) );
        objs.add( new KomodoSearchService( this.kengine ));
        objs.add( new KomodoTeiidService( this.kengine ));
        objs.add( new KomodoImportExportService( this.kengine ));

        CorsFilter corsFilter = initCorsFilter();
        objs.add(corsFilter);

        this.singletons = Collections.unmodifiableSet( objs );

        initSwaggerConfiguration();
    }

    private CorsFilter initCorsFilter() {
        CorsFilter corsFilter = new CorsFilter();
        corsFilter.getAllowedOrigins().add("*");
        String allowHeaders = "Content-Type, X-Requested-With, accept, Origin," +
                                                "Access-Control-Request-Method," +
                                                "Access-Control-Request-Headers, Authorization";
        corsFilter.setAllowedHeaders(allowHeaders);
        corsFilter.setAllowCredentials(true);
        corsFilter.setAllowedMethods("GET, POST, PUT, DELETE, OPTIONS, HEAD");
        corsFilter.setCorsMaxAge(1209600);
        return corsFilter;
    }

    @SuppressWarnings( "nls" )
    private void initSwaggerConfiguration() {
        //
        // Add converters for display of definitions
        //
        ModelConverters converters = ModelConverters.getInstance();
        converters.addConverter(new RestPropertyConverter());
        converters.addConverter(new RestVdbConditionConverter());
        converters.addConverter(new RestVdbConverter());
        converters.addConverter(new RestVdbDataRoleConverter());
        converters.addConverter(new RestVdbImportConverter());
        converters.addConverter(new RestVdbMaskConverter());
        converters.addConverter(new RestVdbModelConverter());
        converters.addConverter(new RestVdbModelSourceConverter());
        converters.addConverter(new RestVdbPermissionConverter());
        converters.addConverter(new RestVdbTranslatorConverter());

        BeanConfig beanConfig = new BeanConfig();
        beanConfig.setTitle("Vdb Builder");
        beanConfig.setDescription("A tool that allows creating, editing and managing dynamic VDBs and their contents");
        beanConfig.setVersion(V1Constants.APP_VERSION);
        beanConfig.setSchemes(new String[]{"http"});
        beanConfig.setBasePath(V1Constants.APP_NAME + V1Constants.APP_PATH);

        // No need to setHost as it will pick up the one its running on

        beanConfig.setResourcePackage(
                                      RestProperty.class.getPackage().getName() + COMMA +
                                      KomodoVdbService.class.getPackage().getName());
        beanConfig.setPrettyPrint(true);
        beanConfig.setScan(true);
    }

    /**
     * @return the default repository of this application.
     *            Should only be applicable for testing.
     *
     */
    public Repository getDefaultRepository() {
        return kengine.getDefaultRepository();
    }

    /**
     * Clears the Komodo default repository.
     *
     * @throws ServerErrorException
     *         if an error occurs clearing the repository
     */
    public void clearRepository() throws ServerErrorException {
        this.latch = new CountDownLatch( 1 );

        final RepositoryClientEvent event = RepositoryClientEvent.createClearEvent( this.kengine );
        this.kengine.getDefaultRepository().notify( event );

        // wait for repository to clear
        boolean cleared = false;

        try {
            cleared = this.latch.await( TIMEOUT, UNIT );
        } catch ( final Exception e ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_CLEAR_ERROR ), Status.INTERNAL_SERVER_ERROR );
        }

        if ( !cleared ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_CLEAR_TIMEOUT, TIMEOUT, UNIT ),
                                            Status.INTERNAL_SERVER_ERROR );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.RepositoryObserver#eventOccurred()
     */
    @Override
    public void eventOccurred() {
        this.latch.countDown();
    }

    /**
     * {@inheritDoc}
     *
     * @see javax.ws.rs.core.Application#getSingletons()
     */
    @Override
    public Set< Object > getSingletons() {
        return this.singletons;
    }

    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> resources = new HashSet<Class<?>>();

        // Enable swagger support
        resources.add(io.swagger.jaxrs.listing.ApiListingResource.class);
        resources.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);

        return resources;
    }

    private KEngine start() throws ServerErrorException {
        final KEngine kengine = KEngine.getInstance();
        final Repository repo = kengine.getDefaultRepository();
        repo.addObserver( this );

        // wait for repository to start
        boolean started = false;

        try {
            kengine.start();
            started = this.latch.await( TIMEOUT, UNIT );
        } catch ( final Exception e ) {
            String stackTrace = StringUtils.exceptionToString(e);
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_STARTUP_ERROR, e ) + NEW_LINE + stackTrace, Status.INTERNAL_SERVER_ERROR );
        }

        if ( !started ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_STARTUP_TIMEOUT, TIMEOUT, UNIT ),
                                            Status.INTERNAL_SERVER_ERROR );
        }

        return kengine;
    }

    /**
     * Stops the Komodo Engine.
     *
     * @throws ServerErrorException
     *         if there is a problem shutting down the Komodo engine
     */
    @PreDestroy
    public void stop() throws ServerErrorException {
        if ( this.kengine != null ) {
            this.latch = new CountDownLatch( 1 );

            // wait for repository to shutdown
            boolean shutdown = false;

            try {
                this.kengine.shutdown();
                shutdown = this.latch.await( TIMEOUT, UNIT );
            } catch ( final Exception e ) {
                throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_SHUTDOWN_ERROR ),
                                                Status.INTERNAL_SERVER_ERROR );
            } finally {
                this.kengine = null;
            }

            if ( !shutdown ) {
                throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_SHUTDOWN_TIMEOUT, TIMEOUT, UNIT ),
                                                Status.REQUEST_TIMEOUT );
            }
        }
    }

    /**
     * Import a vdb into the komodo engine
     *
     * @param vdbStream vdb input stream
     * @throws Exception if error occurs
     */
    public void importVdb(InputStream vdbStream) throws Exception {
        Repository repository = this.kengine.getDefaultRepository();

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction("Import Vdb", false, callback); //$NON-NLS-1$

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = repository.komodoWorkspace(uow);
        VdbImporter importer = new VdbImporter(repository);
        importer.importVdb(uow, vdbStream, workspace, importOptions, importMessages);
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    /**
     * @return the vdbs directly from the kEngine
     * @throws Exception if error occurs
     */
    public Vdb[] getVdbs() throws Exception {
        Repository repository = this.kengine.getDefaultRepository();
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository);

        UnitOfWork uow = repository.createTransaction("Find vdbs", true, null); //$NON-NLS-1$
        Vdb[] vdbs = mgr.findVdbs(uow);
        uow.commit();

        return vdbs;
    }
}
