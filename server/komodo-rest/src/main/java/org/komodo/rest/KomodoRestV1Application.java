/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_CLEAR_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_CLEAR_TIMEOUT;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_SHUTDOWN_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_SHUTDOWN_TIMEOUT;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_ERROR;
import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_TIMEOUT;
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
import org.komodo.core.KEngine;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.KomodoVdbService;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.utils.KLog;

/**
 * The JAX-RS {@link Application} that provides the Komodo REST API.
 */
@ApplicationPath( V1Constants.APP_PATH )
public final class KomodoRestV1Application extends Application implements RepositoryObserver {

    /**
     * Constants associated with version 1 of the Komodo REST application.
     */
    public static interface V1Constants extends StringConstants {

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
        String DATA_ROLE_PLACEHOLDER = "{dataRoleName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for loading of the sample vdb data
         */
        String SAMPLE_DATA = "samples"; //$NON-NLS-1$
    }

    private static final int TIMEOUT = 1;
    private static final TimeUnit UNIT = TimeUnit.MINUTES;

    private final KEngine kengine;
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
            // Ensure server logging level is reduced to something sane!
            KLog.getLogger().setLevel(Level.INFO);
        } catch (Exception ex) {
            throw new ServerErrorException(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), ex);
        }

        this.latch = new CountDownLatch( 1 );
        this.kengine = start();

        final Set< Object > objs = new HashSet< >();
        objs.add( new KomodoExceptionMapper() );
        objs.add( new KomodoVdbService( this.kengine ) );
        this.singletons = Collections.unmodifiableSet( objs );
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
                                            Status.REQUEST_TIMEOUT );
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
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_STARTUP_ERROR ), Status.INTERNAL_SERVER_ERROR );
        }

        if ( !started ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_STARTUP_TIMEOUT, TIMEOUT, UNIT ),
                                            Status.REQUEST_TIMEOUT );
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
     * @param vdbName name of vdb
     * @param vdbStream vdb input stream
     * @throws Exception if error occurs
     */
    public void importVdb(String vdbName, InputStream vdbStream) throws Exception {
        Repository repository = this.kengine.getDefaultRepository();

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction("Import vdb " + vdbName, false, callback); //$NON-NLS-1$

        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.NAME, vdbName);
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
