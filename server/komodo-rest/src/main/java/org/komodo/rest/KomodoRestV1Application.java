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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.annotation.PreDestroy;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Response.Status;
import org.komodo.core.KEngine;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;

/**
 * The JAX-RS {@link Application} that provides the Komodo REST API.
 */
@ApplicationPath( V1Constants.APP_PATH )
public final class KomodoRestV1Application extends Application implements RepositoryObserver {

    /**
     * Constants associated with version 1 of the Komodo REST application.
     */
    public interface V1Constants {

        /**
         * The URI path segment for the Komodo REST application. It is included in the base URI. <strong>DO NOT INCLUDE THIS IN
         * OTHER URI SEGMENTS</strong>
         */
        String APP_PATH = "/v1"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the Komodo workspace.
         */
        String WORKSPACE_SEGMENT = "workspace"; //$NON-NLS-1$

        /**
         * The URI path segment for the Komodo workspace. This will be prefixed by the base URI to get the URL.
         */
        String WORKSPACE_URI_PATH = '/' + WORKSPACE_SEGMENT;

        /**
         * The name of the URI path segment for the VDB manifest XML resource.
         */
        String VDB_MANIFEST_SEGMENT = "manifest"; //$NON-NLS-1$

        /**
         * The application-relative URI path segment for the collection of VDBs in the Komodo workspace.
         */
        String VDBS_RELATIVE_PATH = "workspace/vdbs"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of VDBs in the Komodo workspace.
         */
        String VDBS_SEGMENT = "vdbs"; //$NON-NLS-1$

        /**
         * The URI path segment for the collection of VDBs in the Komodo workspace. This will be prefixed by the base URI to get
         * the URL.
         */
        String VDBS_URI_PATH = WORKSPACE_URI_PATH + '/' + VDBS_SEGMENT;

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

}
