/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_TIMEOUT;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Response;
import org.komodo.core.KEngine;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryObserver;

/**
 * The JAX-RS {@link Application} that provides the Komodo REST API.
 */
@ApplicationPath( "/komodo/v1" )
public final class KomodoRestV1Application extends Application {

    private final Set< Object > singletons;

    /**
     * Constructs a Komodo REST application.
     *
     * @throws Exception
     *         if the {@link KomodoVdbService} cannot be started
     */
    public KomodoRestV1Application() throws Exception {
        // start engine
        final KEngine kengine = KEngine.getInstance();
        final Repository repo = kengine.getDefaultRepository();

        // wait for repository to start
        final CountDownLatch latch = new CountDownLatch( 1 );

        // Observer attached to the default repository for listening for the change of state
        final RepositoryObserver observer = new RepositoryObserver() {

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.spi.repository.RepositoryObserver#eventOccurred()
             */
            @Override
            public void eventOccurred() {
                latch.countDown();
            }
        };

        repo.addObserver( observer );

        // since latch is all setup start engine and block thread until latch has counted down or timeout has been reached
        kengine.start();

        final int timeout = 1;
        final TimeUnit unit = TimeUnit.MINUTES;
        final boolean started = latch.await( timeout, unit );

        if ( !started ) {
            throw new ServerErrorException( Messages.getString( KOMODO_ENGINE_STARTUP_TIMEOUT, timeout, unit ),
                                            Response.Status.REQUEST_TIMEOUT );
        }

        { // construct singletons
            final Set< Object > objs = new HashSet< >();
            objs.add( new KomodoExceptionMapper() );
            objs.add( new KomodoVdbService( kengine ) );
            this.singletons = Collections.unmodifiableSet( objs );
        }
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

}
