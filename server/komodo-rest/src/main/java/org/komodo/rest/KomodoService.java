/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;
import java.util.concurrent.TimeUnit;
import javax.json.JsonObject;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.komodo.core.KEngine;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A Komodo service implementation.
 */
public abstract class KomodoService {

    protected final Repository repo;

    /**
     * Constructs a Komodo service.
     *
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     */
    protected KomodoService( final KEngine engine ) {
        this.repo = engine.getDefaultRepository();
    }

    /**
     * @param transaction
     *        the transaction that needs to be committed (cannot be <code>null</code>)
     * @return the response (never <code>null</code>)
     */
    protected Response commit( final UnitOfWork transaction,
                               final JsonObject json ) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        final int timeout = 30;
        final TimeUnit unit = TimeUnit.SECONDS;

        if ( callback.await( timeout, unit ) ) {
            return Response.ok().entity( json.toString() ).type( MediaType.APPLICATION_JSON ).build();
        }

        return Response.status( Status.GATEWAY_TIMEOUT ).entity( Messages.getString( COMMIT_TIMEOUT,
                                                                                     transaction.getName(),
                                                                                     timeout,
                                                                                     unit ) ).build();
    }

    /**
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction( final String name,
                                            final boolean rollbackOnly ) throws KException {
        final SynchronousCallback callback = new SynchronousCallback();
        return this.repo.createTransaction( ( getClass().getSimpleName() + ':' + name + ':' + System.currentTimeMillis() ),
                                            rollbackOnly,
                                            callback );
    }

}
