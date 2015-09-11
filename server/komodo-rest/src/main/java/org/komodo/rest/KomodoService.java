/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;
import static org.komodo.rest.Messages.Error.RESOURCE_NOT_FOUND;
import static org.komodo.rest.json.KomodoRestEntity.NO_CONTENT;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import org.komodo.core.KEngine;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.json.KomodoRestEntity;
import org.komodo.rest.json.KomodoRestEntity.ResourceNotFound;
import org.komodo.rest.json.serialize.KomodoJsonMarshaller;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A Komodo service implementation.
 */
public abstract class KomodoService {

    private static final Logger LOGGER = LoggerFactory.getLogger( KomodoService.class );

    private static final int TIMEOUT = 30;
    private static final TimeUnit UNIT = TimeUnit.SECONDS;

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

    protected Response commit( final UnitOfWork transaction,
                               final KomodoRestEntity entity ) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final int timeout = TIMEOUT;
        final TimeUnit unit = UNIT;

        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        if ( callback.await( timeout, unit ) ) {
            final Throwable error = callback.error();

            if ( error == null ) {
                LOGGER.debug( "commit: successfully committed '{0}', rollbackOnly = '{1}'", //$NON-NLS-1$
                              transaction.getName(),
                              transaction.isRollbackOnly() );
                ResponseBuilder builder = null;

                if ( entity == NO_CONTENT ) {
                    builder = Response.noContent();
                } else if ( entity instanceof ResourceNotFound ) {
                    final ResourceNotFound resourceNotFound = ( ResourceNotFound )entity;

                    builder = Response.status( Status.NOT_FOUND )
                                      .type( MediaType.TEXT_PLAIN )
                                      .entity( Messages.getString( RESOURCE_NOT_FOUND,
                                                                   resourceNotFound.getResourceName(),
                                                                   resourceNotFound.getOperationName() ) );
                } else {
                    builder = Response.ok( KomodoJsonMarshaller.marshall( entity ), MediaType.APPLICATION_JSON );
                }

                return builder.build();
            }

            // callback was called because of an error condition
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                           .type( MediaType.TEXT_PLAIN )
                           .entity( error.getLocalizedMessage() )
                           .build();
        }

        // callback timeout occurred
        return Response.status( Status.GATEWAY_TIMEOUT )
                       .type( MediaType.TEXT_PLAIN )
                       .entity( Messages.getString( COMMIT_TIMEOUT, transaction.getName(), timeout, unit ) )
                       .build();
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
        final UnitOfWork result = this.repo.createTransaction( ( getClass().getSimpleName() + ':' + name + ':'
                                                                 + System.currentTimeMillis() ),
                                                               rollbackOnly, callback );
        LOGGER.debug( "createTransaction:created '{0}', rollbackOnly = '{1}'", result.getName(), result.isRollbackOnly() ); //$NON-NLS-1$
        return result;
    }

}
