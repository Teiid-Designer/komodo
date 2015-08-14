/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;

/**
 * Maps {@link Throwable errors} to {@link Response responses}.
 */
public final class KomodoExceptionMapper implements ExceptionMapper< Throwable > {

    /**
     * {@inheritDoc}
     *
     * @see javax.ws.rs.ext.ExceptionMapper#toResponse(java.lang.Throwable)
     */
    @Override
    public Response toResponse( final Throwable t ) {
        return Response.status( Status.INTERNAL_SERVER_ERROR ).entity( t.getLocalizedMessage() ).build();
    }

}
