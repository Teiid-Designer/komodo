/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import java.io.InputStream;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service resource (i.e., a driver, UDF, or DDL file).
 *
 * @param <T>
 *        the type of data service resource
 */
public interface DataServiceResourceEntry< T extends DataServiceResource > extends DataServiceEntry< T > {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the resource (can be <code>null</code> if not yet assigned)
     * @throws KException
     *         if an error occurs
     */
    T getResource( final UnitOfWork uow ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the binary contents of this resource as an {@link InputStream} (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    default InputStream getContent( final UnitOfWork transaction ) throws KException {
        final T resource = getResource( transaction );

        if ( resource == null ) {
            return null;
        }

        return resource.getContent( transaction );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param resource
     *        the associated resource (can be <code>null</code> when removing current resource)
     * @throws KException
     *         if an error occurs
     */
    default void setResource( final UnitOfWork uow,
                              final T resource ) throws KException {
        setReference( uow, resource );
    }

}
