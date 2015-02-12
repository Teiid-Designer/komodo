/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.internal;

import org.komodo.relational.model.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A class that determines if a {@link KomodoObject} can be converted into a strong typed relational object.
 */
public interface TypeResolver {

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the operation should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return <code>true</code> if object can be resolved to this resolvers type
     */
    boolean resolvable( final UnitOfWork transaction,
                        final Repository repository,
                        final KomodoObject kobject );

    /**
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if the operation should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return the strong typed {@link RelationalObject} (never <code>null</code>)
     * @throws KException
     *         if the object cannot be resolved or if an error occurs
     * @see #resolvable(UnitOfWork, Repository, KomodoObject)
     */
    RelationalObject resolve( final UnitOfWork transaction,
                              final Repository repository,
                              final KomodoObject kobject ) throws KException;

}
