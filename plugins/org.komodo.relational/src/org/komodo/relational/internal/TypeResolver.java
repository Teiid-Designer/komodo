/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.internal;

import org.komodo.relational.RelationalProperties;
import org.komodo.relational.model.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A class that determines if a {@link KomodoObject} can be converted into a strong typed relational object.
 *
 * @param <T>
 *        the {@link RelationalObject} subclass
 */
public interface TypeResolver< T extends RelationalObject > {

    /**
     * @return the identifier associated with this resolver
     */
    KomodoType identifier();

    /**
     * @return the class implementing this type resolver
     */
    Class<? extends KomodoObject>owningClass();

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
    T resolve( final UnitOfWork transaction,
               final Repository repository,
               final KomodoObject kobject ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the operation should be automatically committed)
     * @param parent
     *        the parent of the new object (cannot be <code>null</code>)
     * @param id
     *        the identifier of the object (cannot be <code>null</code>)
     * @param type
     *        the type of the object (cannot be <code>null</code>)
     * @param properties
     *        any additional properties required for construction
     * @return new instance of the resolved object
     * @throws KException if error occurs
     */
    KomodoObject create(UnitOfWork transaction,
                                      KomodoObject parent,
                                      String id,
                                      RelationalProperties properties) throws KException;
}
