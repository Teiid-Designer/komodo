/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * A class that determines if a {@link KomodoObject} can be converted into a strong typed relational object.
 *
 * @param <T>
 *        the {@link RelationalObject} subclass
 */
public interface TypeResolver< T extends RelationalObject > {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parent
     *        the parent of the new object (can be <code>null</code>)
     * @param id
     *        the identifier/name of the object (cannot be <code>null</code>)
     * @param type
     *        the type of the object (cannot be <code>null</code>)
     * @param properties
     *        any additional properties required for construction (can be empty)
     * @return new instance of the resolved object (never <code>null</code>)
     * @throws KException
     *         if error occurs
     */
    KomodoObject create( final UnitOfWork transaction,
                         final Repository repository,
                         final KomodoObject parent,
                         final String id,
                         final RelationalProperties properties ) throws KException;

    /**
     * @return the identifier associated with this resolver (never <code>null</code>)
     */
    KomodoType identifier();

    /**
     * @return the class this type resolver pertains to (never <code>null</code>)
     */
    Class< ? extends KomodoObject > owningClass();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return <code>true</code> if object can be resolved to this resolver's type
     * @throws KException
     *         if an error occurs
     */
    boolean resolvable( final UnitOfWork transaction,
                        final KomodoObject kobject ) throws KException;

    /**
     * Converts the specified {@link KomodoObject} to this resolver's strong typed relational object. It is assumed that the
     * object has been {@link #resolvable(UnitOfWork, KomodoObject) resolved}.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if the operation should be automatically committed)
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return the strong typed {@link RelationalObject} (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see #resolvable(UnitOfWork, KomodoObject)
     */
    T resolve( final UnitOfWork transaction,
               final KomodoObject kobject ) throws KException;

}
