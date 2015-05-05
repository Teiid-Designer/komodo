/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * A {@link KomodoObject Komodo object} type definition.
 */
public interface Descriptor {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the child {@link KomodoObject Komodo object} {@link Descriptor type descriptors} (never <code>null</code> but can
     *         be empty)
     * @throws KException
     *         if an error occurs
     */
    Descriptor[] getChildDescriptors( final UnitOfWork transaction ) throws KException;

    /**
     * @return the type name (never empty)
     */
    String getName();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the {@link KomodoObject Komodo object's} {@link Property property} {@link PropertyDescriptor descriptors} (never
     *         <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException;

}
