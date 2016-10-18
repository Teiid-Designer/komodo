/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
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
