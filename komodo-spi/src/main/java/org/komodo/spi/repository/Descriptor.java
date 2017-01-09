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
