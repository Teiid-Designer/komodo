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

import java.util.Properties;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Indicates the object has a string representation that can be exported. In many cases this is an XML represention.
 */
public interface Exportable {

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the name of this exportable
     * @throws KException
     *         if an error occurs
     */
    String getName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the type of this exportable
     * @throws KException
     *         if an error occurs
     */
    DocumentType getDocumentType(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param properties (can be <code>null</code> or empty)
     * @return a byte array of the current object state (never empty)
     * @throws KException
     *         if an error occurs
     */
    byte[] export( final UnitOfWork transaction, Properties properties ) throws KException;

}
