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
package org.komodo.relational.dataservice.internal;

import java.io.InputStream;
import org.komodo.relational.dataservice.DataServiceResource;
import org.komodo.relational.dataservice.DataServiceResourceEntry;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

public abstract class AbstractDataServiceResourceEntryImpl<T extends DataServiceResource>
    extends AbstractDataServiceEntryImpl<T> implements DataServiceResourceEntry<T> {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    protected AbstractDataServiceResourceEntryImpl( final UnitOfWork transaction,
                                       final Repository repository,
                                       final String path ) throws KException {
        super( transaction, repository, path );
    }

    @Override
    public InputStream getContent( final UnitOfWork transaction ) throws KException {
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
    @Override
    public void setResource( final UnitOfWork uow,
                              final T resource ) throws KException {
        setReference( uow, resource );
    }
}
