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
package org.komodo.relational.resource.internal;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.internal.DataServiceResourceImpl;
import org.komodo.relational.resource.UdfFile;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Implementation of UDF file instance model.
 */
public class UdfFileImpl extends DataServiceResourceImpl< UdfFile > implements UdfFile {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public UdfFileImpl( final UnitOfWork uow,
                        final Repository repository,
                        final String path ) throws KException {
        super( uow, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return UdfFile.IDENTIFIER;
    }

}
