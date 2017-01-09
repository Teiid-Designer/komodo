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
package org.komodo.relational.internal;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A base implementation of a relational object that does not permit children.
 */
public abstract class RelationalChildRestrictedObject extends RelationalObjectImpl {

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path ) throws KException {
        super( uow, repository, path );
    }

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path,
                                               final int index ) throws KException {
        super( uow, repository, path, index );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public final KomodoType[] getChildTypes() {
        return KomodoType.NO_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if this method is called
     */
    @Override
    public final KomodoObject addChild( final UnitOfWork uow,
                                        final String name,
                                        final String primaryType ) {
        throw new UnsupportedOperationException( "Children cannot be added to objects of type " + getClass().getName() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public final KomodoObject[] getChildren( final UnitOfWork uow,
                                             final String... namePatterns ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String[])
     */
    @Override
    public final KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                                   final String type,
                                                   final String... namePatterns ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public final boolean hasChild( final UnitOfWork uow,
                                   final String name ) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.repository.ObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final boolean hasChildren( final UnitOfWork uow ) {
        return false;
    }
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#isChildRestricted()
     */
    @Override
    public final boolean isChildRestricted() {
        return true;
    }

}
