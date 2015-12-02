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
package org.komodo.relational.datasource.internal;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;

/**
 * Implementation of datasource instance model
 */
public class DatasourceImpl extends RelationalChildRestrictedObject implements Datasource, EventManager {

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
    public DatasourceImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Datasource.RESOLVER.identifier();
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid id property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getId( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property prop = getRawProperty( transaction, JcrLexicon.UUID.getString() );
        final String result = prop.getStringValue( transaction );
        return result;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getJndiName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJndiName( final UnitOfWork uow ) throws KException {
        final String jndiName = getObjectProperty( uow, PropertyValueType.STRING, "getJndiName", //$NON-NLS-1$
                                                   KomodoLexicon.DataSource.JNDI_NAME);

        return jndiName == null ? EMPTY_STRING : jndiName;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#getDriverName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDriverName( final UnitOfWork uow ) throws KException {
        final String driverName = getObjectProperty( uow, PropertyValueType.STRING, "getDriverName", //$NON-NLS-1$
                                                     KomodoLexicon.DataSource.DRIVER_NAME);

        return driverName == null ? EMPTY_STRING : driverName;
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setDriverName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDriverName( final UnitOfWork uow,
                               final String driverName ) throws KException {
        setObjectProperty( uow, "setDriverName", KomodoLexicon.DataSource.DRIVER_NAME, driverName ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.datasource.Datasource#setJndiName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJndiName( final UnitOfWork uow,
                             final String jndiName ) throws KException {
        setObjectProperty( uow, "setJndiName", KomodoLexicon.DataSource.JNDI_NAME, jndiName ); //$NON-NLS-1$
    }
    
    @Override
    public boolean addListener( ExecutionConfigurationListener listener ) {
        return false;
    }

    @Override
    public void permitListeners( boolean enable ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners( ExecutionConfigurationEvent event ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener( ExecutionConfigurationListener listener ) {
        return false;
    }

}
