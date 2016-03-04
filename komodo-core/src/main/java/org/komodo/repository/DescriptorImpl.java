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
package org.komodo.repository;

import javax.jcr.Session;
import javax.jcr.nodetype.NodeDefinition;
import javax.jcr.nodetype.NodeTypeManager;
import javax.jcr.nodetype.PropertyDefinition;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a {@link KomodoObject Komodo object} {@link Descriptor type definition}.
 */
public class DescriptorImpl implements Descriptor {

    final String name;
    final Repository repository;

    /**
     * @param descriptorRepository
     *        the repository where the descriptor is located (cannot be <code>null</code>)
     * @param descriptorName
     *        the descriptor name (cannot be empty)
     */
    public DescriptorImpl( final Repository descriptorRepository,
                           final String descriptorName ) {
        ArgCheck.isNotNull( descriptorRepository, "descriptorRepository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$

        this.repository = descriptorRepository;
        this.name = descriptorName;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj ) {
        if ( ( obj == null ) || !Descriptor.class.isInstance( obj ) ) {
            return false;
        }

        final Descriptor that = ( Descriptor )obj;
        return this.name.equals( that.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Descriptor#getChildDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor[] getChildDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final NodeTypeManager nodeTypeMgr = getSession( transaction ).getWorkspace().getNodeTypeManager();
            final NodeDefinition[] childDefns = nodeTypeMgr.getNodeType( this.name ).getChildNodeDefinitions();
            final Descriptor[] childDescriptors = new Descriptor[ childDefns.length ];
            int i = 0;

            for ( final NodeDefinition childDefn : childDefns ) {
                childDescriptors[i++] = new DescriptorImpl( this.repository, childDefn.getName() );
            }

            return childDescriptors;
        } catch ( final Exception e ) {
            if ( e instanceof KException ) {
                throw ( KException )e;
            }

            throw new KException( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Descriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Descriptor#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final NodeTypeManager nodeTypeMgr = getSession( transaction ).getWorkspace().getNodeTypeManager();
            final PropertyDefinition[] propDefns = nodeTypeMgr.getNodeType( this.name ).getPropertyDefinitions();
            final PropertyDescriptor[] propDescriptors = new PropertyDescriptorImpl[ propDefns.length ];
            int i = 0;

            for ( final PropertyDefinition propDefn : propDefns ) {
                propDescriptors[i++] = new PropertyDescriptorImpl( propDefn );
            }

            return propDescriptors;
        } catch ( final Exception e ) {
            if ( e instanceof KException ) {
                throw ( KException )e;
            }

            throw new KException( e );
        }
    }

    private Session getSession( final UnitOfWork transaction ) {
        return ( ( UnitOfWorkImpl )transaction ).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return this.name.hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.name;
    }

}
