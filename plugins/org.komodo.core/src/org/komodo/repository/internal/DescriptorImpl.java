/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.repository.internal;

import javax.jcr.Session;
import javax.jcr.nodetype.NodeDefinition;
import javax.jcr.nodetype.NodeTypeManager;
import javax.jcr.nodetype.PropertyDefinition;
import org.komodo.repository.internal.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.IRepository;
import org.komodo.spi.repository.IRepository.UnitOfWork;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a {@link KomodoObject Komodo object} {@link Descriptor type definition}.
 */
class DescriptorImpl implements Descriptor {

    final String name;
    final IRepository repository;

    DescriptorImpl( final IRepository descriptorRepository,
                    final String descriptorPath ) {
        ArgCheck.isNotNull(descriptorRepository, "descriptorRepository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(descriptorPath, "descriptorPath"); //$NON-NLS-1$

        this.repository = descriptorRepository;
        this.name = descriptorPath;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Descriptor#getChildDescriptors()
     */
    @Override
    public Descriptor[] getChildDescriptors() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("descriptor-getChildDescriptors", true, null); //$NON-NLS-1$

        try {
            final NodeTypeManager nodeTypeMgr = getSession(transaction).getWorkspace().getNodeTypeManager();
            final NodeDefinition[] childDefns = nodeTypeMgr.getNodeType(this.name).getChildNodeDefinitions();
            final Descriptor[] childDescriptors = new Descriptor[childDefns.length];
            int i = 0;

            for (final NodeDefinition childDefn : childDefns) {
                childDescriptors[i++] = new DescriptorImpl(this.repository, childDefn.getName());
            }

            transaction.commit();
            return childDescriptors;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
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
     * @see org.komodo.spi.repository.Descriptor#getPropertyDescriptors()
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors() throws KException {
        final UnitOfWork transaction = this.repository.createTransaction("descriptor-getPropertyDescriptors", true, null); //$NON-NLS-1$

        try {
            final NodeTypeManager nodeTypeMgr = getSession(transaction).getWorkspace().getNodeTypeManager();
            final PropertyDefinition[] propDefns = nodeTypeMgr.getNodeType(this.name).getPropertyDefinitions();
            final PropertyDescriptor[] propDescriptors = new PropertyDescriptorImpl[propDefns.length];
            int i = 0;

            for (final PropertyDefinition propDefn : propDefns) {
                propDescriptors[i++] = new PropertyDescriptorImpl(propDefn);
            }

            transaction.commit();
            return propDescriptors;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    private Session getSession(final UnitOfWork transaction) {
        return ((UnitOfWorkImpl)transaction).getSession();
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
