/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a data service entry for a Service VDB.
 */
public interface ServiceVdbEntry extends VdbEntry, VdbEntryContainer {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param dependency
     *        the VDB dependency being added (cannot be <code>null</code>)
     * @return the VDB dependency entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addDependency( final UnitOfWork transaction,
                            final Vdb dependency ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param dependencyEntryName
     *        the name of the VDB dependency entry to create (cannot be empty)
     * @return the VDB dependency entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addDependencyEntry( final UnitOfWork transaction,
                                 final String dependencyEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the entries corresponding to VDB dependency entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry[] getDependencies( final UnitOfWork transaction ) throws KException;
}
