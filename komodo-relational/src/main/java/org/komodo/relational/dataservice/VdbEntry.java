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
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service entry for a VDB.
 */
public interface VdbEntry extends DataServiceEntry< Vdb > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.VDB_DATA_SERVICE_ENTRY;

    /**
     * An empty collection of VDB entries.
     */
    VdbEntry[] NO_ENTRIES = new VdbEntry[ 0 ];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the VDB name (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getVdbName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the VDB version (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getVdbVersion( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param vdbName
     *        the value to use to set the VDB name of the entry (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setVdbName( final UnitOfWork transaction,
                     final String vdbName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param vdbVersion
     *        the value to use to set the VDB version of the entry (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setVdbVersion( final UnitOfWork transaction,
                        final String vdbVersion ) throws KException;

}
