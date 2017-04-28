/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import org.komodo.relational.connection.Connection;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service entry for a connection.
 */
public interface ConnectionEntry extends DataServiceEntry< Connection > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.CONNECTION_ENTRY;

    /**
     * An empty collection of entries.
     */
    ConnectionEntry[] NO_ENTRIES = new ConnectionEntry[ 0 ];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the JNDI name (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getJndiName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param jndiName
     *        the value to use to set the JNDI name (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setJndiName( final UnitOfWork transaction,
                      final String jndiName ) throws KException;

}
