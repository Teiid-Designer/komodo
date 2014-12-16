/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a DDL statement option from a relational model.
 */
public interface StatementOption extends RelationalObject {

    /**
     * An empty collection of index constraints.
     */
    StatementOption[] NO_OPTIONS = new StatementOption[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the statement option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOption( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newOption
     *        the new value for the <code>statement option</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOption( final UnitOfWork transaction,
                    final String newOption ) throws KException;

}
