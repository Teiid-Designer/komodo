/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a DDL statement option from a relational model.
 */
public interface StatementOption extends RelationalObject {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.STATEMENT_OPTION;

    /**
     * An empty collection of index constraints.
     */
    StatementOption[] NO_OPTIONS = new StatementOption[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the statement option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOption( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOption
     *        the new value for the <code>statement option</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOption( final UnitOfWork transaction,
                    final String newOption ) throws KException;

}
