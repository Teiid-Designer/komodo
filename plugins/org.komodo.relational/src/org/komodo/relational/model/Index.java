/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a relational model index.
 */
public interface Index extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = Index.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.INDEX;

    /**
     * The constraint type for an index. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.INDEX;

    /**
     * An empty collection of index constraints.
     */
    Index[] NO_INDEXES = new Index[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the index expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getExpression( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newExpression
     *        the new expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setExpression( final UnitOfWork transaction,
                        final String newExpression ) throws KException;

}
