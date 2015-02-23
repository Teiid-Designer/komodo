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
 * Represents a relational model index.
 */
public interface Index extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = Index.class.hashCode();

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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the index expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getExpression( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newExpression
     *        the new expression (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setExpression( final UnitOfWork transaction,
                        final String newExpression ) throws KException;

}
