/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import org.komodo.relational.model.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a VDB permission condition.
 */
public interface Condition extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Condition.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_CONDITION;

    /**
     * The default value indicating if this condition is a constraint. Value is {@value} .
     */
    boolean DEFAULT_CONSTRAINT = true;

    /**
     * An empty array of conditions.
     */
    Condition[] NO_CONDITIONS = new Condition[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this condition is a constraint
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CONSTRAINT
     */
    boolean isConstraint( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newConstraint
     *        the new value for the <code>constraint</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CONSTRAINT
     */
    void setConstraint( final UnitOfWork transaction,
                        final boolean newConstraint ) throws KException;

}
