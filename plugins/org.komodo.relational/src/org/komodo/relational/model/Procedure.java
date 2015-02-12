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
 * Represents a relational model procedure.
 */
public interface Procedure extends AbstractProcedure {

    /**
     * The default value for the <code>non-prepared</code> property. Value is {@value} .
     */
    boolean DEFAULT_NON_PREPARED = false;

    /**
     * The default value of this table's update count. Value is {@value} .
     */
    int DEFAULT_UPDATE_COUNT = 0;

    /**
     * An empty array of procedures.
     */
    Procedure[] NO_PROCEDURES = new Procedure[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>update count</code> property
     * @throws KException
     *         if an error occurs
     */
    int getUpdateCount( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if non-prepared
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    boolean isNonPrepared( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newNonPrepared
     *        the new value for the <code>non-prepared</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    void setNonPrepared( final UnitOfWork transaction,
                         final boolean newNonPrepared ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newUpdateCount
     *        the new value of the <code>update count</code> property
     * @throws KException
     *         if an error occurs
     */
    void setUpdateCount( final UnitOfWork transaction,
                         final int newUpdateCount ) throws KException;

}
