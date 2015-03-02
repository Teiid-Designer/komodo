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

/**
 * Represents a virtual procedure (CREATE VIRTUAL PROCEDURE).
 */
public interface VirtualProcedure extends Procedure {

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.VIRTUAL_PROCEDURE;

    /**
     * An empty array of virtual procedures.
     */
    VirtualProcedure[] NO_PROCEDURES = new VirtualProcedure[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = VirtualProcedure.class.hashCode();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the AS clause <code>statement</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAsClauseStatement( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newStatement
     *        the new AS clause statement (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAsClauseStatement( final UnitOfWork transaction,
                               final String newStatement ) throws KException;

}
