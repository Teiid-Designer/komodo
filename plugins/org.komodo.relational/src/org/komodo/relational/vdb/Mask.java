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
 * Represents a VDB permission mask.
 */
public interface Mask extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Mask.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_MASK;

    /**
     * An empty array of masks.
     */
    Mask[] NO_MASKS = new Mask[0];

    /**
     * A name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>order</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOrder( final UnitOfWork transaction ) throws KException;

    /**
     * Sets the name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOrder
     *        the new value of the <code>order</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOrder( final UnitOfWork transaction,
                   final String newOrder ) throws KException;

}
