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
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a VDB permission mask.
 */
public interface Mask extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Mask.class.hashCode();

    /**
     * An empty array of masks.
     */
    Mask[] NO_MASKS = new Mask[0];

    /**
     * A name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>order</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOrder( final UnitOfWork transaction ) throws KException;

    /**
     * Sets the name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newOrder
     *        the new value of the <code>order</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOrder( final UnitOfWork transaction,
                   final String newOrder ) throws KException;

}
