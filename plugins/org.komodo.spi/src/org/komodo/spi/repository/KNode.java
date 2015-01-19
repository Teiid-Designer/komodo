/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A Komodo object.
 */
public interface KNode {

    /**
     * @return the {@link KomodoObject Komodo object's} absolute path (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getAbsolutePath();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the last segment of the absolute path (never empty)
     * @throws KException
     *         if an error occurs
     * @see #getAbsolutePath()
     */
    String getName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the parent {@link KomodoObject Komodo object} (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getParent( final UnitOfWork transaction ) throws KException;

    /**
     * @return the repository where this object is found (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Repository getRepository() throws KException;

}
