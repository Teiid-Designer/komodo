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
 * Indicates the object has a string representation that can be exported. In many cases this is an XML represention.
 */
public interface Exportable {

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return a text representation of the current object state (never empty)
     * @throws KException
     *         if an error occurs
     */
    String export( final UnitOfWork transaction ) throws KException;

}
