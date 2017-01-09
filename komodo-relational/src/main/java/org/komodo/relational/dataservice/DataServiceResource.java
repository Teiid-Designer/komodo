/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import java.io.InputStream;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service file resource (i.e., a driver, UDF, or DDL file).
 */
public interface DataServiceResource extends Exportable, RelationalObject {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the binary contents of this resource as an {@link InputStream} (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    InputStream getContent( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param content
     *        the bytes of content (can be <code>null</code> if there is no content)
     * @throws KException
     *         if an error occurs
     */
    void setContent( final UnitOfWork transaction,
                             final byte[] content ) throws KException;

}
