/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.api.JcrConstants;

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
    default InputStream getContent( final UnitOfWork transaction ) throws KException {
        if ( !hasChild( transaction, JcrLexicon.CONTENT.getString() ) ) return null;

        final KomodoObject fileNode = getChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
        final Property property = fileNode.getProperty( transaction, JcrConstants.JCR_DATA );
        return property.getBinaryValue( transaction );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param content
     *        the bytes of content (can be <code>null</code> if there is no content)
     * @throws KException
     *         if an error occurs
     */
    default void setContent( final UnitOfWork transaction,
                             final byte[] content ) throws KException {
        KomodoObject fileNode = null;

        if ( !hasChild( transaction, JcrConstants.JCR_CONTENT ) ) {
            fileNode = addChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
        } else {
            fileNode = getChild( transaction, JcrConstants.JCR_CONTENT );
        }

        final ByteArrayInputStream stream = new ByteArrayInputStream( content );
        fileNode.setProperty( transaction, JcrConstants.JCR_DATA, stream );
    }

}
