/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice;

import java.util.Properties;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.DataServiceEntry.PublishPolicy;

/**
 * Represents an entry in a data service archive.
 *
 * @param <T>
 *        the entry type
 */
public interface DataServiceEntry< T extends Exportable & RelationalObject > extends Exportable, RelationalObject {

    /**
     * Empty resource content.
     */
    byte[] NO_CONTENT = new byte[0];

    /**
     * @return the archive path segment where the resource should be archived (can be <code>null</code> or empty if the resource
     *         should be located at the archive root)
     */
    String getArchiveFolder();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    byte[] export( final UnitOfWork transaction,
                           final Properties properties ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#getDocumentType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    DocumentType getDocumentType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the entry path (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    String getEntryPath( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the entry's publish policy (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PublishPolicy getPublishPolicy( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the referenced object or <code>null</code> if none exists
     * @throws KException
     *         if an error occurs
     */
    T getReference( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newEntryPath
     *        the new entry path (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setEntryPath( final UnitOfWork transaction,
                               final String newEntryPath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newPublishPolicy
     *        the new publish policy (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setPublishPolicy( final UnitOfWork transaction,
                                   final PublishPolicy newPublishPolicy ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param reference
     *        the referenced object or <code>null</code> if removing an existing reference
     * @throws KException
     *         if an error occurs
     */
    void setReference( final UnitOfWork transaction,
                               final T reference ) throws KException;

}
