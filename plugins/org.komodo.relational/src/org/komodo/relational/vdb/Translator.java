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
 * Represents a VDB translator.
 */
public interface Translator extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Translator.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_TRANSLATOR;

    /**
     * The default value for a translator type. Value is {@value} .
     */
    String DEFAULT_TYPE = "oracle"; //$NON-NLS-1$

    /**
     * An empty array of translators.
     */
    Translator[] NO_TRANSLATORS = new Translator[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>type</code> property (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newType
     *        the new value of the <code>type</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setType( final UnitOfWork transaction,
                  final String newType ) throws KException;

}
