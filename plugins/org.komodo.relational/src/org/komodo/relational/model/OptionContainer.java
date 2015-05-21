/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Indicates the implementing class may have {@link StatementOption options}.
 */
public interface OptionContainer extends KomodoObject {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the user-defined and any other non-standard statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException;

    /**
     * @return the names of the standard options that this object may have (never <code>null</code> but can be empty)
     */
    String[] getStandardOptionNames();

    /**
     * This result includes both the standard statement options and any custom options that have been set.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the statement option names for this object (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getStatementOptionNames( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the statement options that have been set (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the name of the option being checked (cannot be empty)
     * @return <code>true</code> if the custom option exists
     * @throws KException
     *         if an error occurs
     */
    boolean isCustomOption( final UnitOfWork transaction,
                            final String name ) throws KException;

    /**
     * A standard option is a statement option that is built-in/well known.
     *
     * @param name
     *        the name of the option being checked (cannot be empty)
     * @return <code>true</code> if a standard option
     */
    boolean isStandardOption( final String name );

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param optionToRemove
     *        the name of the statement option being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeStatementOption( final UnitOfWork transaction,
                                final String optionToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param optionName
     *        the name of the statement option being added (cannot be empty)
     * @param optionValue
     *        the statement option value (can be empty if removing the option)
     * @return the statement option (<code>null</code> if removed)
     * @throws KException
     *         if an error occurs
     */
    StatementOption setStatementOption( final UnitOfWork transaction,
                                        final String optionName,
                                        final String optionValue ) throws KException;

}
