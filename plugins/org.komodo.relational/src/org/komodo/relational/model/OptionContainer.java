/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KNode;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * Indicates the implementing class may have {@link StatementOption options}.
 */
public interface OptionContainer extends KNode {

    /**
     * Common utilities for {@link OptionContainer}s.
     */
    class Utils {

        /**
         * @param uow
         *        the transaction (can be <code>null</code> if query should be automatically committed)
         * @param container
         *        the option container whose statement option is being requested (cannot be <code>null</code>)
         * @param name
         *        the name of the statement option being requested (cannot be empty)
         * @return the statement option or <code>null</code> if not found
         * @throws KException
         *         if an error occurs
         */
        public static StatementOption getOption( final UnitOfWork uow,
                                                 final OptionContainer container,
                                                 final String name ) throws KException {
            ArgCheck.isNotNull(container, "container"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

            UnitOfWork transaction = uow;

            if (transaction == null) {
                final String transactionName = (container.getClass().getSimpleName() + "-get" + name); //$NON-NLS-1$
                transaction = container.getRepository().createTransaction(transactionName, true, null);
            }

            assert (transaction != null);

            try {
                StatementOption result = null;
                final StatementOption[] options = container.getStatementOptions(transaction);

                if (options.length != 0) {
                    for (final StatementOption option : options) {
                        if (name.equals(option.getName(transaction))) {
                            result = option;
                            break;
                        }
                    }
                }

                if (uow == null) {
                    transaction.commit();
                }

                return result;
            } catch (final Exception e) {
                throw ObjectImpl.handleError(uow, transaction, e);
            }
        }

    }

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the user-defined, built-in, and any other non-standard statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param optionToRemove
     *        the name of the statement option being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeStatementOption( final UnitOfWork transaction,
                                final String optionToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
