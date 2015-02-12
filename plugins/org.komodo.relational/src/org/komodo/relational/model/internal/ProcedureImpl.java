/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * An implementation of a relational model procedure.
 */
public final class ProcedureImpl extends AbstractProcedureImpl implements Procedure {

    /*
      - teiidddl:schemaElementType (string) = 'FOREIGN' mandatory autocreated < 'FOREIGN', 'VIRTUAL'
      + * (teiidddl:procedureParameter) = teiidddl:procedureParameter
      + resultSet (teiidddl:resultSet)
      + * (ddl:statementOption) = ddl:statementOption
      - teiidddl:statement (string) -- procedure only, not function
     */

    private enum StandardOptions {

        NON_PREPARED( "non-prepared" ), //$NON-NLS-1$
        UPDATECOUNT( "UPDATECOUNT" ); //$NON-NLS-1$

        private final String name;

        private StandardOptions( final String optionName ) {
            this.name = optionName;
        }

        public String getName() {
            return this.name;
        }

    }

    /**
     * The resolver of a {@link Procedure}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, CreateProcedure.PROCEDURE_STATEMENT);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Procedure resolve( final UnitOfWork transaction,
                                  final Repository repository,
                                  final KomodoObject kobject ) throws KException {
            return new ProcedureImpl(transaction, repository, kobject.getAbsolutePath());
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a procedure
     */
    public ProcedureImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-getCustomOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption[] allOptions = getStatementOptions(transaction);

        if (allOptions.length == 0) {
            return allOptions;
        }

        final StatementOption[] superOptions = super.getCustomOptions(transaction);
        final List< StatementOption > temp = new ArrayList<>(allOptions.length);

        for (final StatementOption option : allOptions) {
            if (StandardOptions.valueOf(option.getName(transaction)) == null) {
                temp.add(option);
            }
        }

        if (temp.isEmpty()) {
            return superOptions;
        }

        final StatementOption[] result = temp.toArray(new StatementOption[temp.size()]);

        if (superOptions.length == 0) {
            return result;
        }

        final StatementOption[] combined = new StatementOption[superOptions.length + result.length];
        System.arraycopy(result, 0, combined, 0, result.length);
        System.arraycopy(superOptions, 0, combined, result.length, superOptions.length);

        return combined;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#getUpdateCount(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getUpdateCount( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.UPDATECOUNT.getName());

        if (option == null) {
            return Procedure.DEFAULT_UPDATE_COUNT;
        }

        return Integer.parseInt(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#isNonPrepared(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isNonPrepared( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NON_PREPARED.getName());

        if (option == null) {
            return Procedure.DEFAULT_NON_PREPARED;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#setNonPrepared(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setNonPrepared( final UnitOfWork transaction,
                                final boolean newNonPrepared ) throws KException {
        setStatementOption(transaction, StandardOptions.NON_PREPARED.getName(), Boolean.toString(newNonPrepared));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#setUpdateCount(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setUpdateCount( final UnitOfWork transaction,
                                final int newUpdateCount ) throws KException {
        setStatementOption(transaction, StandardOptions.UPDATECOUNT.getName(), Integer.toString(newUpdateCount));
    }

}
