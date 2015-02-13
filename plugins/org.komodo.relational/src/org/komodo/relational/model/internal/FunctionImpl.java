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
import org.komodo.relational.model.Function;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * An implementation of a relational model function.
 */
public class FunctionImpl extends AbstractProcedureImpl implements Function {

    private enum StandardOptions {

        AGGREGATE( "AGGREGATE" ), //$NON-NLS-1$
        ALLOWS_DISTINCT( "ALLOWS-DISTINCT" ), //$NON-NLS-1$
        ALLOWS_ORDERBY( "ALLOWS-ORDERBY" ), //$NON-NLS-1$
        ANALYTIC( "ANALYTIC" ), //$NON-NLS-1$
        CATEGORY( "CATEGORY" ), //$NON-NLS-1$
        DECOMPOSABLE( "DECOMPOSABLE" ), //$NON-NLS-1$
        DETERMINISM( "DETERMINISM" ), //$NON-NLS-1$
        JAVA_CLASS( "JAVA_CLASS" ), //$NON-NLS-1$
        JAVA_METHOD( "JAVA_METHOD" ), //$NON-NLS-1$
        NULL_ON_NULL( "NULL-ON-NULL" ), //$NON-NLS-1$
        USES_DISTINCT_ROWS( "USES-DISTINCT-ROWS" ), //$NON-NLS-1$
        VARARGS( "VARARGS" ); //$NON-NLS-1$

        private final String name;

        private StandardOptions( final String optionName ) {
            this.name = optionName;
        }

        public String getName() {
            return this.name;
        }

    }

    /**
     * The resolver of a {@link Function}.
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
                ObjectImpl.validateType(transaction, repository, kobject, CreateProcedure.FUNCTION_STATEMENT);
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
        public Function resolve( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject kobject ) throws KException {
            return new FunctionImpl(transaction, repository, kobject.getAbsolutePath());
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
    public FunctionImpl( final UnitOfWork uow,
                         final Repository repository,
                         final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#getCategory(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCategory( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CATEGORY.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
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
            transaction = getRepository().createTransaction("functionimpl-getCustomOptions", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Function#getDeterminism(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Determinism getDeterminism( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.DETERMINISM.getName());

        if (option == null) {
            return Determinism.DEFAULT_VALUE;
        }

        return Determinism.valueOf(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#getJavaClass(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJavaClass( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.JAVA_CLASS.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#getJavaMethod(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJavaMethod( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.JAVA_METHOD.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAggregate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAggregate( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.AGGREGATE.getName());

        if (option == null) {
            return Function.DEFAULT_AGGREGATE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAllowsDistinct(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowsDistinct( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ALLOWS_DISTINCT.getName());

        if (option == null) {
            return Function.DEFAULT_ALLOWS_DISTINCT;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAllowsOrderBy(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowsOrderBy( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ALLOWS_ORDERBY.getName());

        if (option == null) {
            return Function.DEFAULT_ALLOWS_ORDER_BY;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAnalytic(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAnalytic( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ANALYTIC.getName());

        if (option == null) {
            return Function.DEFAULT_ANALYTIC;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isDecomposable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isDecomposable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.DECOMPOSABLE.getName());

        if (option == null) {
            return Function.DEFAULT_DECOMPOSABLE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isNullOnNull(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isNullOnNull( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NULL_ON_NULL.getName());

        if (option == null) {
            return Function.DEFAULT_NULL_ON_NULL;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isUsesDistinctRows(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUsesDistinctRows( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.USES_DISTINCT_ROWS.getName());

        if (option == null) {
            return Function.DEFAULT_USES_DISTINCT_ROWS;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isVarArgs(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isVarArgs( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.VARARGS.getName());

        if (option == null) {
            return Function.DEFAULT_VARARGS;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAggregate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAggregate( final UnitOfWork transaction,
                              final boolean newAggregate ) throws KException {
        setStatementOption(transaction, StandardOptions.AGGREGATE.getName(), Boolean.toString(newAggregate));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAllowsDistinct(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowsDistinct( final UnitOfWork transaction,
                                   final boolean newAllowsDistinct ) throws KException {
        setStatementOption(transaction, StandardOptions.ALLOWS_DISTINCT.getName(), Boolean.toString(newAllowsDistinct));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAllowsOrderBy(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowsOrderBy( final UnitOfWork transaction,
                                  final boolean newAllowsOrderBy ) throws KException {
        setStatementOption(transaction, StandardOptions.ALLOWS_ORDERBY.getName(), Boolean.toString(newAllowsOrderBy));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAnalytic(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAnalytic( final UnitOfWork transaction,
                             final boolean newIsAnalytic ) throws KException {
        setStatementOption(transaction, StandardOptions.ANALYTIC.getName(), Boolean.toString(newIsAnalytic));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setCategory(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setCategory( final UnitOfWork transaction,
                             final String newCategory ) throws KException {
        setStatementOption(transaction, StandardOptions.CATEGORY.getName(), newCategory);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setDecomposable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setDecomposable( final UnitOfWork transaction,
                                 final boolean newDecomposable ) throws KException {
        setStatementOption(transaction, StandardOptions.DECOMPOSABLE.getName(), Boolean.toString(newDecomposable));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setDeterminism(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Function.Determinism)
     */
    @Override
    public void setDeterminism( final UnitOfWork transaction,
                                final Determinism newDeterminism ) throws KException {
        final String value = ((newDeterminism == null) ? Determinism.DEFAULT_VALUE.toString() : newDeterminism.toString());
        setStatementOption(transaction, StandardOptions.DETERMINISM.getName(), value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setJavaClass(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJavaClass( final UnitOfWork transaction,
                              final String newJavaClass ) throws KException {
        setStatementOption(transaction, StandardOptions.JAVA_CLASS.getName(), newJavaClass);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setJavaMethod(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJavaMethod( final UnitOfWork transaction,
                               final String newJavaMethod ) throws KException {
        setStatementOption(transaction, StandardOptions.JAVA_METHOD.getName(), newJavaMethod);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setNullOnNull(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setNullOnNull( final UnitOfWork transaction,
                               final boolean newNullOnNull ) throws KException {
        setStatementOption(transaction, StandardOptions.NULL_ON_NULL.getName(), Boolean.toString(newNullOnNull));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setUsesDistinctRows(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUsesDistinctRows( final UnitOfWork transaction,
                                     final boolean newUsesDistinctRows ) throws KException {
        setStatementOption(transaction, StandardOptions.USES_DISTINCT_ROWS.getName(), Boolean.toString(newUsesDistinctRows));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setVarArgs(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setVarArgs( final UnitOfWork transaction,
                            final boolean newVarArgs ) throws KException {
        setStatementOption(transaction, StandardOptions.VARARGS.getName(), Boolean.toString(newVarArgs));
    }

}
