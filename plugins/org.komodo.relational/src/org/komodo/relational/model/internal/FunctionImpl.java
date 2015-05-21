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
import org.komodo.relational.model.Function;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An base implementation of a relational model function.
 */
public abstract class FunctionImpl extends AbstractProcedureImpl implements Function {

    private enum StandardOption {

        AGGREGATE( "AGGREGATE" ), //$NON-NLS-1$
        ALLOWS_DISTINCT( "ALLOWS-DISTINCT" ), //$NON-NLS-1$
        ALLOWS_ORDERBY( "ALLOWS-ORDERBY" ), //$NON-NLS-1$
        ANALYTIC( "ANALYTIC" ), //$NON-NLS-1$
        DECOMPOSABLE( "DECOMPOSABLE" ), //$NON-NLS-1$
        DETERMINISM( "DETERMINISM" ), //$NON-NLS-1$
        NULL_ON_NULL( "NULL-ON-NULL" ), //$NON-NLS-1$
        USES_DISTINCT_ROWS( "USES-DISTINCT-ROWS" ), //$NON-NLS-1$
        VARARGS( "VARARGS" ); //$NON-NLS-1$

        /**
         * @param name
         *        the name being checked (can be <code>null</code>)
         * @return <code>true</code> if the name is the name of a standard option
         */
        static boolean isValid( final String name ) {
            for ( final StandardOption option : values() ) {
                if ( option.name().equals( name ) ) {
                    return true;
                }
            }

            return false;
        }

        /**
         * @return the names of all the options (never <code>null</code> or empty)
         */
        static String[] names() {
            final StandardOption[] options = values();
            final String[] result = new String[ options.length ];
            int i = 0;

            for ( final StandardOption option : options ) {
                result[i++] = option.name();
            }

            return result;
        }

        private final String name;

        private StandardOption( final String optionName ) {
            this.name = optionName;
        }

        protected String getName() {
            return this.name;
        }

    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a procedure
     */
    protected FunctionImpl( final UnitOfWork uow,
                            final Repository repository,
                            final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // get super custom options then delete any subclass standard options
        final StatementOption[] superOptions = super.getCustomOptions( transaction );
        StatementOption[] result = StatementOption.NO_OPTIONS;

        if ( superOptions.length != 0 ) {
            final List< StatementOption > temp = new ArrayList<>( superOptions.length );

            for ( final StatementOption option : superOptions ) {
                if ( !isStandardOption( option.getName( transaction ) ) ) {
                    temp.add( option );
                }
            }

            if ( !temp.isEmpty() ) {
                result = temp.toArray( new StatementOption[ temp.size() ] );
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#getDeterminism(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Determinism getDeterminism( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.DETERMINISM.getName() );

        if (option == null) {
            return Determinism.DEFAULT_VALUE;
        }

        return Determinism.valueOf( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStandardOptionNames()
     */
    @Override
    public String[] getStandardOptionNames() {
        final String[] superNames = super.getStandardOptionNames();
        final String[] names = StandardOption.names();

        // combine
        final String[] result = new String[ superNames.length + names.length ];
        System.arraycopy( superNames, 0, result, 0, superNames.length );
        System.arraycopy( names, 0, result, superNames.length, names.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAggregate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAggregate( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.AGGREGATE.getName() );

        if (option == null) {
            return Function.DEFAULT_AGGREGATE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAllowsDistinct(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowsDistinct( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.ALLOWS_DISTINCT.getName() );

        if (option == null) {
            return Function.DEFAULT_ALLOWS_DISTINCT;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAllowsOrderBy(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowsOrderBy( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.ALLOWS_ORDERBY.getName() );

        if (option == null) {
            return Function.DEFAULT_ALLOWS_ORDER_BY;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isAnalytic(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAnalytic( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.ANALYTIC.getName() );

        if (option == null) {
            return Function.DEFAULT_ANALYTIC;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isDecomposable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isDecomposable( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.DECOMPOSABLE.getName() );

        if (option == null) {
            return Function.DEFAULT_DECOMPOSABLE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isNullOnNull(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isNullOnNull( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.NULL_ON_NULL.getName() );

        if (option == null) {
            return Function.DEFAULT_NULL_ON_NULL;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isStandardOption(java.lang.String)
     */
    @Override
    public boolean isStandardOption( final String name ) {
        return ( super.isStandardOption( name ) || StandardOption.isValid( name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isUsesDistinctRows(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUsesDistinctRows( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.USES_DISTINCT_ROWS.getName() );

        if (option == null) {
            return Function.DEFAULT_USES_DISTINCT_ROWS;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#isVarArgs(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isVarArgs( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.VARARGS.getName() );

        if (option == null) {
            return Function.DEFAULT_VARARGS;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAggregate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAggregate( final UnitOfWork transaction,
                              final boolean newAggregate ) throws KException {
        setStatementOption( transaction, StandardOption.AGGREGATE.getName(), Boolean.toString( newAggregate ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAllowsDistinct(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowsDistinct( final UnitOfWork transaction,
                                   final boolean newAllowsDistinct ) throws KException {
        setStatementOption( transaction, StandardOption.ALLOWS_DISTINCT.getName(), Boolean.toString( newAllowsDistinct ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAllowsOrderBy(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowsOrderBy( final UnitOfWork transaction,
                                  final boolean newAllowsOrderBy ) throws KException {
        setStatementOption( transaction, StandardOption.ALLOWS_ORDERBY.getName(), Boolean.toString( newAllowsOrderBy ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setAnalytic(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAnalytic( final UnitOfWork transaction,
                             final boolean newIsAnalytic ) throws KException {
        setStatementOption( transaction, StandardOption.ANALYTIC.getName(), Boolean.toString( newIsAnalytic ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setDecomposable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setDecomposable( final UnitOfWork transaction,
                                 final boolean newDecomposable ) throws KException {
        setStatementOption( transaction, StandardOption.DECOMPOSABLE.getName(), Boolean.toString( newDecomposable ) );
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
        final String value = ( ( newDeterminism == null ) ? Determinism.DEFAULT_VALUE.toString() : newDeterminism.name() );
        setStatementOption( transaction, StandardOption.DETERMINISM.getName(), value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setNullOnNull(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setNullOnNull( final UnitOfWork transaction,
                               final boolean newNullOnNull ) throws KException {
        setStatementOption( transaction, StandardOption.NULL_ON_NULL.getName(), Boolean.toString( newNullOnNull ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setUsesDistinctRows(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUsesDistinctRows( final UnitOfWork transaction,
                                     final boolean newUsesDistinctRows ) throws KException {
        setStatementOption( transaction, StandardOption.USES_DISTINCT_ROWS.getName(), Boolean.toString( newUsesDistinctRows ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Function#setVarArgs(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setVarArgs( final UnitOfWork transaction,
                            final boolean newVarArgs ) throws KException {
        setStatementOption( transaction, StandardOption.VARARGS.getName(), Boolean.toString( newVarArgs ) );
    }

}
