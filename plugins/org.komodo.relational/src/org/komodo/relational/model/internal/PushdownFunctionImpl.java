/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * An implementation of a pushdown function.
 */
public final class PushdownFunctionImpl extends FunctionImpl implements PushdownFunction {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] KID_TYPES;

    static {
        KID_TYPES = new KomodoType[ CHILD_TYPES.length + 2 ];
        System.arraycopy( CHILD_TYPES, 0, KID_TYPES, 0, CHILD_TYPES.length );
        KID_TYPES[ CHILD_TYPES.length ] = DataTypeResultSet.IDENTIFIER;
        KID_TYPES[ CHILD_TYPES.length + 1 ] = TabularResultSet.IDENTIFIER;
    }

    /**
     * The resolver of a {@link PushdownFunction}.
     */
    public static final TypeResolver< PushdownFunction > RESOLVER = new TypeResolver< PushdownFunction >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public PushdownFunction create( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject parent,
                                        final String id,
                                        final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Model parentModel = adapter.adapt( transaction, parent, Model.class );

            if ( parentModel == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          PushdownFunction.class.getSimpleName() ) );
            }

            return parentModel.addPushdownFunction( transaction, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< PushdownFunctionImpl > owningClass() {
            return PushdownFunctionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.FUNCTION_STATEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        SchemaElement.TYPE,
                                                        SchemaElementType.FOREIGN.name() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public PushdownFunction resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            return new PushdownFunctionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

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
    public PushdownFunctionImpl( final UnitOfWork uow,
                                 final Repository repository,
                                 final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return KID_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.PushdownFunction#getResultSet(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ProcedureResultSet getResultSet( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        ProcedureResultSet result = null;

        if ( hasChild( transaction, CreateProcedure.RESULT_SET ) ) {
            final KomodoObject kobject = getChild( transaction, CreateProcedure.RESULT_SET );

            if ( DataTypeResultSetImpl.RESOLVER.resolvable( transaction, kobject ) ) {
                result = DataTypeResultSetImpl.RESOLVER.resolve( transaction, kobject );
            } else if ( TabularResultSetImpl.RESOLVER.resolvable( transaction, kobject ) ) {
                result = TabularResultSetImpl.RESOLVER.resolve( transaction, kobject );
            } else {
                throw new UnsupportedOperationException( Messages.getString( Relational.UNEXPECTED_RESULT_SET_TYPE,
                                                                             kobject.getAbsolutePath() ) );
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.PushdownFunction#removeResultSet(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void removeResultSet( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // delete existing result set
        final ProcedureResultSet resultSet = getResultSet( transaction );

        if ( resultSet == null ) {
            throw new KException( Messages.getString( Relational.RESULT_SET_NOT_FOUND_TO_REMOVE, getAbsolutePath() ) );
        }

        resultSet.remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.PushdownFunction#setResultSet(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.Class)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public < T extends ProcedureResultSet > T setResultSet( final UnitOfWork transaction,
                                                            final Class< T > resultSetType ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // delete existing result set (don't call removeResultSet as it throws exception if one does not exist)
        final ProcedureResultSet resultSet = getResultSet( transaction );

        if ( resultSet != null ) {
            resultSet.remove( transaction );
        }

        T result = null;

        if ( resultSetType == TabularResultSet.class ) {
            result = ( T )RelationalModelFactory.createTabularResultSet( transaction, getRepository(), this );
        } else if ( resultSetType == DataTypeResultSet.class ) {
            result = ( T )RelationalModelFactory.createDataTypeResultSet( transaction, getRepository(), this );
        }

        return result;
    }

}
