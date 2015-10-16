/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.PushdownFunctionImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * Represents a pushdown function (CREATE FOREIGN FUNCTION).
 */
public interface PushdownFunction extends Function {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PUSHDOWN_FUNCTION;

    /**
     * An empty array of pushdown functions.
     */
    PushdownFunction[] NO_PUSHDOWNS = new PushdownFunction[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = PushdownFunction.class.hashCode();

    /**
     * The resolver of a {@link PushdownFunction}.
     */
    public static final TypeResolver< PushdownFunction > RESOLVER = new TypeResolver< PushdownFunction >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public PushdownFunction create( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject parent,
                                        final String id,
                                        final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
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
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< PushdownFunctionImpl > owningClass() {
            return PushdownFunctionImpl.class;
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
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
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public PushdownFunction resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            return new PushdownFunctionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the result set (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ProcedureResultSet getResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @throws KException
     *         if a result set does not exist or an error occurs
     */
    void removeResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * Deletes the current result set and returns a new one of the requested type.
     *
     * @param <T>
     *        the type of result set
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param resultSetType
     *        the type of result set being requested
     * @return the new result set (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see TabularResultSet
     * @see DataTypeResultSet
     */
    < T extends ProcedureResultSet > T setResultSet( final UnitOfWork transaction,
                                                     final Class< T > resultSetType ) throws KException;

}
