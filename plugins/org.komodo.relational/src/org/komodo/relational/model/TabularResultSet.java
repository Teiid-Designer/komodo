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
import org.komodo.relational.model.internal.TabularResultSetImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * Represents a tabular result set.
 */
public interface TabularResultSet extends ProcedureResultSet {

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.TABULAR_RESULT_SET;

    /**
     * The type identifier.
     */
    int TYPE_ID = TabularResultSet.class.hashCode();
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    AbstractProcedure getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link TabularResultSet}.
     */
    public static final TypeResolver< TabularResultSet > RESOLVER = new TypeResolver< TabularResultSet >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public TabularResultSet create( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject parent,
                                        final String id,
                                        final RelationalProperties properties ) throws KException {
            final Class< ? extends AbstractProcedure > clazz = AbstractProcedure.Utils.getProcedureType( transaction, parent );
            final AdapterFactory adapter = new AdapterFactory( );
            final AbstractProcedure parentProc = adapter.adapt( transaction, parent, clazz );

            if ( parentProc == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          TabularResultSet.class.getSimpleName() ) );
            }

            if ( parentProc instanceof StoredProcedure ) {
                return ( ( StoredProcedure )parentProc ).setResultSet( transaction, TabularResultSet.class );
            }

            if ( parentProc instanceof PushdownFunction ) {
                return ( ( PushdownFunction )parentProc ).setResultSet( transaction, TabularResultSet.class );
            }

            throw new KException( Messages.getString( Relational.UNEXPECTED_RESULT_SET_TYPE, clazz.getName() ) );
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
        public Class< TabularResultSetImpl > owningClass() {
            return TabularResultSetImpl.class;
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
            // must have the right name
            if ( CreateProcedure.RESULT_SET.equals( kobject.getName( transaction ) ) ) {
                return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.RESULT_COLUMNS );
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public TabularResultSet resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == TabularResultSet.TYPE_ID ) {
                return ( TabularResultSet )kobject;
            }

            return new TabularResultSetImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnName
     *        the name of the column being added (cannot be empty)
     * @return the new column (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ResultSetColumn addColumn( final UnitOfWork transaction,
                               final String columnName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the columns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ResultSetColumn[] getColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToRemove
     *        the name of the column being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final String columnToRemove ) throws KException;

    /**
     * <p>
     * <strong><em>Rename is not allowed!!</em></strong>
     *
     * @see org.komodo.spi.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException if called
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws UnsupportedOperationException;

}
