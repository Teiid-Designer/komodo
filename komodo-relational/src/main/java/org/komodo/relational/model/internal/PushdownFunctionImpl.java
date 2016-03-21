/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;

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

            if ( DataTypeResultSet.RESOLVER.resolvable( transaction, kobject ) ) {
                result = DataTypeResultSet.RESOLVER.resolve( transaction, kobject );
            } else if ( TabularResultSet.RESOLVER.resolvable( transaction, kobject ) ) {
                result = TabularResultSet.RESOLVER.resolve( transaction, kobject );
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
        return PushdownFunction.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Model result = Model.RESOLVER.resolve( transaction, parent );
        return result;
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
