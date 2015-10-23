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
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * An implementation of a relational model procedure tabular result set.
 */
public final class TabularResultSetImpl extends RelationalObjectImpl implements TabularResultSet {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { ResultSetColumn.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a procedure result set
     */
    public TabularResultSetImpl( final UnitOfWork uow,
                                 final Repository repository,
                                 final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TabularResultSet#addColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public ResultSetColumn addColumn( final UnitOfWork transaction,
                                      final String columnName ) throws KException {
        return RelationalModelFactory.createResultSetColumn( transaction, getRepository(), this, columnName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TabularResultSet#getColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ResultSetColumn[] getColumns( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ResultSetColumn > result = new ArrayList<>();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, CreateProcedure.RESULT_COLUMN ) ) {
            final ResultSetColumn column = new ResultSetColumnImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( column );
        }

        if ( result.isEmpty() ) {
            return ResultSetColumn.NO_COLUMNS;
        }

        return result.toArray( new ResultSetColumn[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.internal.TableImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return TabularResultSet.RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TabularResultSet#removeColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeColumn( final UnitOfWork transaction,
                              final String columnToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( columnToRemove, "columnToRemove" ); //$NON-NLS-1$

        boolean found = false;
        final ResultSetColumn[] columns = getColumns( transaction );

        if ( columns.length != 0 ) {
            for ( final ResultSetColumn column : columns ) {
                if ( columnToRemove.equals( column.getName( transaction ) ) ) {
                    column.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.COLUMN_NOT_FOUND_TO_REMOVE, columnToRemove ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public final void rename( final UnitOfWork transaction,
                              final String newName ) throws UnsupportedOperationException {
        throw new UnsupportedOperationException( Messages.getString( Relational.RENAME_NOT_ALLOWED, getAbsolutePath() ) );
    }

}
