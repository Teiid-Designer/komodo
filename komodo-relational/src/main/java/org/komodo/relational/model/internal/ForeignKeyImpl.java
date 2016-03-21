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
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;

/**
 * An implementation of a relational model foreign key.
 */
public final class ForeignKeyImpl extends TableConstraintImpl implements ForeignKey {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a foreign key
     */
    public ForeignKeyImpl( final UnitOfWork uow,
                           final Repository repository,
                           final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return ForeignKey.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#addReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void addReferencesColumn( final UnitOfWork transaction,
                                     final Column newReferencesColumn ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( newReferencesColumn, "newReferencesColumn" ); //$NON-NLS-1$

        String[] newValue = null;
        final Property property = getProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES );
        final String columnId = newReferencesColumn.getRawProperty( transaction, JcrLexicon.UUID.getString() ).getStringValue( transaction );

        if ( property == null ) {
            newValue = new String[ 1 ];
            newValue[0] = columnId;
        } else {
            final String[] columnRefs = property.getStringValues( transaction );
            newValue = new String[ columnRefs.length + 1 ];
            System.arraycopy( columnRefs, 0, newValue, 0, columnRefs.length );
            newValue[columnRefs.length] = columnId;
        }

        setProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )newValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getConstraintType()
     */
    @Override
    public ConstraintType getConstraintType() {
        return ConstraintType.FOREIGN_KEY;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#getReferencesColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getReferencesColumns( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Repository repository = getRepository();
        Column[] result = null;

        final Property property = getProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES );

        if ( property == null ) {
            result = new Column[ 0 ];
        } else {
            final String[] columnRefs = property.getStringValues( transaction );
            result = new Column[ columnRefs.length ];
            int i = 0;

            for ( final String columnId : columnRefs ) {
                final KomodoObject kobject = repository.getUsingId( transaction, columnId );

                if ( kobject == null ) {
                    throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
                }

                result[i] = new ColumnImpl( transaction, repository, kobject.getAbsolutePath() );
                ++i;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#getReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getReferencesTable( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Table result = null;
        final Property property = getProperty( transaction, Constraint.TABLE_REFERENCE );

        if ( property != null ) {
            final String tableId = property.getStringValue( transaction );
            final KomodoObject kobject = getRepository().getUsingId( transaction, tableId );

            if ( kobject == null ) {
                throw new KException( Messages.getString( Relational.REFERENCED_TABLE_NOT_FOUND, tableId ) );
            }

            result = new TableImpl( transaction, getRepository(), kobject.getAbsolutePath() );
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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Table result = Table.RESOLVER.resolve( transaction, parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#removeReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void removeReferencesColumn( final UnitOfWork transaction,
                                        final Column removeReferencesColumn ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( removeReferencesColumn, "removeReferencesColumn" ); //$NON-NLS-1$

        final String columnId = removeReferencesColumn.getRawProperty( transaction, JcrLexicon.UUID.getString() ).getStringValue( transaction );
        final Column[] current = getReferencesColumns( transaction );

        if ( current.length == 0 ) {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }

        setProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )null );

        boolean found = false;
//        final Column[] updated = new Column[ current.length - 1 ];
//        int i = 0;

        for ( final Column column : current ) {
            if ( column.equals( removeReferencesColumn ) ) {
                found = true;
            } else {
                addReferencesColumn( transaction, column );
//                updated[i] = column;
//                ++i;
            }
        }

        if ( found ) {
//            setProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )updated );
        } else {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#setReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Table)
     */
    @Override
    public void setReferencesTable( final UnitOfWork transaction,
                                    final Table newReferencesTable ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( newReferencesTable, "newReferencesTable" ); //$NON-NLS-1$

        // only set if different
        final Table current = getReferencesTable( transaction );

        if ( !newReferencesTable.equals( current ) ) {
            final String tableId = newReferencesTable.getRawProperty( transaction,
                                                                      JcrLexicon.UUID.getString() ).getStringValue( transaction );
            setProperty( transaction, Constraint.TABLE_REFERENCE, tableId );

            if ( ( current != null ) && hasProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES ) ) {
                // remove reference columns because they pertained to previous referenced table
                setProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )null );
            }
        }
    }

}
