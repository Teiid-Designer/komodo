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
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

/**
 * A base implementation of a relational model table constraint.
 */
abstract class TableConstraintImpl extends RelationalObjectImpl implements TableConstraint {

    protected TableConstraintImpl( final UnitOfWork uow,
                                   final Repository repository,
                                   final String path ) throws KException {
        super(uow, repository, path);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#addColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void addColumn( final UnitOfWork uow,
                           final Column columnToAdd ) throws KException {
        ArgCheck.isNotNull(columnToAdd, "columnToAdd"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + "-addColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addColumn: transaction = {0}, columnToAdd = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         columnToAdd.getName(transaction));
        }

        try {
            String[] newRefs = null;
            final Property property = getProperty(transaction, Constraint.REFERENCES);
            final String columnId = columnToAdd.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue(transaction);

            if (property == null) {
                newRefs = new String[] {columnId};
            } else {
                // add to existing multi-valued property
                final String[] columnRefs = property.getStringValues(transaction);
                newRefs = new String[columnRefs.length + 1];
                System.arraycopy(columnRefs, 0, newRefs, 0, columnRefs.length);
                newRefs[columnRefs.length] = columnId;
            }

            setProperty(transaction, Constraint.REFERENCES, (Object[])newRefs);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) {
        return KomodoObject.EMPTY_ARRAY; // table constraints do not have children
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) {
        return KomodoObject.EMPTY_ARRAY; // table constraints do not have children
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getColumns( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + "-getColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final Repository repository = getRepository();
        Column[] result = null;

        try {
            final Property property = getProperty(transaction, Constraint.REFERENCES);

            if (property == null) {
                result = new Column[0];
            } else {
                final String[] columnRefs = property.getStringValues(transaction);
                result = new Column[columnRefs.length];
                int i = 0;

                for (final String columnId : columnRefs) {
                    final KomodoObject kobject = repository.getUsingId(transaction, columnId);

                    if (kobject == null) {
                        throw new KException(Messages.getString(Relational.REFERENCED_COLUMN_NOT_FOUND, columnId));
                    }

                    result[i] = new ColumnImpl(transaction, repository, kobject.getAbsolutePath());
                    ++i;
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getTable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + "-getTable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            Table result = null;
            final KomodoObject parent = getParent(transaction);

            if (parent.hasDescriptor(transaction, CreateTable.TABLE_STATEMENT)) {
                result = new TableImpl(transaction, getRepository(), parent.getAbsolutePath());
            } else if (parent.hasDescriptor(transaction, CreateTable.VIEW_STATEMENT)) {
                result = new ViewImpl(transaction, getRepository(), parent.getAbsolutePath());
            } else {
                throw new KException(Messages.getString(Relational.UNEXPECTED_TABLE_TYPE));
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#removeColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void removeColumn( final UnitOfWork uow,
                              final Column columnToRemove ) throws KException {
        ArgCheck.isNotNull(columnToRemove, "columnToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction(getClass().getSimpleName() + "-removeColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeColumn: transaction = {0}, columnToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         columnToRemove);
        }

        assert (columnToRemove != null);

        try {
            final String columnId = columnToRemove.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue(transaction);
            final Column[] current = getColumns(transaction);

            if (current.length == 0) {
                throw new KException(Messages.getString(Relational.REFERENCED_COLUMN_NOT_FOUND, columnId));
            }

            boolean found = false;
            final Column[] updated = new Column[current.length - 1];
            int i = 0;

            for (final Column column : current) {
                if (column.equals(columnToRemove)) {
                    found = true;
                } else {
                    updated[i] = column;
                    ++i;
                }
            }

            if (found) {
                setProperty(transaction, Constraint.REFERENCES, (Object[])updated);

                if (uow == null) {
                    transaction.commit();
                }
            } else {
                throw new KException(Messages.getString(Relational.REFERENCED_COLUMN_NOT_FOUND, columnId));
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
