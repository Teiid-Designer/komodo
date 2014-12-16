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
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

/**
 * An implementation of a relational model foreign key.
 */
public final class ForeignKeyImpl extends TableConstraintImpl implements ForeignKey {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ForeignKeyImpl( final Repository repository,
                           final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#addReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void addReferencesColumn( final UnitOfWork uow,
                                     final Column newReferencesColumn ) throws KException {
        ArgCheck.isNotNull(newReferencesColumn, "newReferencesColumn"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("foreignkeyimpl-addReferencesColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        String[] newValue = null;

        try {
            final Property property = getProperty(transaction, Constraint.TABLE_REFERENCE_REFERENCES);
            final String columnId = newReferencesColumn.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue();

            if (property == null) {
                newValue = new String[1];
                newValue[0] = columnId;
            } else {
                final String[] columnRefs = property.getStringValues();
                newValue = new String[columnRefs.length + 1];
                System.arraycopy(columnRefs, 0, newValue, 0, columnRefs.length);
                newValue[columnRefs.length] = columnId;
            }

            setProperty(transaction, Constraint.TABLE_REFERENCE_REFERENCES, (Object[])newValue);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
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
    public Column[] getReferencesColumns( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("foreignkeyimpl-getReferencesColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final Repository repository = getRepository();
        Column[] result = null;

        try {
            final Property property = getProperty(transaction, Constraint.TABLE_REFERENCE_REFERENCES);

            if (property == null) {
                result = new Column[0];
            } else {
                final String[] columnRefs = property.getStringValues();
                result = new Column[columnRefs.length];
                int i = 0;

                for (final String columnId : columnRefs) {
                    final KomodoObject kobject = repository.getUsingId(transaction, columnId);

                    if (kobject == null) {
                        throw new KException(Messages.getString(Relational.REFERENCED_COLUMN_NOT_FOUND, columnId));
                    }

                    result[i] = new ColumnImpl(repository, kobject.getAbsolutePath());
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
     * @see org.komodo.relational.model.ForeignKey#getReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getReferencesTable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("foreignkeyimpl-getReferencesTable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            Table result = null;
            final Property property = getProperty(transaction, Constraint.TABLE_REFERENCE);

            if (property != null) {
                final String tableId = property.getStringValue();
                final KomodoObject kobject = getRepository().getUsingId(transaction, tableId);

                if (kobject == null) {
                    throw new KException(Messages.getString(Relational.REFERENCED_TABLE_NOT_FOUND, tableId));
                }

                result = new TableImpl(getRepository(), kobject.getAbsolutePath());
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
     * @see org.komodo.relational.model.ForeignKey#removeReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void removeReferencesColumn( final UnitOfWork uow,
                                        final Column removeReferencesColumn ) throws KException {
        ArgCheck.isNotNull(removeReferencesColumn, "removeReferencesColumn"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-removeReferencesColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeRefencesColumn: transaction = '{0}', removeReferencesColumn = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         removeReferencesColumn);
        }

        assert (removeReferencesColumn != null);

        try {
            final String columnId = removeReferencesColumn.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue();
            final Column[] current = getReferencesColumns(transaction);

            if (current.length == 0) {
                throw new KException(Messages.getString(Relational.REFERENCED_COLUMN_NOT_FOUND, columnId));
            }

            boolean found = false;
            final Column[] updated = new Column[current.length - 1];
            int i = 0;

            for (final Column column : current) {
                if (column.equals(removeReferencesColumn)) {
                    found = true;
                } else {
                    updated[i] = column;
                    ++i;
                }
            }

            if (found) {
                setProperty(transaction, Constraint.TABLE_REFERENCE_REFERENCES, (Object[])updated);

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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#setReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Table)
     */
    @Override
    public void setReferencesTable( final UnitOfWork uow,
                                    final Table newReferencesTable ) throws KException {
        ArgCheck.isNotNull(newReferencesTable, "newReferencesTable"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("foreignkeyimpl-setReferencesTable", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setReferencesTable: transaction = '{0}', newReferencesTable = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newReferencesTable);
        }

        try {
            String tableId = null;

            if (newReferencesTable != null) {
                tableId = newReferencesTable.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue();
            }

            setProperty(transaction, Constraint.TABLE_REFERENCE, tableId);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
