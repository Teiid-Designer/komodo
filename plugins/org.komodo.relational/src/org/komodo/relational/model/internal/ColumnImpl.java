/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Column;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model column.
 */
public final class ColumnImpl extends RelationalObjectImpl implements Column {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ColumnImpl( final Repository repository,
                       final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCollationName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCollationName( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = RelationalConstants.DEFAULT_COLLATION_NAME;
            final Property property = getProperty(transaction, StandardDdlLexicon.COLLATION_NAME);

            if (property != null) {
                result = property.getStringValue();
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
     * @see org.komodo.relational.model.Column#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getDatatypeName", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = RelationalConstants.DEFAULT_DATATYPE_NAME;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_NAME);

            if (property != null) {
                result = property.getStringValue();
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
     * @see org.komodo.relational.model.Column#getDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDefaultValue( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getDefaultValue", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = RelationalConstants.DEFAULT_VALUE;
            final Property property = getProperty(transaction, StandardDdlLexicon.DEFAULT_VALUE);

            if (property != null) {
                result = property.getStringValue();
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
     * @see org.komodo.relational.model.Column#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getLength", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            long result = RelationalConstants.DEFAULT_LENGTH;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_LENGTH);

            if (property != null) {
                result = property.getLongValue();
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
     * @see org.komodo.relational.model.Column#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getNullable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            Nullable result = Nullable.DEFAULT_VALUE;
            final Property property = getProperty(transaction, StandardDdlLexicon.NULLABLE);

            if (property != null) {
                final String value = property.getStringValue();
                result = Nullable.fromValue(value);
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
     * @see org.komodo.relational.model.Column#getPrecision(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getPrecision( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getPrecision", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = RelationalConstants.DEFAULT_PRECISION;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_PRECISION);

            if (property != null) {
                result = (int)property.getLongValue();
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
     * @see org.komodo.relational.model.Column#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getScale( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getScale", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = RelationalConstants.DEFAULT_SCALE;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_SCALE);

            if (property != null) {
                result = (int)property.getLongValue();
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
     * @see org.komodo.relational.model.Column#isAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAutoIncremented( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getScale", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = RelationalConstants.DEFAULT_AUTO_INCREMENTED;
            final Property property = getProperty(transaction, CreateTable.AUTO_INCREMENT);

            if (property != null) {
                result = property.getBooleanValue();
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
     * @see org.komodo.relational.model.Column#setAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAutoIncremented( final UnitOfWork uow,
                                    final boolean newAutoIncremented ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setAutoIncremented", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAutoIncremented: transaction = '{0}', newAutoIncremented = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAutoIncremented);
        }

        try {
            setProperty(transaction, CreateTable.AUTO_INCREMENT, newAutoIncremented);

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
     * @see org.komodo.relational.model.Column#setCollationName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setCollationName( final UnitOfWork uow,
                                  final String newCollationName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setCollationName", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setCollationName: transaction = '{0}', newCollationName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newCollationName);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.COLLATION_NAME, StringUtils.isBlank(newCollationName) ? null : newCollationName);

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
     * @see org.komodo.relational.model.Column#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDatatypeName( final UnitOfWork uow,
                                 final String newTypeName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setDatatypeName", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDatatypeName: transaction = '{0}', newTypeName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newTypeName);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_NAME, StringUtils.isBlank(newTypeName) ? null : newTypeName);

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
     * @see org.komodo.relational.model.Column#setDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDefaultValue( final UnitOfWork uow,
                                 final String newDefaultValue ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setDefaultValue", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDefaultValue: transaction = '{0}', newDefaultValue = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newDefaultValue);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DEFAULT_VALUE, StringUtils.isBlank(newDefaultValue) ? null : newDefaultValue);

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
     * @see org.komodo.relational.model.Column#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setLength", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setLength: transaction = '{0}', newLength = '{1}'", transaction.getName(), newLength); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_LENGTH, newLength);

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
     * @see org.komodo.relational.model.Column#setNullable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.Nullable)
     */
    @Override
    public void setNullable( final UnitOfWork uow,
                             final Nullable newNullable ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setNullable", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setNullable: transaction = '{0}', newNullable = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newNullable);
        }

        try {
            setProperty(transaction,
                        StandardDdlLexicon.NULLABLE,
                        (newNullable == null) ? Nullable.DEFAULT_VALUE : newNullable.toString());

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
     * @see org.komodo.relational.model.Column#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setPrecision( final UnitOfWork uow,
                              final int newPrecision ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setPrecision", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrecision: transaction = '{0}', newPrecision = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newPrecision);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_PRECISION, newPrecision);

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
     * @see org.komodo.relational.model.Column#setScale(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setScale( final UnitOfWork uow,
                          final int newScale ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setScale", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setScale: transaction = '{0}', newScale = '{1}'", transaction.getName(), newScale); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_SCALE, newScale);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
