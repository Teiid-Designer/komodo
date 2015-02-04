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
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
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
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a column
     */
    public ColumnImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCollationName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCollationName( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getCollationName", //$NON-NLS-1$
                                 StandardDdlLexicon.COLLATION_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getDatatypeName", //$NON-NLS-1$
                                               StandardDdlLexicon.DATATYPE_NAME);

        if (StringUtils.isBlank(value)) {
            return RelationalConstants.DEFAULT_DATATYPE_NAME;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDefaultValue( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getDefaultValue", //$NON-NLS-1$
                                 StandardDdlLexicon.DEFAULT_VALUE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty(uow, Property.ValueType.LONG, "getLength", //$NON-NLS-1$
                                             StandardDdlLexicon.DATATYPE_LENGTH);

        if (value == null) {
            return RelationalConstants.DEFAULT_LENGTH;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getNullable", //$NON-NLS-1$
                                               StandardDdlLexicon.NULLABLE);

        if (StringUtils.isBlank(value)) {
            return Nullable.DEFAULT_VALUE;
        }

        return Nullable.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = super.getParent(transaction);
            final Table result = new TableImpl(transaction, getRepository(), kobject.getAbsolutePath());

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
        final Integer value = getObjectProperty(uow, Property.ValueType.INTEGER, "getPrecision", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_PRECISION);

        if (value == null) {
            return RelationalConstants.DEFAULT_PRECISION;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getScale( final UnitOfWork uow ) throws KException {
        final Integer value = getObjectProperty(uow, Property.ValueType.INTEGER, "getScale", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_SCALE);

        if (value == null) {
            return RelationalConstants.DEFAULT_SCALE;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAutoIncremented( final UnitOfWork uow ) throws KException {
        final Boolean value = getObjectProperty(uow, Property.ValueType.BOOLEAN, "isAutoIncremented", //$NON-NLS-1$
                                                CreateTable.AUTO_INCREMENT);

        if (value == null) {
            return RelationalConstants.DEFAULT_AUTO_INCREMENTED;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAutoIncremented( final UnitOfWork uow,
                                    final boolean newAutoIncremented ) throws KException {
        setObjectProperty(uow, "setAutoIncremented", CreateTable.AUTO_INCREMENT, newAutoIncremented); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCollationName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setCollationName( final UnitOfWork uow,
                                  final String newCollationName ) throws KException {
        setObjectProperty(uow, "setCollationName", StandardDdlLexicon.COLLATION_NAME, newCollationName); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDatatypeName( final UnitOfWork uow,
                                 final String newTypeName ) throws KException {
        setObjectProperty(uow, "setDatatypeName", StandardDdlLexicon.DATATYPE_NAME, newTypeName); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDefaultValue( final UnitOfWork uow,
                                 final String newDefaultValue ) throws KException {
        setObjectProperty(uow, "setDefaultValue", StandardDdlLexicon.DEFAULT_VALUE, newDefaultValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        setObjectProperty(uow, "setLength", StandardDdlLexicon.DATATYPE_LENGTH, newLength); //$NON-NLS-1$
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
        setObjectProperty(uow, "setNullable", //$NON-NLS-1$
                          StandardDdlLexicon.NULLABLE,
                          (newNullable == null) ? Nullable.DEFAULT_VALUE.toString() : newNullable.toString());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setPrecision( final UnitOfWork uow,
                              final int newPrecision ) throws KException {
        setObjectProperty(uow, "setPrecision", StandardDdlLexicon.DATATYPE_PRECISION, newPrecision); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setScale(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setScale( final UnitOfWork uow,
                          final int newScale ) throws KException {
        setObjectProperty(uow, "setScale", StandardDdlLexicon.DATATYPE_SCALE, newScale); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#validateInitialState(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    protected void validateInitialState( final UnitOfWork uow,
                                         final String path ) throws KException {
        validateType(uow, path, CreateTable.TABLE_ELEMENT);
    }

}
