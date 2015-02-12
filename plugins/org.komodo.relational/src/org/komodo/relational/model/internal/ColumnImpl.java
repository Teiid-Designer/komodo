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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model column.
 */
public final class ColumnImpl extends RelationalObjectImpl implements Column {

    private enum StandardOptions {
        ANNOTATION,
        CASE_SENSITIVE,
        CHAR_OCTET_LENGTH,
        CURRENCY,
        DISTINCT_VALUES,
        FIXED_LENGTH,
        MAX_VALUE,
        MIN_VALUE,
        NAMEINSOURCE,
        NATIVE_TYPE,
        NULL_VALUE_COUNT,
        RADIX,
        SEARCHABLE,
        SELECTABLE,
        SIGNED,
        UPDATABLE,
        UUID
    }

    /**
     * The resolver of a {@link Column}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, CreateTable.TABLE_ELEMENT);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Column resolve( final UnitOfWork transaction,
                               final Repository repository,
                               final KomodoObject kobject ) throws KException {
            return new ColumnImpl(transaction, repository, kobject.getAbsolutePath());
        }

    };

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
     * @see org.komodo.relational.model.Column#getCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getCharOctetLength( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CHAR_OCTET_LENGTH.toString());

        if (option == null) {
            return Column.DEFAULT_CHAR_OCTET_LENGTH;
        }

        return Integer.parseInt(option.getOption(transaction));
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
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getCustomOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption[] options = getStatementOptions(transaction);

        if (options.length == 0) {
            return options;
        }

        final List< StatementOption > result = new ArrayList<>(options.length);

        for (final StatementOption option : options) {
            if (StandardOptions.valueOf(option.getName(transaction)) == null) {
                result.add(option);
            }
        }

        return result.toArray(new StatementOption[result.size()]);
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
     * @see org.komodo.relational.model.Column#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ANNOTATION.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDistinctValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getDistinctValues( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.DISTINCT_VALUES.toString());

        if (option == null) {
            return Column.DEFAULT_DISTINCT_VALUES;
        }

        return Long.parseLong(option.getOption(transaction));
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
     * @see org.komodo.relational.model.Column#getMaxValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMaxValue( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MAX_VALUE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getMinValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMinValue( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MIN_VALUE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NAMEINSOURCE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNativeType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNativeType( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NATIVE_TYPE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
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
     * @see org.komodo.relational.model.Column#getNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getNullValueCount( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NULL_VALUE_COUNT.toString());

        if (option == null) {
            return Column.DEFAULT_NULL_VALUE_COUNT;
        }

        return Long.parseLong(option.getOption(transaction));
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
     * @see org.komodo.relational.model.Column#getRadix(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getRadix( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.RADIX.toString());

        if (option == null) {
            return Column.DEFAULT_RADIX;
        }

        return Integer.parseInt(option.getOption(transaction));
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
     * @see org.komodo.relational.model.Column#getSearchable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Searchable getSearchable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SEARCHABLE.toString());

        if (option == null) {
            return Searchable.DEFAULT_VALUE;
        }

        return Searchable.valueOf(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-getStatementOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getStatementOptions: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< StatementOption > result = new ArrayList< StatementOption >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
                final StatementOption option = new StatementOptionImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getStatementOptions: transaction = {0}, found statement option = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(option);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return StatementOption.NO_OPTIONS;
            }

            return result.toArray(new StatementOption[result.size()]);
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
        final Boolean value = getObjectProperty(uow, Property.ValueType.BOOLEAN, "isAutoIncremented", //$NON-NLS-1$
                                                CreateTable.AUTO_INCREMENT);

        if (value == null) {
            return Column.DEFAULT_AUTO_INCREMENTED;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isCaseSensitive(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isCaseSensitive( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isCaseSensitive", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CASE_SENSITIVE.toString());

        if (option == null) {
            return Column.DEFAULT_CASE_SENSITIVE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isCurrency(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isCurrency( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isCurrency", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CURRENCY.toString());

        if (option == null) {
            return Column.DEFAULT_CURRENCY;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isFixedLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isFixedLength( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isFixedLength", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.FIXED_LENGTH.toString());

        if (option == null) {
            return Column.DEFAULT_FIXED_LENGTH;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSelectable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSelectable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isSelectable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SELECTABLE.toString());

        if (option == null) {
            return Column.DEFAULT_SELECTABLE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSigned(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSigned( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isSigned", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SIGNED.toString());

        if (option == null) {
            return Column.DEFAULT_SIGNED;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isUpdatable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUpdatable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-isUpdatable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.UPDATABLE.toString());

        if (option == null) {
            return Column.DEFAULT_UPDATABLE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork uow,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotEmpty(optionToRemove, "optionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-removeStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeStatementOption: transaction = {0}, optionToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         optionToRemove);
        }

        boolean found = false;

        try {
            final StatementOption[] options = getStatementOptions(transaction);

            if (options.length != 0) {
                for (final StatementOption option : options) {
                    if (optionToRemove.equals(option.getName(transaction))) {
                        removeChild(transaction, optionToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove));
            }

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
     * @see org.komodo.relational.model.Column#setCaseSensitive(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCaseSensitive( final UnitOfWork transaction,
                                  final boolean newCaseSensitive ) throws KException {
        setStatementOption(transaction, StandardOptions.CASE_SENSITIVE.toString(), Boolean.toString(newCaseSensitive));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setCharOctetLength( final UnitOfWork transaction,
                                    final int newCharOctetLength ) throws KException {
        setStatementOption(transaction, StandardOptions.CHAR_OCTET_LENGTH.toString(), Integer.toString(newCharOctetLength));
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
     * @see org.komodo.relational.model.Column#setCurrency(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCurrency( final UnitOfWork transaction,
                             final boolean newCurrency ) throws KException {
        setStatementOption(transaction, StandardOptions.CURRENCY.toString(), Boolean.toString(newCurrency));
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
     * @see org.komodo.relational.model.Column#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setStatementOption(transaction, StandardOptions.ANNOTATION.toString(), newDescription);
    }

    @Override
    public void setDistinctValues( final UnitOfWork transaction,
                                   final long newDistinctValues ) throws KException {
        setStatementOption(transaction, StandardOptions.DISTINCT_VALUES.toString(), Long.toString(newDistinctValues));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setFixedLength(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setFixedLength( final UnitOfWork transaction,
                                final boolean newFixedLength ) throws KException {
        setStatementOption(transaction, StandardOptions.FIXED_LENGTH.toString(), Boolean.toString(newFixedLength));
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
     * @see org.komodo.relational.model.Column#setMaxValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMaxValue( final UnitOfWork transaction,
                             final String newMaxValue ) throws KException {
        setStatementOption(transaction, StandardOptions.MAX_VALUE.toString(), newMaxValue);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMinValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMinValue( final UnitOfWork transaction,
                             final String newMinValue ) throws KException {
        setStatementOption(transaction, StandardOptions.MIN_VALUE.toString(), newMinValue);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption(transaction, StandardOptions.NAMEINSOURCE.toString(), newNameInSource);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNativeType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNativeType( final UnitOfWork transaction,
                               final String newNativeType ) throws KException {
        setStatementOption(transaction, StandardOptions.NATIVE_TYPE.toString(), newNativeType);
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
     * @see org.komodo.relational.model.Column#setNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setNullValueCount( final UnitOfWork transaction,
                                   final long newNullValueCount ) throws KException {
        setStatementOption(transaction, StandardOptions.NULL_VALUE_COUNT.toString(), Long.toString(newNullValueCount));
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
     * @see org.komodo.relational.model.Column#setRadix(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setRadix( final UnitOfWork transaction,
                          final int newRadix ) throws KException {
        setStatementOption(transaction, StandardOptions.RADIX.toString(), Integer.toString(newRadix));
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
     * @see org.komodo.relational.model.Column#setSearchable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column.Searchable)
     */
    @Override
    public void setSearchable( final UnitOfWork transaction,
                               final Searchable newSearchable ) throws KException {
        final String value = ((newSearchable == null) ? Searchable.DEFAULT_VALUE.toString() : newSearchable.toString());
        setStatementOption(transaction, StandardOptions.SEARCHABLE.toString(), value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSelectable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSelectable( final UnitOfWork transaction,
                               final boolean newSelectable ) throws KException {
        setStatementOption(transaction, StandardOptions.SELECTABLE.toString(), Boolean.toString(newSelectable));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSigned(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSigned( final UnitOfWork transaction,
                           final boolean newSigned ) throws KException {
        setStatementOption(transaction, StandardOptions.SIGNED.toString(), Boolean.toString(newSigned));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#setStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption setStatementOption( final UnitOfWork uow,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("columnimpl-setStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setStatementOption: transaction = {0}, optionName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         optionName);
        }

        try {
            StatementOption result = null;

            if (StringUtils.isBlank(optionValue)) {
                removeStatementOption(transaction, optionName);
            } else {
                result = Utils.getOption(transaction, this, optionName);

                if (result == null) {
                    result = RelationalModelFactory.createStatementOption(transaction,
                                                                          getRepository(),
                                                                          this,
                                                                          optionName,
                                                                          optionValue);
                } else {
                    result.setOption(transaction, optionValue);
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
     * @see org.komodo.relational.model.Column#setUpdatable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUpdatable( final UnitOfWork transaction,
                              final boolean newUpdatable ) throws KException {
        setStatementOption(transaction, StandardOptions.UPDATABLE.toString(), Boolean.toString(newUpdatable));
    }

}
