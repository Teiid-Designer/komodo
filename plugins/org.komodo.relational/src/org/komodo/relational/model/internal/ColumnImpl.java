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
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model column.
 */
public final class ColumnImpl extends RelationalChildRestrictedObject implements Column {

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
    public static final TypeResolver< Column > RESOLVER = new TypeResolver< Column >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Column create( UnitOfWork transaction,
                              Repository repository,
                              KomodoObject parent,
                              String id,
                              RelationalProperties properties ) throws KException {
            AdapterFactory adapter = new AdapterFactory( parent.getRepository() );
            Table parentTable = adapter.adapt( transaction, parent, Table.class );
            return RelationalModelFactory.createColumn( transaction, parent.getRepository(), parentTable, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< ColumnImpl > owningClass() {
            return ColumnImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateTable.TABLE_ELEMENT );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Column resolve( final UnitOfWork transaction,
                               final KomodoObject kobject ) throws KException {
            return new ColumnImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getCharOctetLength( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CHAR_OCTET_LENGTH.name());

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
        return getObjectProperty(uow, PropertyValueType.STRING, "getCollationName", //$NON-NLS-1$
                                 StandardDdlLexicon.COLLATION_NAME);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        StatementOption[] result = getStatementOptions( transaction );

        if ( result.length != 0 ) {
            final List< StatementOption > temp = new ArrayList<>( result.length );

            for ( final StatementOption option : result ) {
                if ( StandardOptions.valueOf( option.getName( transaction ) ) == null ) {
                    temp.add( option );
                }
            }

            result = temp.toArray( new StatementOption[ temp.size() ] );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, PropertyValueType.STRING, "getDatatypeName", //$NON-NLS-1$
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
        return getObjectProperty(uow, PropertyValueType.STRING, "getDefaultValue", //$NON-NLS-1$
                                 StandardDdlLexicon.DEFAULT_VALUE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ANNOTATION.name());

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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.DISTINCT_VALUES.name());

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
        final Long value = getObjectProperty(uow, PropertyValueType.LONG, "getLength", //$NON-NLS-1$
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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MAX_VALUE.name());

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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MIN_VALUE.name());

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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NAMEINSOURCE.name());

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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NATIVE_TYPE.name());

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
        final String value = getObjectProperty(uow, PropertyValueType.STRING, "getNullable", //$NON-NLS-1$
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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NULL_VALUE_COUNT.name());

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
        final Integer value = getObjectProperty(uow, PropertyValueType.INTEGER, "getPrecision", //$NON-NLS-1$
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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.RADIX.name());

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
        final Integer value = getObjectProperty(uow, PropertyValueType.INTEGER, "getScale", //$NON-NLS-1$
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
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SEARCHABLE.name());

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
    public StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject same = new ObjectImpl( getRepository(), getAbsolutePath(), getIndex() );
        final List< StatementOption > result = new ArrayList< StatementOption >();

        for ( final KomodoObject kobject : same.getChildrenOfType( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION ) ) {
            final StatementOption option = new StatementOptionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( option );
        }

        if ( result.isEmpty() ) {
            return StatementOption.NO_OPTIONS;
        }

        return result.toArray( new StatementOption[ result.size() ] );
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
     * @see org.komodo.relational.model.Column#getUuid(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getUuid( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption( transaction, this, StandardOptions.UUID.name() );

        if (option == null) {
            return null;
        }

        return option.getOption( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAutoIncremented( final UnitOfWork uow ) throws KException {
        final Boolean value = getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAutoIncremented", //$NON-NLS-1$
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
    public boolean isCaseSensitive( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CASE_SENSITIVE.name());

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
    public boolean isCurrency( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CURRENCY.name());

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
    public boolean isFixedLength( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.FIXED_LENGTH.name());

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
    public boolean isSelectable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SELECTABLE.name());

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
    public boolean isSigned( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.SIGNED.name());

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
    public boolean isUpdatable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.UPDATABLE.name());

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
    public void removeStatementOption( final UnitOfWork transaction,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionToRemove, "optionToRemove" ); //$NON-NLS-1$

        boolean found = false;
        final StatementOption[] options = getStatementOptions( transaction );

        if ( options.length != 0 ) {
            for ( final StatementOption option : options ) {
                if ( optionToRemove.equals( option.getName( transaction ) ) ) {
                    option.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove ) );
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
        setStatementOption(transaction, StandardOptions.CASE_SENSITIVE.name(), Boolean.toString(newCaseSensitive));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setCharOctetLength( final UnitOfWork transaction,
                                    final int newCharOctetLength ) throws KException {
        setStatementOption(transaction, StandardOptions.CHAR_OCTET_LENGTH.name(), Integer.toString(newCharOctetLength));
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
        setStatementOption(transaction, StandardOptions.CURRENCY.name(), Boolean.toString(newCurrency));
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
        setStatementOption(transaction, StandardOptions.ANNOTATION.name(), newDescription);
    }

    @Override
    public void setDistinctValues( final UnitOfWork transaction,
                                   final long newDistinctValues ) throws KException {
        setStatementOption(transaction, StandardOptions.DISTINCT_VALUES.name(), Long.toString(newDistinctValues));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setFixedLength(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setFixedLength( final UnitOfWork transaction,
                                final boolean newFixedLength ) throws KException {
        setStatementOption(transaction, StandardOptions.FIXED_LENGTH.name(), Boolean.toString(newFixedLength));
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
        setStatementOption(transaction, StandardOptions.MAX_VALUE.name(), newMaxValue);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMinValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMinValue( final UnitOfWork transaction,
                             final String newMinValue ) throws KException {
        setStatementOption(transaction, StandardOptions.MIN_VALUE.name(), newMinValue);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption(transaction, StandardOptions.NAMEINSOURCE.name(), newNameInSource);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNativeType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNativeType( final UnitOfWork transaction,
                               final String newNativeType ) throws KException {
        setStatementOption(transaction, StandardOptions.NATIVE_TYPE.name(), newNativeType);
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
                          (newNullable == null) ? Nullable.DEFAULT_VALUE.toValue() : newNullable.toValue());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setNullValueCount( final UnitOfWork transaction,
                                   final long newNullValueCount ) throws KException {
        setStatementOption(transaction, StandardOptions.NULL_VALUE_COUNT.name(), Long.toString(newNullValueCount));
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
        setStatementOption(transaction, StandardOptions.RADIX.name(), Integer.toString(newRadix));
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
        setStatementOption(transaction, StandardOptions.SEARCHABLE.name(), value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSelectable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSelectable( final UnitOfWork transaction,
                               final boolean newSelectable ) throws KException {
        setStatementOption(transaction, StandardOptions.SELECTABLE.name(), Boolean.toString(newSelectable));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSigned(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSigned( final UnitOfWork transaction,
                           final boolean newSigned ) throws KException {
        setStatementOption(transaction, StandardOptions.SIGNED.name(), Boolean.toString(newSigned));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#setStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption setStatementOption( final UnitOfWork transaction,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$

        StatementOption result = null;

        if ( StringUtils.isBlank( optionValue ) ) {
            removeStatementOption( transaction, optionName );
        } else {
            result = Utils.getOption( transaction, this, optionName );

            if ( result == null ) {
                result = RelationalModelFactory.createStatementOption( transaction,
                                                                       getRepository(),
                                                                       this,
                                                                       optionName,
                                                                       optionValue );
            } else {
                result.setOption( transaction, optionValue );
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUpdatable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUpdatable( final UnitOfWork transaction,
                              final boolean newUpdatable ) throws KException {
        setStatementOption(transaction, StandardOptions.UPDATABLE.name(), Boolean.toString(newUpdatable));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUuid(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setUuid( final UnitOfWork transaction,
                         final String newUuid ) throws KException {
        setStatementOption( transaction, StandardOptions.UUID.name(), newUuid );
    }

}
