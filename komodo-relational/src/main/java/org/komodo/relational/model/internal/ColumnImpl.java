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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model column.
 */
public final class ColumnImpl extends RelationalChildRestrictedObject implements Column {

    private enum StandardOption {

        ANNOTATION( null ),
        CASE_SENSITIVE( Boolean.toString( Column.DEFAULT_CASE_SENSITIVE ) ),
        CHAR_OCTET_LENGTH( Long.toString( Column.DEFAULT_CHAR_OCTET_LENGTH ) ),
        CURRENCY( Boolean.toString( Column.DEFAULT_CURRENCY ) ),
        DISTINCT_VALUES( Long.toString( Column.DEFAULT_DISTINCT_VALUES ) ),
        FIXED_LENGTH( Boolean.toString( Column.DEFAULT_FIXED_LENGTH ) ),
        MAX_VALUE( null ),
        MIN_VALUE( null ),
        NAMEINSOURCE( null ),
        NATIVE_TYPE( null ),
        NULL_VALUE_COUNT( Long.toString( Column.DEFAULT_NULL_VALUE_COUNT ) ),
        RADIX( Long.toString( Column.DEFAULT_RADIX ) ),
        SEARCHABLE( Column.Searchable.DEFAULT_VALUE.name() ),
        SELECTABLE( Boolean.toString( Column.DEFAULT_SELECTABLE ) ),
        SIGNED( Boolean.toString( Column.DEFAULT_SIGNED ) ),
        UPDATABLE( Boolean.toString( Column.DEFAULT_UPDATABLE ) ),
        UUID( null );

        private static Map< String, String > _defaultValues = null;

        /**
         * @return an unmodifiable collection of the names and default values of all the standard options (never <code>null</code>
         *         or empty)
         */
        static Map< String, String > defaultValues() {
            if ( _defaultValues == null ) {
                final StandardOption[] options = values();
                final Map< String, String > temp = new HashMap< >();

                for ( final StandardOption option : options ) {
                    temp.put( option.name(), option.defaultValue );
                }

                _defaultValues = Collections.unmodifiableMap( temp );
            }

            return _defaultValues;
        }

        /**
         * @param name
         *        the name being checked (can be <code>null</code>)
         * @return <code>true</code> if the name is the name of a standard option
         */
        static boolean isValid( final String name ) {
            for ( final StandardOption option : values() ) {
                if ( option.name().equals( name ) ) {
                    return true;
                }
            }

            return false;
        }

        private final String defaultValue;

        private StandardOption( final String defaultValue ) {
            this.defaultValue = defaultValue;
        }

    }

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
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getCharOctetLength( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.CHAR_OCTET_LENGTH.name() );

        if ( option == null ) {
            return Column.DEFAULT_CHAR_OCTET_LENGTH;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCollationName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCollationName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getCollationName", //$NON-NLS-1$
                                  StandardDdlLexicon.COLLATION_NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getCustomOptions( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getDatatypeName", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_NAME );

        if ( StringUtils.isBlank( value ) ) {
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
        return getObjectProperty( uow, PropertyValueType.STRING, "getDefaultValue", //$NON-NLS-1$
                                  StandardDdlLexicon.DEFAULT_VALUE );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.ANNOTATION.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDistinctValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getDistinctValues( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.DISTINCT_VALUES.name() );

        if ( option == null ) {
            return Column.DEFAULT_DISTINCT_VALUES;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty( uow, PropertyValueType.LONG, "getLength", //$NON-NLS-1$
                                              StandardDdlLexicon.DATATYPE_LENGTH );

        if ( value == null ) {
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
        return OptionContainerUtils.getOption( transaction, this, StandardOption.MAX_VALUE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getMinValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMinValue( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.MIN_VALUE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.NAMEINSOURCE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNativeType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNativeType( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.NATIVE_TYPE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getNullable", //$NON-NLS-1$
                                                StandardDdlLexicon.NULLABLE );

        if ( StringUtils.isBlank( value ) ) {
            return Nullable.DEFAULT_VALUE;
        }

        return Nullable.fromValue( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getNullValueCount( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.NULL_VALUE_COUNT.name() );

        if ( option == null ) {
            return Column.DEFAULT_NULL_VALUE_COUNT;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getPrecision(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getPrecision( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty( uow, PropertyValueType.LONG, "getPrecision", //$NON-NLS-1$
                                                 StandardDdlLexicon.DATATYPE_PRECISION );

        if ( value == null ) {
            return RelationalConstants.DEFAULT_PRECISION;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.createPrimaryType(transaction, this, super.getPrimaryType( transaction ));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork transaction,
                                 final String name ) throws KException {
        return OptionContainerUtils.getProperty( transaction, this, name, super.getProperty( transaction, name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        return OptionContainerUtils.getPropertyDescriptor( transaction,
                                                           this,
                                                           propName,
                                                           super.getPropertyDescriptor( transaction, propName ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getPropertyNames( transaction, this, super.getPropertyNames( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getRadix(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getRadix( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.RADIX.name() );

        if ( option == null ) {
            return Column.DEFAULT_RADIX;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getScale( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty( uow, PropertyValueType.LONG, "getScale", //$NON-NLS-1$
                                                 StandardDdlLexicon.DATATYPE_SCALE );

        if ( value == null ) {
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
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.SEARCHABLE.name() );

        if ( option == null ) {
            return Searchable.DEFAULT_VALUE;
        }

        return Searchable.valueOf( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStandardOptions()
     */
    @Override
    public Map< String, String > getStandardOptions() {
        return StandardOption.defaultValues();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptionNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getStatementOptionNames( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOptionNames( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOptions( transaction, this );
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
        return Column.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getUuid(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getUuid( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.UUID.name() );
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
     * @see org.komodo.repository.ObjectImpl#hasProperties(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasProperties( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.hasProperties( transaction, this, super.hasProperties( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasProperty( final UnitOfWork transaction,
                                final String name ) throws KException {
        return OptionContainerUtils.hasProperty( transaction, this, name, super.hasProperty( transaction, name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAutoIncremented( final UnitOfWork uow ) throws KException {
        final Boolean value = getObjectProperty( uow, PropertyValueType.BOOLEAN, "isAutoIncremented", //$NON-NLS-1$
                                                 CreateTable.AUTO_INCREMENT );

        if ( value == null ) {
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
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.CASE_SENSITIVE.name() );

        if ( option == null ) {
            return Column.DEFAULT_CASE_SENSITIVE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isCurrency(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isCurrency( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.CURRENCY.name() );

        if ( option == null ) {
            return Column.DEFAULT_CURRENCY;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isCustomOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean isCustomOption( final UnitOfWork transaction,
                                   final String name ) throws KException {
        return OptionContainerUtils.hasCustomOption( transaction, this, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isFixedLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isFixedLength( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.FIXED_LENGTH.name() );

        if ( option == null ) {
            return Column.DEFAULT_FIXED_LENGTH;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSelectable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSelectable( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.SELECTABLE.name() );

        if ( option == null ) {
            return Column.DEFAULT_SELECTABLE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSigned(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSigned( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.SIGNED.name() );

        if ( option == null ) {
            return Column.DEFAULT_SIGNED;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isStandardOption(java.lang.String)
     */
    @Override
    public boolean isStandardOption( final String name ) {
        return StandardOption.isValid( name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isUpdatable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUpdatable( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.UPDATABLE.name() );

        if ( option == null ) {
            return Column.DEFAULT_UPDATABLE;
        }

        return Boolean.parseBoolean( option );
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
        OptionContainerUtils.removeOption( transaction, this, optionToRemove );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAutoIncremented( final UnitOfWork uow,
                                    final boolean newAutoIncremented ) throws KException {
        setObjectProperty( uow, "setAutoIncremented", CreateTable.AUTO_INCREMENT, newAutoIncremented ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCaseSensitive(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCaseSensitive( final UnitOfWork transaction,
                                  final boolean newCaseSensitive ) throws KException {
        setStatementOption( transaction, StandardOption.CASE_SENSITIVE.name(), Boolean.toString( newCaseSensitive ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setCharOctetLength( final UnitOfWork transaction,
                                    final long newCharOctetLength ) throws KException {
        setStatementOption( transaction, StandardOption.CHAR_OCTET_LENGTH.name(), Long.toString( newCharOctetLength ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCollationName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setCollationName( final UnitOfWork uow,
                                  final String newCollationName ) throws KException {
        setObjectProperty( uow, "setCollationName", StandardDdlLexicon.COLLATION_NAME, newCollationName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCurrency(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCurrency( final UnitOfWork transaction,
                             final boolean newCurrency ) throws KException {
        setStatementOption( transaction, StandardOption.CURRENCY.name(), Boolean.toString( newCurrency ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDatatypeName( final UnitOfWork uow,
                                 final String newTypeName ) throws KException {
        setObjectProperty( uow, "setDatatypeName", StandardDdlLexicon.DATATYPE_NAME, newTypeName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDefaultValue( final UnitOfWork uow,
                                 final String newDefaultValue ) throws KException {
        setObjectProperty( uow, "setDefaultValue", StandardDdlLexicon.DEFAULT_VALUE, newDefaultValue ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setStatementOption( transaction, StandardOption.ANNOTATION.name(), newDescription );
    }

    @Override
    public void setDistinctValues( final UnitOfWork transaction,
                                   final long newDistinctValues ) throws KException {
        setStatementOption( transaction, StandardOption.DISTINCT_VALUES.name(), Long.toString( newDistinctValues ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setFixedLength(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setFixedLength( final UnitOfWork transaction,
                                final boolean newFixedLength ) throws KException {
        setStatementOption( transaction, StandardOption.FIXED_LENGTH.name(), Boolean.toString( newFixedLength ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        setObjectProperty( uow, "setLength", StandardDdlLexicon.DATATYPE_LENGTH, newLength ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMaxValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMaxValue( final UnitOfWork transaction,
                             final String newMaxValue ) throws KException {
        setStatementOption( transaction, StandardOption.MAX_VALUE.name(), newMaxValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMinValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMinValue( final UnitOfWork transaction,
                             final String newMinValue ) throws KException {
        setStatementOption( transaction, StandardOption.MIN_VALUE.name(), newMinValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption( transaction, StandardOption.NAMEINSOURCE.name(), newNameInSource );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNativeType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNativeType( final UnitOfWork transaction,
                               final String newNativeType ) throws KException {
        setStatementOption( transaction, StandardOption.NATIVE_TYPE.name(), newNativeType );
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
        setObjectProperty( uow, "setNullable", //$NON-NLS-1$
                           StandardDdlLexicon.NULLABLE,
                           ( newNullable == null ) ? Nullable.DEFAULT_VALUE.toValue() : newNullable.toValue() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setNullValueCount( final UnitOfWork transaction,
                                   final long newNullValueCount ) throws KException {
        setStatementOption( transaction, StandardOption.NULL_VALUE_COUNT.name(), Long.toString( newNullValueCount ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setPrecision( final UnitOfWork uow,
                              final long newPrecision ) throws KException {
        setObjectProperty( uow, "setPrecision", StandardDdlLexicon.DATATYPE_PRECISION, newPrecision ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork transaction,
                             final String propertyName,
                             final Object... values ) throws KException {
        // if an option was not set then set a property
        if ( !OptionContainerUtils.setProperty( transaction, this, propertyName, values ) ) {
            super.setProperty( transaction, propertyName, values );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setRadix(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setRadix( final UnitOfWork transaction,
                          final long newRadix ) throws KException {
        setStatementOption( transaction, StandardOption.RADIX.name(), Long.toString( newRadix ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setScale(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setScale( final UnitOfWork uow,
                          final long newScale ) throws KException {
        setObjectProperty( uow, "setScale", StandardDdlLexicon.DATATYPE_SCALE, newScale ); //$NON-NLS-1$
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
        final String value = ( ( newSearchable == null ) ? Searchable.DEFAULT_VALUE.toString() : newSearchable.toString() );
        setStatementOption( transaction, StandardOption.SEARCHABLE.name(), value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSelectable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSelectable( final UnitOfWork transaction,
                               final boolean newSelectable ) throws KException {
        setStatementOption( transaction, StandardOption.SELECTABLE.name(), Boolean.toString( newSelectable ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSigned(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSigned( final UnitOfWork transaction,
                           final boolean newSigned ) throws KException {
        setStatementOption( transaction, StandardOption.SIGNED.name(), Boolean.toString( newSigned ) );
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
        return OptionContainerUtils.setOption( transaction, this, optionName, optionValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUpdatable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUpdatable( final UnitOfWork transaction,
                              final boolean newUpdatable ) throws KException {
        setStatementOption( transaction, StandardOption.UPDATABLE.name(), Boolean.toString( newUpdatable ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUuid(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setUuid( final UnitOfWork transaction,
                         final String newUuid ) throws KException {
        setStatementOption( transaction, StandardOption.UUID.name(), newUuid );
    }

}
