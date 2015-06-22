/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * An implementation of a relational model DDL statement option.
 */
public final class StatementOptionImpl extends RelationalChildRestrictedObject implements StatementOption {

    static class OptionDescriptor implements PropertyDescriptor {

        private final String name;

        OptionDescriptor( final String optionName ) {
            this.name = optionName;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getDefaultValues()
         */
        @Override
        public Object[] getDefaultValues() {
            return StringConstants.EMPTY_ARRAY;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getName()
         */
        @Override
        public String getName() {
            return this.name;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#getType()
         */
        @Override
        public Type getType() {
            return Type.STRING;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isMandatory()
         */
        @Override
        public boolean isMandatory() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isModifiable()
         */
        @Override
        public boolean isModifiable() {
            return true;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.PropertyDescriptor#isMultiple()
         */
        @Override
        public boolean isMultiple() {
            return false;
        }

    }

    /**
     * The resolver of a {@link StatementOption}.
     */
    public static final TypeResolver< StatementOption > RESOLVER = new TypeResolver< StatementOption >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public StatementOption create( final UnitOfWork transaction,
                                       final Repository repository,
                                       final KomodoObject parent,
                                       final String id,
                                       final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Object optionValueValue = properties.getValue( StandardDdlLexicon.VALUE );
            final String optionValue = optionValueValue == null ? null : optionValueValue.toString();
            final OptionContainer parentContainer = adapter.adapt( transaction, parent, OptionContainer.class );

            if ( parentContainer == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          StatementOption.class.getSimpleName() ) );
            }

            return parentContainer.setStatementOption( transaction, id, optionValue );
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
        public Class< StatementOptionImpl > owningClass() {
            return StatementOptionImpl.class;
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
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            StandardDdlLexicon.TYPE_STATEMENT_OPTION );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StatementOption resolve( final UnitOfWork transaction,
                                        final KomodoObject kobject ) throws KException {
            return new StatementOptionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    private PropertyDescriptor descriptor;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a statement option
     */
    public StatementOptionImpl( final UnitOfWork transaction,
                                final Repository repository,
                                final String workspacePath ) throws KException {
        super( transaction, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBooleanValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean getBooleanValue( final UnitOfWork transaction ) throws KException {
        final String value = getOption( transaction );
        return Boolean.parseBoolean( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBooleanValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public boolean[] getBooleanValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar getDateValue( final UnitOfWork transaction ) throws KException {
        final String value = getOption( transaction );

        try {
            final Date date = DateFormat.getInstance().parse( value );
            final Calendar result = Calendar.getInstance();
            result.setTime( date );
            return result;
        } catch ( final Exception e ) {
            throw new KException( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public Calendar[] getDateValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public BigDecimal getDecimalValue( final UnitOfWork transaction ) throws KException {
        final String value = getOption( transaction );
        return new BigDecimal( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public BigDecimal[] getDecimalValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyDescriptor getDescriptor( final UnitOfWork transaction ) throws KException {
        if ( this.descriptor == null ) {
            this.descriptor = new OptionDescriptor( this.getName( transaction ) );
        }

        return this.descriptor;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public double getDoubleValue( final UnitOfWork transaction ) throws KException {
        final String value = getOption( transaction );
        return Double.parseDouble( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public double[] getDoubleValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLongValue( final UnitOfWork transaction ) throws KException {
        final String value = getOption( transaction );
        return Long.parseLong( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public long[] getLongValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.StatementOption#getOption(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOption( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getOption", StandardDdlLexicon.VALUE ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getStringValue( final UnitOfWork transaction ) throws KException {
        return getOption( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public String[] getStringValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork transaction ) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getValue( final UnitOfWork transaction ) throws KException {
        return getOption( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValues(org.komodo.spi.repository.Repository.UnitOfWork)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public Object[] getValues( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
    }

    /**
     * {@inheritDoc}
     * <p>
     * Always returns {@link PropertyValueType#STRING}
     *
     * @see org.komodo.spi.repository.Property#getValueType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyValueType getValueType( final UnitOfWork transaction ) {
        return PropertyValueType.STRING;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Always returns <code>false</code>
     *
     * @see org.komodo.spi.repository.Property#isMultiple(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isMultiple( final UnitOfWork transaction ) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#set(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.Object[])
     */
    @Override
    public void set( final UnitOfWork transaction,
                     final Object... values ) throws KException {
        if ( ( values == null ) || ( values.length == 0 ) ) {
            setOption( transaction, null );
        } else if ( values.length == 1 ) {
            setOption( transaction, values[0].toString() );
        } else {
            throw new UnsupportedOperationException( Messages.getString( Messages.Relational.INVALID_STATEMENT_OPTION_VALUE ) );
        }

    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.StatementOption#setOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setOption( final UnitOfWork transaction,
                           final String newOption ) throws KException {
        ArgCheck.isNotEmpty( newOption, "newOption" ); //$NON-NLS-1$
        setObjectProperty( transaction, "setOption", StandardDdlLexicon.VALUE, newOption ); //$NON-NLS-1$

        if ( StringUtils.isBlank( newOption ) ) {
            remove( transaction );
        }
    }

}
