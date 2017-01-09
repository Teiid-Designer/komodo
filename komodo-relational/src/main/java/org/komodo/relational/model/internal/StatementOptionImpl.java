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

import java.io.InputStream;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import org.komodo.relational.Messages;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * An implementation of a relational model DDL statement option.
 */
public final class StatementOptionImpl extends RelationalChildRestrictedObject implements StatementOption {

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

    @Override
    public InputStream getBinaryValue(UnitOfWork uow) throws KException {
        throw new UnsupportedOperationException();
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
            // find descriptor in the primary type
            final Descriptor primaryType = getParent( transaction ).getPrimaryType( transaction );
            final String name = getName( transaction );

            for ( final PropertyDescriptor descriptor : primaryType.getPropertyDescriptors( transaction ) ) {
                if ( name.equals( descriptor.getName() )) {
                    this.descriptor = descriptor;
                    break;
                }
            }
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
        return StatementOption.IDENTIFIER;
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
