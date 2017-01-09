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
package org.komodo.repository;

import java.io.InputStream;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.jcr.Binary;
import javax.jcr.Node;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import org.komodo.repository.Messages.Komodo;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * The base class for a {@link Property Komodo object property}.
 */
public class PropertyImpl implements Property {

    /**
     * An empty array of values.
     */
    public static final Value[] NO_VALUES = new Value[0];

    /**
     * @param value
     *        the JCR value holder (cannot be <code>null</code>)
     * @param propertyType
     *        the required type of the property
     * @return the <code>Object</code> representation of the JCR value (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Object convert( final Value value,
                                  final int propertyType ) throws KException {
        try {
            switch (propertyType) {
                case PropertyType.BOOLEAN:
                    return value.getBoolean();
                case PropertyType.LONG:
                    return value.getLong();
                case PropertyType.DOUBLE:
                    return value.getDouble();
                case PropertyType.DATE:
                    return value.getDate();
                case PropertyType.DECIMAL:
                    return value.getDecimal();
                default:
                    return value.getString();
            }
        } catch (final Exception e) {
            throw new KException(Messages.getString(Komodo.UNABLE_TO_CONVERT_VALUE, propertyType), e);
        }
    }

    /**
     * Values will be converted to a string if a conversion cannot be done. Only boolean, long, decimal, double, and string are
     * supported. A string conversion is done for all unsupported types.
     *
     * @param value
     *        the value (cannot be <code>null</code>)
     * @param propertyType
     *        the required type of the property
     * @return the value converted to the required type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Object convert( final Object value,
                                  final int propertyType ) throws KException {
        if ( value instanceof Value ) {
            return convert( ( Value )value, propertyType );
        }

        try {
            switch ( propertyType ) {
                case PropertyType.BOOLEAN:
                    return Boolean.parseBoolean( value.toString() );
                case PropertyType.LONG:
                    return Long.parseLong( value.toString() );
                case PropertyType.DOUBLE:
                case PropertyType.DECIMAL:
                    return Double.parseDouble( value.toString() );
                default:
                    return value.toString();
            }
        } catch ( final Exception e ) {
            throw new KException( Messages.getString( Komodo.UNABLE_TO_CONVERT_VALUE, propertyType ), e );
        }
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param value
     *        the value being converted to a JCR value holder (can be <code>null</code>)
     * @return the JCR value holder (can be <code>null</code> if input is <code>null</code>)
     * @throws Exception if an error occurs
     */
    public static Value createValue( final ValueFactory factory,
                                     final Object value ) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$

        if (value == null ) {
            return null;
        }

        if (value instanceof Value) {
            return (Value)value;
        }

        if (value instanceof Boolean) {
            return factory.createValue(Boolean.class.cast(value));
        }

        if (value instanceof Long) {
            return factory.createValue(Long.class.cast(value));
        }

        if (value instanceof Double) {
            return factory.createValue(Double.class.cast(value));
        }

        if (value instanceof Calendar) {
            return factory.createValue(Calendar.class.cast(value));
        }

        if (value instanceof BigDecimal) {
            return factory.createValue(BigDecimal.class.cast(value));
        }

        if (value instanceof InputStream) {
            Binary binary = factory.createBinary((InputStream) value);
            return factory.createValue(binary);
        }

        return factory.createValue(value.toString());
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param value
     *        the value being converted to a JCR value holder (cannot be <code>null</code>)
     * @param jcrPropType
     *        the JCR {@link PropertyType property type}
     * @return the JCR value holder (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Value createValue( final ValueFactory factory,
                                     final Object value,
                                     final int jcrPropType ) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(value, "value"); //$NON-NLS-1$

        if (PropertyType.UNDEFINED == jcrPropType) {
            return createValue(factory, value);
        }

        if (PropertyType.BINARY == jcrPropType) {
            if (value instanceof InputStream) {
                InputStream stream = (InputStream) value;
                Binary binary = factory.createBinary(stream);
                return factory.createValue(binary);
            }

            throw new Exception("A Binary property value must be in the form of an InputStream");
        }

        if (PropertyType.BOOLEAN == jcrPropType) {
            if (value instanceof Boolean) {
                return factory.createValue((Boolean)value);
            }

            return factory.createValue(Boolean.parseBoolean(value.toString()));
        }

        if (PropertyType.LONG == jcrPropType) {
            if (value instanceof Long) {
                return factory.createValue((Long)value);
            }

            return factory.createValue(Long.parseLong(value.toString()));
        }

        if (PropertyType.DOUBLE == jcrPropType) {
            if (value instanceof Double) {
                return factory.createValue((Double)value);
            }

            return factory.createValue(Double.parseDouble(value.toString()));
        }

        if (PropertyType.DATE == jcrPropType) {
            if (value instanceof Calendar) {
                return factory.createValue((Calendar)value);
            }

            final Calendar calendar = Calendar.getInstance();
            final Date date = DateFormat.getDateInstance().parse(value.toString());
            calendar.setTime(date);

            return factory.createValue(calendar);
        }

        if (PropertyType.DECIMAL == jcrPropType) {
            if (value instanceof BigDecimal) {
                return factory.createValue((BigDecimal)value);
            }

            return factory.createValue(new BigDecimal(value.toString()));
        }

        return factory.createValue(value.toString());
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param values
     *        the values being converted to a JCR value holders (cannot be <code>null</code>)
     * @param jcrPropType
     *        the JCR {@link PropertyType property type}
     * @return the JCR value holders (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Value[] createValues( final ValueFactory factory,
                                        final Object[] values,
                                        final int jcrPropType ) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(values, "values"); //$NON-NLS-1$

        final List< Value > result = new ArrayList< Value >();

        if ((values == null) || (values.length == 0)) {
            return NO_VALUES;
        }

        for (final Object value : values) {
            result.add(createValue(factory, value, jcrPropType));
        }

        return result.toArray(new Value[result.size()]);
    }

    private final String path;
    private final Repository repository;

    /**
     * @param propertyRepository
     *        the repository where this property is located (cannot be <code>null</code>)
     * @param propertyPath
     *        the JCR property path (cannot be empty)
     * @throws KException
     *         if there is an error constructing the property
     */
    public PropertyImpl( final Repository propertyRepository,
                         final String propertyPath ) throws KException {
        ArgCheck.isNotNull(propertyRepository, "propertyRepository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(propertyPath, "propertyPath"); //$NON-NLS-1$

        this.repository = propertyRepository;
        this.path = propertyPath;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getAbsolutePath()
     */
    @Override
    public String getAbsolutePath() {
        return this.path;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBinaryValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public InputStream getBinaryValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Binary result = getSession(transaction).getProperty(this.path).getBinary();
            if (result == null)
                return null;

            return result.getStream();

        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBooleanValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean getBooleanValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final boolean result = getSession(transaction).getProperty(this.path).getBoolean();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBooleanValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean[] getBooleanValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final boolean[] booleanValues = new boolean[values.length];
            int i = 0;

            for (final Value value : values) {
                booleanValues[i++] = value.getBoolean();
            }

            return booleanValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar getDateValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Calendar result = getSession(transaction).getProperty(this.path).getDate();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar[] getDateValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final Calendar[] dateValues = new Calendar[values.length];
            int i = 0;

            for (final Value value : values) {
                dateValues[i++] = value.getDate();
            }

            return dateValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public BigDecimal getDecimalValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final BigDecimal result = getSession(transaction).getProperty(this.path).getDecimal();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public BigDecimal[] getDecimalValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final BigDecimal[] decimalValues = new BigDecimal[values.length];
            int i = 0;

            for (final Value value : values) {
                decimalValues[i++] = value.getDecimal();
            }

            return decimalValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyDescriptor getDescriptor( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final PropertyDescriptor result = new PropertyDescriptorImpl(property.getDefinition());
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public double getDoubleValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final double result = getSession(transaction).getProperty(this.path).getDouble();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public double[] getDoubleValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final double[] doubleValues = new double[values.length];
            int i = 0;

            for (final Value value : values) {
                doubleValues[i++] = value.getDouble();
            }

            return doubleValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLongValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final long result = getSession(transaction).getProperty(this.path).getLong();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long[] getLongValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final long[] longValues = new long[values.length];
            int i = 0;

            for (final Value value : values) {
                longValues[i++] = value.getLong();
            }

            return longValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getName( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final String result = getSession(transaction).getProperty(getAbsolutePath()).getName();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Node parent = getSession(transaction).getProperty(getAbsolutePath()).getParent();
            String parentPath = parent.getPath();

            if (!parentPath.endsWith("/")) { //$NON-NLS-1$
                parentPath += "/"; //$NON-NLS-1$
            }

            return new ObjectImpl(this.repository, parent.getPath(), 0);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getRepository()
     */
    @Override
    public Repository getRepository() {
        return this.repository;
    }

    private Session getSession( final UnitOfWork transaction ) {
        return ((UnitOfWorkImpl)transaction).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getStringValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final String result = getSession(transaction).getProperty(this.path).getString();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getStringValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final String[] stringValues = new String[values.length];
            int i = 0;

            for (final Value value : values) {
                stringValues[i++] = value.getString();
            }

            return stringValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Object getValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value value = property.getValue();
            final int propType = property.getType();
            final Object result = convert(value, propType);

            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Object[] getValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final int propType = property.getType();
            final Value[] values = property.getValues();
            final Object[] objectValues = new Object[values.length];
            int i = 0;

            for (final Value value : values) {
                objectValues[i++] = convert(value, propType);
            }

            return objectValues;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValueType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyValueType getValueType( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        PropertyValueType propertyValueType = PropertyValueType.UNDEFINED;

        try {
            final javax.jcr.Property result = getSession(transaction).getProperty(this.path);

            int requiredType = result.getDefinition().getRequiredType();
            switch (requiredType) {
                case PropertyType.NAME:
                case PropertyType.STRING:
                case PropertyType.PATH:
                case PropertyType.REFERENCE:
                case PropertyType.URI:
                case PropertyType.WEAKREFERENCE:
                    propertyValueType = PropertyValueType.STRING;
                    break;
                case PropertyType.LONG:
                    propertyValueType = PropertyValueType.LONG;
                    break;
                case PropertyType.DOUBLE:
                case PropertyType.DECIMAL:
                    propertyValueType = PropertyValueType.DOUBLE;
                    break;
                case PropertyType.DATE:
                    propertyValueType = PropertyValueType.CALENDAR;
                    break;
                case PropertyType.BOOLEAN:
                    propertyValueType = PropertyValueType.BOOLEAN;
                    break;
                case PropertyType.BINARY:
                case PropertyType.UNDEFINED:
                default:
                    propertyValueType = PropertyValueType.UNDEFINED;
            }

            return propertyValueType;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#isMultiple(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isMultiple( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Session session = getSession(transaction);
            final boolean result = session.getProperty(this.path).isMultiple();
            return result;
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#set(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.Object[])
     */
    @Override
    public void set( final UnitOfWork transaction,
                     final Object... values ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final Session session = getSession(transaction);
            final javax.jcr.Property property = session.getProperty(this.path);

            if (values == null) {
                property.remove();
            } else {
                final int count = values.length;
                final boolean multiple = property.isMultiple();
                final int type = property.getType();

                if (count == 0) {
                    // remove if property is multi-valued
                    if (multiple) {
                        property.remove();
                    } else {
                        // single-valued property
                        throw new KException(Messages.getString(Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                this.path,
                                                                getParent(transaction)));
                    }
                } else if (count > 1) {
                    if (multiple) {
                        property.setValue(createValues(session.getValueFactory(), values, type));
                    } else {
                        throw new KException(Messages.getString(Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                property.getName(),
                                                                getParent(transaction)));
                    }
                } else {
                    // only one value so set property
                    if (multiple) {
                        property.setValue(createValues(session.getValueFactory(), values, type));
                    } else {
                        property.setValue(createValue(session.getValueFactory(), values[0]));
                    }
                }
            }
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.path;
    }

}
