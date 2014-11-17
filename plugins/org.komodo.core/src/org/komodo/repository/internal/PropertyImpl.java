/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.repository.internal;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import org.komodo.core.Messages;
import org.komodo.repository.internal.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * The base class for a {@link Property Komodo object property}.
 */
class PropertyImpl implements Property {

    /**
     * An empty array of values.
     */
    static final Value[] NO_VALUES = new Value[0];

    /**
     * @param value
     *        the JCR value holder (cannot be <code>null</code>)
     * @param propertyType
     *        the required type of the property
     * @return the <code>Object</code> representation of the JCR value (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    static Object convert( final Value value,
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
                    return value.toString();
            }
        } catch (final Exception e) {
            throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_CONVERT_VALUE, propertyType), e);
        }
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param value
     *        the value being converted to a JCR value holder (cannot be <code>null</code>)
     * @return the JCR value holder (never <code>null</code>)
     */
    static Value createValue( final ValueFactory factory,
                              final Object value ) {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(value, "value"); //$NON-NLS-1$

        if (value instanceof Value) return (Value)value;
        if (value instanceof Boolean) return factory.createValue(Boolean.class.cast(value));
        if (value instanceof Long) return factory.createValue(Long.class.cast(value));
        if (value instanceof Double) return factory.createValue(Double.class.cast(value));
        if (value instanceof Calendar) return factory.createValue(Calendar.class.cast(value));
        if (value instanceof BigDecimal) return factory.createValue(BigDecimal.class.cast(value));
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
    static Value createValue( final ValueFactory factory,
                              final Object value,
                              final int jcrPropType ) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(value, "value"); //$NON-NLS-1$

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
    static Value[] createValues( final ValueFactory factory,
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

    private final String name;
    private final ObjectImpl parent;
    private final String path;

    /**
     * @param propertyParent
     *        the property's parent (cannot be <code>null</code>)
     * @param jcrProperty
     *        the JCR property (cannot be <code>null</code>)
     * @throws KException
     *         if there is an error constructing the property
     */
    PropertyImpl( final ObjectImpl propertyParent,
                  final javax.jcr.Property jcrProperty ) throws KException {
        ArgCheck.isNotNull(propertyParent, "propertyParent"); //$NON-NLS-1$
        ArgCheck.isNotNull(jcrProperty, "jcrProperty"); //$NON-NLS-1$

        this.parent = propertyParent;

        try {
            this.path = jcrProperty.getPath();
            this.name = jcrProperty.getName();
        } catch (final Exception e) {
            throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_CONSTRUCT_PROPERTY, this.parent.getName()), e);
        }
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
     * @see org.komodo.spi.repository.Property#getBooleanValue()
     */
    @Override
    public boolean getBooleanValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getBooleanValue", true, null); //$NON-NLS-1$

        try {
            final boolean result = getSession(transaction).getProperty(this.path).getBoolean();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getBooleanValues()
     */
    @Override
    public boolean[] getBooleanValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getBooleanValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final boolean[] booleanValues = new boolean[values.length];
            int i = 0;

            for (final Value value : values) {
                booleanValues[i++] = value.getBoolean();
            }

            transaction.commit();
            return booleanValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValue()
     */
    @Override
    public Calendar getDateValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDateValue", true, null); //$NON-NLS-1$

        try {
            final Calendar result = getSession(transaction).getProperty(this.path).getDate();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDateValues()
     */
    @Override
    public Calendar[] getDateValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDateValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final Calendar[] dateValues = new Calendar[values.length];
            int i = 0;

            for (final Value value : values) {
                dateValues[i++] = value.getDate();
            }

            transaction.commit();
            return dateValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValue()
     */
    @Override
    public BigDecimal getDecimalValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDecimalValue", true, null); //$NON-NLS-1$

        try {
            final BigDecimal result = getSession(transaction).getProperty(this.path).getDecimal();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDecimalValues()
     */
    @Override
    public BigDecimal[] getDecimalValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDecimalValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final BigDecimal[] decimalValues = new BigDecimal[values.length];
            int i = 0;

            for (final Value value : values) {
                decimalValues[i++] = value.getDecimal();
            }

            transaction.commit();
            return decimalValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDescriptor()
     */
    @Override
    public PropertyDescriptor getDescriptor() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDescriptor", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            return new PropertyDescriptorImpl(property.getDefinition());
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValue()
     */
    @Override
    public double getDoubleValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDoubleValue", true, null); //$NON-NLS-1$

        try {
            final double result = getSession(transaction).getProperty(this.path).getDouble();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getDoubleValues()
     */
    @Override
    public double[] getDoubleValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getDoubleValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final double[] doubleValues = new double[values.length];
            int i = 0;

            for (final Value value : values) {
                doubleValues[i++] = value.getDouble();
            }

            transaction.commit();
            return doubleValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValue()
     */
    @Override
    public long getLongValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getLongValue", true, null); //$NON-NLS-1$

        try {
            final long result = getSession(transaction).getProperty(this.path).getLong();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getLongValues()
     */
    @Override
    public long[] getLongValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getLongValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final long[] longValues = new long[values.length];
            int i = 0;

            for (final Value value : values) {
                longValues[i++] = value.getLong();
            }

            transaction.commit();
            return longValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent()
     */
    @Override
    public KomodoObject getParent() {
        return this.parent;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getRepository()
     */
    @Override
    public Repository getRepository() {
        return this.parent.getRepository();
    }

    private Session getSession( final UnitOfWork transaction ) {
        return ((UnitOfWorkImpl)transaction).getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValue()
     */
    @Override
    public String getStringValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getStringValue", true, null); //$NON-NLS-1$

        try {
            final String result = getSession(transaction).getProperty(this.path).getString();
            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getStringValues()
     */
    @Override
    public String[] getStringValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getStringValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value[] values = property.getValues();
            final String[] stringValues = new String[values.length];
            int i = 0;

            for (final Value value : values) {
                stringValues[i++] = value.getString();
            }

            transaction.commit();
            return stringValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValue()
     */
    @Override
    public Object getValue() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getValue", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final Value value = property.getValue();
            final int propType = property.getType();
            final Object result = convert(value, propType);

            transaction.commit();
            return result;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#getValues()
     */
    @Override
    public Object[] getValues() throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-getValues", true, null); //$NON-NLS-1$

        try {
            final javax.jcr.Property property = getSession(transaction).getProperty(this.path);
            final int propType = property.getType();
            final Value[] values = property.getValues();
            final Object[] objectValues = new Object[values.length];
            int i = 0;

            for (final Value value : values) {
                objectValues[i++] = convert(value, propType);
            }

            transaction.commit();
            return objectValues;
        } catch (final Exception e) {
            transaction.rollback();

            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Property#set(java.lang.Object[])
     */
    @Override
    public void set( final Object... values ) throws KException {
        final UnitOfWork transaction = getRepository().createTransaction("property-set", true, null); //$NON-NLS-1$

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
                        throw new KException(
                                             Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                path,
                                                                parent.getName()));
                    }
                } else if (count > 1) {
                    if (multiple) {
                        property.setValue(createValues(session.getValueFactory(), values, type));
                    } else {
                        throw new KException(
                                             Messages.getString(Messages.Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                property.getName(),
                                                                parent.getName()));
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

            transaction.commit();
        } catch (final Exception e) {
            transaction.rollback();

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
