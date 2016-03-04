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

import javax.jcr.PropertyType;
import javax.jcr.Value;
import javax.jcr.nodetype.PropertyDefinition;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;

/**
 * A {@link PropertyDescriptor property descriptor} implementation.
 */
public class PropertyDescriptorImpl implements PropertyDescriptor {

    /**
     * @param type
     *        the {@link PropertyType JCR property type} being converted
     * @return the object property type or {@link org.komodo.spi.repository.PropertyDescriptor.Type#UNDEFINED} if type cannot be
     *         determined
     */
    public static Type convert( final int type ) {
        if (type == PropertyType.BINARY) return Type.BINARY;
        if (type == PropertyType.BOOLEAN) return Type.BOOLEAN;
        if (type == PropertyType.DATE) return Type.DATE;
        if (type == PropertyType.DECIMAL) return Type.DECIMAL;
        if (type == PropertyType.DOUBLE) return Type.DOUBLE;
        if (type == PropertyType.LONG) return Type.LONG;
        if (type == PropertyType.NAME) return Type.NAME;
        if (type == PropertyType.PATH) return Type.PATH;
        if (type == PropertyType.REFERENCE) return Type.REFERENCE;
        if (type == PropertyType.STRING) return Type.STRING;
        if (type == PropertyType.URI) return Type.URI;
        if (type == PropertyType.WEAKREFERENCE) return Type.WEAKREFERENCE;

        return Type.UNDEFINED;
    }

    /**
     * @param type
     *        the {@link KomodoObject Komodo object} {@link Property property}
     *        {@link org.komodo.spi.repository.PropertyDescriptor.Type type} being converted (cannot be <code>null</code>)
     * @return the {@link PropertyType JCR property type}
     */
    public static int convert( final Type type ) {
        ArgCheck.isNotNull(type, "type"); //$NON-NLS-1$

        if (type == Type.BINARY) return PropertyType.BINARY;
        if (type == Type.BOOLEAN) return PropertyType.BOOLEAN;
        if (type == Type.DATE) return PropertyType.DATE;
        if (type == Type.DECIMAL) return PropertyType.DECIMAL;
        if (type == Type.DOUBLE) return PropertyType.DOUBLE;
        if (type == Type.LONG) return PropertyType.LONG;
        if (type == Type.NAME) return PropertyType.NAME;
        if (type == Type.PATH) return PropertyType.PATH;
        if (type == Type.REFERENCE) return PropertyType.REFERENCE;
        if (type == Type.STRING) return PropertyType.STRING;
        if (type == Type.URI) return PropertyType.URI;
        if (type == Type.WEAKREFERENCE) return PropertyType.WEAKREFERENCE;

        return PropertyType.UNDEFINED;
    }

    private final Object[] defaultValues;
    private final boolean mandatory;
    private final boolean modifiable;
    private final boolean multiple;
    private final String name;
    private final Type type;

    /**
     * @param propDefn
     *        the JCR property definition (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public PropertyDescriptorImpl( final PropertyDefinition propDefn ) throws KException {
        ArgCheck.isNotNull(propDefn, "propDefn"); //$NON-NLS-1$

        this.mandatory = propDefn.isMandatory();
        this.modifiable = propDefn.isProtected();
        this.multiple = propDefn.isMultiple();
        this.name = propDefn.getName();
        this.type = convert(propDefn.getRequiredType());

        final Value[] values = propDefn.getDefaultValues();

        if ((values == null) || (values.length == 0)) {
            this.defaultValues = NO_VALUES;
        } else {
            this.defaultValues = new Object[values.length];
            final int type = propDefn.getRequiredType();
            int i = 0;

            for (final Value value : values) {
                this.defaultValues[i++] = PropertyImpl.convert(value, type);
            }
        }
    }

    /**
     * @param mandatory
     *        <code>true</code> if the property is mandatory
     * @param modifiable
     *        <code>true</code> if the property is modifiable
     * @param multiple
     *        <code>true</code> if the property is multi-valued
     * @param name
     *        the property name (cannot be empty)
     * @param type
     *        the property type (cannot be <code>null</code>)
     * @param values
     *        the default values (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    public PropertyDescriptorImpl( final boolean mandatory,
                                   final boolean modifiable,
                                   final boolean multiple,
                                   final String name,
                                   final Type type,
                                   final Object[] values ) throws KException {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotNull( type, "type" ); //$NON-NLS-1$

        this.mandatory = mandatory;
        this.modifiable = modifiable;
        this.multiple = multiple;
        this.name = name;
        this.type = type;

        if ( ( values == null ) || ( values.length == 0 ) ) {
            this.defaultValues = NO_VALUES;
        } else {
            this.defaultValues = new Object[ values.length ];
            int i = 0;

            for ( final Object value : values ) {
                this.defaultValues[i++] = PropertyImpl.convert( value, convert( this.type ) );
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#getDefaultValues()
     */
    @Override
    public Object[] getDefaultValues() {
        return this.defaultValues;
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
        return this.type;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isMandatory()
     */
    @Override
    public boolean isMandatory() {
        return this.mandatory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isModifiable()
     */
    @Override
    public boolean isModifiable() {
        return this.modifiable;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isMultiple()
     */
    @Override
    public boolean isMultiple() {
        return this.multiple;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getName();
    }

}
