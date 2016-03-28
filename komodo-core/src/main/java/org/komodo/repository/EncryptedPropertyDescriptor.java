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

import org.komodo.spi.repository.PropertyDescriptor;

/**
 * A {@link PropertyDescriptor property descriptor} implementation for encrypted properties.
 */
public class EncryptedPropertyDescriptor implements PropertyDescriptor {

    private final Object[] defaultValues;
    private final boolean mandatory;
    private final boolean modifiable;
    private final boolean multiple;
    private final String name;
    private final Type type;

    /**
     * Constructs a property descriptor for an encrypted property.
     *
     * @param delegate
     *        the property descriptor delegate
     */
    public EncryptedPropertyDescriptor( final PropertyDescriptor delegate ) {
        this.defaultValues = delegate.getDefaultValues();
        this.mandatory = delegate.isMandatory();
        this.modifiable = delegate.isModifiable();
        this.multiple = delegate.isMultiple();
        this.name = delegate.getName();
        this.type = delegate.getType();
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
     * @see org.komodo.spi.repository.PropertyDescriptor#isEncrypted()
     */
    @Override
    public final boolean isEncrypted() {
        return true;
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

}
