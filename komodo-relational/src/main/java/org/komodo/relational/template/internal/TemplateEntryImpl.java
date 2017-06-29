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
package org.komodo.relational.template.internal;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Properties;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Implementation of template instance model
 */
public class TemplateEntryImpl extends RelationalObjectImpl implements TemplateEntry, EventManager {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public TemplateEntryImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return TemplateEntry.IDENTIFIER;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid id property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getId( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property prop = getRawProperty( transaction, JcrLexicon.UUID.getString() );
        final String result = prop.getStringValue( transaction );
        return result;
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

    @Override
    public String getDescription(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, "getDescription", DataVirtLexicon.TemplateEntry.DESCRIPTION); //$NON-NLS-1$
        return value != null ? value : EMPTY_STRING;
    }

    @Override
    public void setDescription(UnitOfWork transaction, String description) throws KException {
        setObjectProperty(transaction, "setDescription", DataVirtLexicon.TemplateEntry.DESCRIPTION, description);
    }

    @Override
    public String getDisplayName(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, "getDisplayName", DataVirtLexicon.TemplateEntry.DISPLAY_NAME); //$NON-NLS-1$
        return value != null ? value : EMPTY_STRING;
    }

    @Override
    public void setDisplayName(UnitOfWork transaction, String displayName) throws KException {
        setObjectProperty(transaction, "setDisplayName", DataVirtLexicon.TemplateEntry.DISPLAY_NAME, displayName);
    }

    @Override
    public Collection<Object> getAllowedValues(UnitOfWork transaction) throws KException {
        Property property = getProperty(transaction, DataVirtLexicon.TemplateEntry.ALLOWED_VALUES);
        if (property == null)
            return Collections.emptyList();

        return Arrays.asList(property.getValues(transaction));
    }

    @Override
    public void setAllowedValues(UnitOfWork transaction, Collection<Object> allowedValues) throws KException {
        setObjectProperty(transaction, "setAllowedValues", DataVirtLexicon.TemplateEntry.ALLOWED_VALUES, allowedValues.toArray(new Object[0]));
    }

    @Override
    public String getCategory(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, "getCategory", DataVirtLexicon.TemplateEntry.CATEGORY); //$NON-NLS-1$
        return value != null ? value : EMPTY_STRING;
    }

    @Override
    public void setCategory(UnitOfWork transaction, String category) throws KException {
        setObjectProperty(transaction, "setCategory", DataVirtLexicon.TemplateEntry.CATEGORY, category);
    }

    @Override
    public Object getDefaultValue(UnitOfWork transaction) throws KException {
        String typeClass = getTypeClassName(transaction);

        PropertyValueType propertyType;
        if (Boolean.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.BOOLEAN;
        else if (Long.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.LONG;
        else if (Double.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.DOUBLE;
        else if (Integer.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.INTEGER;
        else if (BigDecimal.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.BIG_DECIMAL;
        else if (Calendar.class.getCanonicalName().equals(typeClass) || Date.class.getCanonicalName().equals(typeClass))
            propertyType = PropertyValueType.CALENDAR;
        else
            propertyType = PropertyValueType.STRING;

        Object value = getObjectProperty(transaction, propertyType, "getDefaultValue", DataVirtLexicon.TemplateEntry.DEFAULT_VALUE); //$NON-NLS-1$
        return value;
    }

    @Override
    public void setDefaultValue(UnitOfWork transaction, Object defaultValue) throws KException {
        setObjectProperty(transaction, "setDefaultValue", DataVirtLexicon.TemplateEntry.DEFAULT_VALUE, defaultValue);
    }

    @Override
    public String getTypeClassName(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, "getTypeClassName", DataVirtLexicon.TemplateEntry.TYPE_CLASS_NAME); //$NON-NLS-1$
        return value != null ? value : EMPTY_STRING;
    }

    @Override
    public void setTypeClassName(UnitOfWork transaction, String typeClassName) throws KException {
        setObjectProperty(transaction, "setTypeClassName", DataVirtLexicon.TemplateEntry.TYPE_CLASS_NAME, typeClassName);
    }

    @Override
    public boolean isConstrainedToAllowedValues(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.BOOLEAN, "isConstrainedToAllowedValues", //$NON-NLS-1$
                                 DataVirtLexicon.TemplateEntry.CONSTRAINED_TO_ALLOWED_VALUES);
    }

    @Override
    public void setConstrainedToAllowedValues(UnitOfWork transaction, boolean constrainedToAllowedValues) throws KException {
        setObjectProperty(transaction, "setConstrainedToAllowedValues", DataVirtLexicon.TemplateEntry.CONSTRAINED_TO_ALLOWED_VALUES, constrainedToAllowedValues);
    }

    @Override
    public boolean isAdvanced(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.BOOLEAN, "isAdvanced", //$NON-NLS-1$
                                 DataVirtLexicon.TemplateEntry.ADVANCED);
    }

    @Override
    public void setAdvanced(UnitOfWork transaction, boolean advanced) throws KException {
        setObjectProperty(transaction, "setAdvanced", DataVirtLexicon.TemplateEntry.ADVANCED, advanced);
    }

    @Override
    public boolean isMasked(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.BOOLEAN, "isMasked", //$NON-NLS-1$
                                 DataVirtLexicon.TemplateEntry.MASKED);
    }

    @Override
    public void setMasked(UnitOfWork transaction, boolean masked) throws KException {
        setObjectProperty(transaction, "setMasked", DataVirtLexicon.TemplateEntry.MASKED, masked);
    }

    @Override
    public boolean isModifiable(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.BOOLEAN, "isModifiable", //$NON-NLS-1$
                                 DataVirtLexicon.TemplateEntry.MODIFIABLE);
    }

    @Override
    public void setModifiable(UnitOfWork transaction, boolean modifiable) throws KException {
        setObjectProperty(transaction, "setModifiable", DataVirtLexicon.TemplateEntry.MODIFIABLE, modifiable);
    }

    @Override
    public boolean isRequired(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.BOOLEAN, "isRequired", //$NON-NLS-1$
                                 DataVirtLexicon.TemplateEntry.REQUIRED);
    }

    @Override
    public void setRequired(UnitOfWork transaction, boolean required) throws KException {
        setObjectProperty(transaction, "setRequired", DataVirtLexicon.TemplateEntry.REQUIRED, required);
    }

    @Override
    public Properties getCustomProperties(UnitOfWork transaction) throws KException {
        Properties properties = new Properties();

        String[] propertyNames = getPropertyNames(transaction);
        for (String propertyName : propertyNames) {
            if (! propertyName.startsWith(TemplateEntry.CUSTOM_PREFIX))
                continue;

            String value = getObjectProperty(transaction, PropertyValueType.STRING, "get" + propertyName, propertyName);
            properties.setProperty(propertyName, value);
        }

        return properties;
    }

    @Override
    public void addCustomProperty(UnitOfWork transaction, String key, String value) throws KException {
        setObjectProperty(transaction, "set" + key, TemplateEntry.CUSTOM_PREFIX + key.toString(), value);
    }

    @Override
    public void setCustomProperties(UnitOfWork transaction, Properties properties) throws KException {
        for (Object key : properties.keySet()) {
            String value = properties.getProperty(key.toString());
            addCustomProperty(transaction, key.toString(), value);
        }
    }

    @Override
    public boolean addListener(ExecutionConfigurationListener listener) {
        return false;
    }

    @Override
    public void permitListeners(boolean enable) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners(ExecutionConfigurationEvent event) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener(ExecutionConfigurationListener listener) {
        return false;
    }
}
