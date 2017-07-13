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
package org.komodo.rest.relational.connection;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.codehaus.jackson.annotate.JsonProperty;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A templateEntry that can be used by GSON to build a JSON document representation.
 */
public final class RestTemplateEntry extends RestBasicEntity {

    /**
     * Required label
     */
    public static final String REQUIRED_LABEL = "required";

    /**
     * Modifiable label
     */
    public static final String MODIFIABLE_LABEL = "modifiable";

    /**
     * Masked label
     */
    public static final String MASKED_LABEL = "masked";

    /**
     * Constrained to allowed values label
     */
    public static final String CONSTRAINED_ALLOWED_VALUES_LABEL = "constrainedToAllowedValues";

    /**
     * Advanced label
     */
    public static final String ADVANCED_LABEL = "advanced";

    /**
     * Type class name label
     */
    public static final String TYPE_CLASS_NAME_LABEL = "typeClassName";

    /**
     * Display label
     */
    public static final String DISPLAY_NAME_LABEL = "displayName";

    /**
     * Description label
     */
    public static final String DESCRIPTION_LABEL = "description";

    /**
     * Default value label
     */
    public static final String DEFAULT_VALUE_LABEL = "defaultValue";

    /**
     * Category label
     */
    public static final String CATEGORY_LABEL = "category";

    /**
     * Allowed values label
     */
    public static final String ALLOWED_VALUES_LABEL = "allowedValues";

    /**
     * Custom properties label
     */
    public static final String CUSTOM_PROPERTIES_LABEL = "customProperties";

    /**
     * An empty array of templateEntrys.
     */
    public static final RestTemplateEntry[] NO_ENTRIES = new RestTemplateEntry[ 0 ];

    @JsonProperty(CUSTOM_PROPERTIES_LABEL)
    private Map<String, String> customProperties;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestTemplateEntry() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param templateEntry the templateEntry
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestTemplateEntry(URI baseUri, TemplateEntry templateEntry, UnitOfWork uow) throws KException {
        super(baseUri, templateEntry, uow, false);

        this.setAllowedValues(templateEntry.getAllowedValues(uow));
        this.setCategory(templateEntry.getCategory(uow));
        this.setCustomProperties(templateEntry.getCustomProperties(uow));
        this.setDefaultValue(templateEntry.getDefaultValue(uow));
        this.setDescription(templateEntry.getDescription(uow));
        this.setDisplayName(templateEntry.getDisplayName(uow));
        this.setTypeClassName(templateEntry.getTypeClassName(uow));
        this.setAdvanced(templateEntry.isAdvanced(uow));
        this.setConstrainedToAllowedValues(templateEntry.isConstrainedToAllowedValues(uow));
        this.setMasked(templateEntry.isMasked(uow));
        this.setModifiable(templateEntry.isModifiable(uow));
        this.setRequired(templateEntry.isRequired(uow));

        Template template = ancestor(templateEntry, Template.class, uow);
        ArgCheck.isNotNull(template);
        String templateName = template.getName(uow);

        CachedTeiid cachedTeiid = ancestor(template, CachedTeiid.class, uow);
        ArgCheck.isNotNull(cachedTeiid);
        String teiidName = cachedTeiid.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.TEMPLATE_NAME, templateName);
        getUriBuilder().addSetting(settings, SettingNames.TEIID_NAME, teiidName);
        getUriBuilder().addSetting(settings, SettingNames.TEMPLATE_NAME, templateName);
        getUriBuilder().addSetting(settings, SettingNames.TEMPLATE_ENTRY_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().templateEntryUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().templateEntryUri(LinkType.PARENT, settings)));
    }

    /**
     * @return required flag
     */
    public boolean isRequired() {
        Object value = tuples.get(REQUIRED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param required
     */
    public void setRequired(boolean required) {
        tuples.put(REQUIRED_LABEL, required);
    }

    /**
     * @return modifiable flag
     */
    public boolean isModifiable() {
        Object value = tuples.get(MODIFIABLE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param modifiable
     */
    public void setModifiable(boolean modifiable) {
        tuples.put(MODIFIABLE_LABEL, modifiable);
    }

    /**
     * @return masked flag
     */
    public boolean isMasked() {
        Object value = tuples.get(MASKED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param masked
     */
    public void setMasked(boolean masked) {
        tuples.put(MASKED_LABEL, masked);
    }

    /**
     * @return is constrained to allowed values flag
     */
    public boolean isConstrainedToAllowedValues() {
        Object value = tuples.get(CONSTRAINED_ALLOWED_VALUES_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param constrainedToAllowedValues
     */
    public void setConstrainedToAllowedValues(boolean constrainedToAllowedValues) {
        tuples.put(CONSTRAINED_ALLOWED_VALUES_LABEL, constrainedToAllowedValues);
    }

    /**
     * @return advanced flag
     */
    public boolean isAdvanced() {
        Object value = tuples.get(ADVANCED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param advanced
     */
    public void setAdvanced(boolean advanced) {
        tuples.put(ADVANCED_LABEL, advanced);
    }

    /**
     * @return the type class name
     */
    public String getTypeClassName() {
        Object value = tuples.get(TYPE_CLASS_NAME_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param typeClassName
     */
    public void setTypeClassName(String typeClassName) {
        tuples.put(TYPE_CLASS_NAME_LABEL, typeClassName);
    }

    /**
     * @return the display name
     */
    public String getDisplayName() {
        Object value = tuples.get(DISPLAY_NAME_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param displayName
     */
    public void setDisplayName(String displayName) {
        tuples.put(DISPLAY_NAME_LABEL, displayName);
    }

    /**
     * @return the description
     */
    public String getDescription() {
        Object value = tuples.get(DESCRIPTION_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param description
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }

    /**
     * @return the default value
     */
    public Object getDefaultValue() {
        Object value = tuples.get(DEFAULT_VALUE_LABEL);
        return value != null ? value : null;
    }

    /**
     * @param defaultValue
     */
    public void setDefaultValue(Object defaultValue) {
        tuples.put(DEFAULT_VALUE_LABEL, defaultValue);
    }

    /**
     * @return the customProperties
     */
    public Map<String, String> getCustomProperties() {
        if (customProperties == null)
            return Collections.emptyMap();

        return Collections.unmodifiableMap(this.customProperties);
    }

    public void setCustomProperties(Properties customProperties) {
        if (this.customProperties == null)
            this.customProperties = new HashMap<>();

        if (customProperties != null) {
            for (Object keyObj : customProperties.keySet()) {
                String key = keyObj.toString();
                this.customProperties.put(key, customProperties.getProperty(key));
            }
        }
    }

    /**
     * @return the category
     */
    public String getCategory() {
        Object value = tuples.get(CATEGORY_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param category
     */
    public void setCategory(String category) {
        tuples.put(CATEGORY_LABEL, category);
    }

    /**
     * @return the allowed values
     */
    public Object[] getAllowedValues( ) {
        Object[] values = (Object[]) tuples.get(ALLOWED_VALUES_LABEL);
        return values != null ? values : null;
    }

    /**
     * @param allowedValues
     */
    public <T> void setAllowedValues(Collection<T> allowedValues) {
        if (allowedValues == null || allowedValues.isEmpty())
            return;

        tuples.put(ALLOWED_VALUES_LABEL, allowedValues.toArray(new Object[0]));
    }
}
