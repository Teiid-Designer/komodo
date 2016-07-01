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
package org.komodo.rest.relational.request;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.utils.ArgCheck;


/**
 * Object to be serialised by GSON that encapsulates a search object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoSearcherAttributes extends KomodoPathAttribute {

    /**
     * Label for the search name
     */
    public static final String SEARCH_NAME_LABEL = "searchName"; //$NON-NLS-1$

    /**
     * Label for the type
     */
    public static final String TYPE_LABEL = "type"; //$NON-NLS-1$

    /**
     * Label for the parent
     */
    public static final String PARENT_LABEL = "parent"; //$NON-NLS-1$

    /**
     * Label for the ancestor
     */
    public static final String ANCESTOR_LABEL = "ancestor"; //$NON-NLS-1$

    /**
     * Label for the contains
     */
    public static final String CONTAINS_LABEL = "contains"; //$NON-NLS-1$

    /**
     * Label for the name
     */
    public static final String OBJECT_NAME_LABEL = "objectName"; //$NON-NLS-1$

    /**
     * Label for the parameters
     */
    public static final String PARAMETERS_LABEL = "parameters"; //$NON-NLS-1$

    @JsonProperty(SEARCH_NAME_LABEL)
    private String searchName;

    @JsonProperty(TYPE_LABEL)
    private String type;

    @JsonProperty(PARENT_LABEL)
    private String parent;

    @JsonProperty(ANCESTOR_LABEL)
    private String ancestor;

    @JsonProperty(CONTAINS_LABEL)
    private String contains;

    @JsonProperty(OBJECT_NAME_LABEL)
    private String objectName;

    @JsonProperty(PARAMETERS_LABEL)
    private Map<String, String> parameters;

    /**
     * Default constructor for deserialization
     */
    public KomodoSearcherAttributes() {
        // do nothing
    }

    /**
     * @param name the name of this search
     *
     */
    public KomodoSearcherAttributes(String name) {
        ArgCheck.isNotNull(name);
        this.searchName = name;
    }

    /**
     * @return the searchName
     */
    public String getSearchName() {
        return this.searchName;
    }

    /**
     * @param searchName the searchName to set
     */
    public void setSearchName(String searchName) {
        this.searchName = searchName;
    }

    /**
     * @return the type
     */
    public String getType() {
        return this.type;
    }

    /**
     * @param type the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the parent
     */
    public String getParent() {
        return this.parent;
    }

    /**
     * @param parent the parent to set
     */
    public void setParent(String parent) {
        this.parent = parent;
    }

    /**
     * @return the ancestor
     */
    public String getAncestor() {
        return this.ancestor;
    }

    /**
     * @param ancestor the ancestor to set
     */
    public void setAncestor(String ancestor) {
        this.ancestor = ancestor;
    }

    /**
     * @return the contains
     */
    public String getContains() {
        return this.contains;
    }

    /**
     * @param contains the contains to set
     */
    public void setContains(String contains) {
        this.contains = contains;
    }

    /**
     * @return the objectName
     */
    public String getObjectName() {
        return this.objectName;
    }

    /**
     * @param objectName the objectName to set
     */
    public void setObjectName(String objectName) {
        this.objectName = objectName;
    }

    /**
     * @return the parameters
     */
    public Map<String, String> getParameters() {
        if (parameters == null)
            return Collections.emptyMap();

        return Collections.unmodifiableMap(this.parameters);
    }

    /**
     * Add a parameter with value
     * @param name the name
     * @param value the value
     */
    public void setParameter(String name, String value) {
        if (this.parameters == null)
            this.parameters = new HashMap<>();

        this.parameters.put(name, value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((ancestor == null) ? 0 : ancestor.hashCode());
        result = prime * result + ((contains == null) ? 0 : contains.hashCode());
        result = prime * result + ((objectName == null) ? 0 : objectName.hashCode());
        result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
        result = prime * result + ((parent == null) ? 0 : parent.hashCode());
        result = prime * result + ((searchName == null) ? 0 : searchName.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        KomodoSearcherAttributes other = (KomodoSearcherAttributes)obj;
        if (ancestor == null) {
            if (other.ancestor != null)
                return false;
        } else if (!ancestor.equals(other.ancestor))
            return false;
        if (contains == null) {
            if (other.contains != null)
                return false;
        } else if (!contains.equals(other.contains))
            return false;
        if (objectName == null) {
            if (other.objectName != null)
                return false;
        } else if (!objectName.equals(other.objectName))
            return false;
        if (parameters == null) {
            if (other.parameters != null)
                return false;
        } else if (!parameters.equals(other.parameters))
            return false;
        if (parent == null) {
            if (other.parent != null)
                return false;
        } else if (!parent.equals(other.parent))
            return false;
        if (searchName == null) {
            if (other.searchName != null)
                return false;
        } else if (!searchName.equals(other.searchName))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoSearcherAttributes [searchName=" + this.searchName + ", type=" + this.type + ", parent=" + this.parent + ", ancestor=" + this.ancestor + ", contains=" + this.contains + ", objectName=" + this.objectName + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    }
}
