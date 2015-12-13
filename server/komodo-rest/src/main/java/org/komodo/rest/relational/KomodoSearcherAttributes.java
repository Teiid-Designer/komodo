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
package org.komodo.rest.relational;

import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;
import org.komodo.utils.ArgCheck;


/**
 * Object to be serialised by GSON that encapsulates a search object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoSearcherAttributes implements KRestEntity {

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
     * Label for the path
     */
    public static final String PATH_LABEL = "path"; //$NON-NLS-1$

    /**
     * Label for the contains
     */
    public static final String CONTAINS_LABEL = "contains"; //$NON-NLS-1$

    /**
     * Label for the name
     */
    public static final String OBJECT_NAME_LABEL = "objectName"; //$NON-NLS-1$

    @JsonProperty(SEARCH_NAME_LABEL)
    private String searchName;

    @JsonProperty(TYPE_LABEL)
    private String type;

    @JsonProperty(PARENT_LABEL)
    private String parent;

    @JsonProperty(ANCESTOR_LABEL)
    private String ancestor;

    @JsonProperty(PATH_LABEL)
    private String path;

    @JsonProperty(CONTAINS_LABEL)
    private String contains;

    @JsonProperty(OBJECT_NAME_LABEL)
    private String objectName;

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

    @Override
    @JsonIgnore
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    @JsonIgnore
    public Object getXml() {
        throw new UnsupportedOperationException();
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
     * @return the path
     */
    public String getPath() {
        return this.path;
    }

    /**
     * @param path the path to set
     */
    public void setPath(String path) {
        this.path = path;
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.ancestor == null) ? 0 : this.ancestor.hashCode());
        result = prime * result + ((this.contains == null) ? 0 : this.contains.hashCode());
        result = prime * result + ((this.objectName == null) ? 0 : this.objectName.hashCode());
        result = prime * result + ((this.parent == null) ? 0 : this.parent.hashCode());
        result = prime * result + ((this.path == null) ? 0 : this.path.hashCode());
        result = prime * result + ((this.searchName == null) ? 0 : this.searchName.hashCode());
        result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        KomodoSearcherAttributes other = (KomodoSearcherAttributes)obj;
        if (this.ancestor == null) {
            if (other.ancestor != null)
                return false;
        } else
            if (!this.ancestor.equals(other.ancestor))
                return false;
        if (this.contains == null) {
            if (other.contains != null)
                return false;
        } else
            if (!this.contains.equals(other.contains))
                return false;
        if (this.objectName == null) {
            if (other.objectName != null)
                return false;
        } else
            if (!this.objectName.equals(other.objectName))
                return false;
        if (this.parent == null) {
            if (other.parent != null)
                return false;
        } else
            if (!this.parent.equals(other.parent))
                return false;
        if (this.path == null) {
            if (other.path != null)
                return false;
        } else
            if (!this.path.equals(other.path))
                return false;
        if (this.searchName == null) {
            if (other.searchName != null)
                return false;
        } else
            if (!this.searchName.equals(other.searchName))
                return false;
        if (this.type == null) {
            if (other.type != null)
                return false;
        } else
            if (!this.type.equals(other.type))
                return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoSearcherAttributes [searchName=" + this.searchName + ", type=" + this.type + ", parent=" + this.parent + ", ancestor=" + this.ancestor + ", path=" + this.path + ", contains=" + this.contains + ", objectName=" + this.objectName + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    }
}
