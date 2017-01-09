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

import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;


/**
 * Object to be serialised by GSON that encapsulates properties for VDB update request
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoVdbUpdateAttributes implements KRestEntity {

    /**
     * Label for the VDB name
     */
    public static final String VDB_NAME_LABEL = "vdbName"; //$NON-NLS-1$

    /**
     * Label for the Model name
     */
    public static final String MODEL_NAME_LABEL = "modelName"; //$NON-NLS-1$

    /**
     * Label for the teiid VDB name
     */
    public static final String TEIID_VDB_NAME_LABEL = "teiidVdbName"; //$NON-NLS-1$

    /**
     * Label for the teiid Model name
     */
    public static final String TEIID_MODEL_NAME_LABEL = "teiidModelName"; //$NON-NLS-1$

    @JsonProperty(VDB_NAME_LABEL)
    private String vdbName;

    @JsonProperty(MODEL_NAME_LABEL)
    private String modelName;

    @JsonProperty(TEIID_VDB_NAME_LABEL)
    private String teiidVdbName;

    @JsonProperty(TEIID_MODEL_NAME_LABEL)
    private String teiidModelName;

    /**
     * Default constructor for deserialization
     */
    public KomodoVdbUpdateAttributes() {
        // do nothing
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
     * @return vdbName
     */
    public String getVdbName() {
        return vdbName;
    }

    /**
     * @param vdbName the Vdb name
     */
    public void setVdbName(String vdbName) {
        this.vdbName = vdbName;
    }

    /**
     * @return modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @param modelName the Model name
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    /**
     * @return teiidVdbName
     */
    public String getTeiidVdbName() {
        return teiidVdbName;
    }

    /**
     * @param teiidVdbName the Teiid Vdb name
     */
    public void setTeiidVdbName(String teiidVdbName) {
        this.teiidVdbName = teiidVdbName;
    }

    /**
     * @return teiidModelName
     */
    public String getTeiidModelName() {
        return teiidModelName;
    }

    /**
     * @param teiidModelName the Teiid Model name
     */
    public void setTeiidModelName(String teiidModelName) {
        this.teiidModelName = teiidModelName;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((vdbName == null) ? 0 : vdbName.hashCode());
        result = prime * result + ((modelName == null) ? 0 : modelName.hashCode());
        result = prime * result + ((teiidVdbName == null) ? 0 : teiidVdbName.hashCode());
        result = prime * result + ((teiidModelName == null) ? 0 : teiidModelName.hashCode());
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
        KomodoVdbUpdateAttributes other = (KomodoVdbUpdateAttributes)obj;
        if (teiidModelName == null) {
            if (other.teiidModelName != null)
                return false;
        } else if (!teiidModelName.equals(other.teiidModelName))
            return false;
        if (teiidVdbName == null) {
            if (other.teiidVdbName != null)
                return false;
        } else if (!teiidVdbName.equals(other.teiidVdbName))
            return false;
        if (modelName == null) {
            if (other.modelName != null)
                return false;
        } else if (!modelName.equals(other.modelName))
            return false;
        if (vdbName == null) {
            if (other.vdbName != null)
                return false;
        } else if (!vdbName.equals(other.vdbName))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoVdbUpdateAttributes [vdbName=" + vdbName + ", modelName=" + modelName + ", teiidVdbName=" + teiidVdbName + ", teiidModelName=" + teiidModelName + "]";
    }
}
