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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;


/**
 * Object to be serialised by GSON that encapsulates a storage type and parameters object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoStorageAttributes implements KRestEntity {

    /**
     * Label for the storage type
     */
    public static final String STORAGE_TYPE_LABEL = "storageType"; //$NON-NLS-1$

    /**
     * Label for the data path
     */
    public static final String ARTIFACT_PATH_LABEL = "dataPath"; //$NON-NLS-1$

    /**
     * Label for the parameters
     */
    public static final String PARAMETERS_LABEL = "parameters"; //$NON-NLS-1$

    @JsonProperty(STORAGE_TYPE_LABEL)
    private String storageType;

    @JsonProperty(ARTIFACT_PATH_LABEL)
    private String artifactPath;

    @JsonProperty(PARAMETERS_LABEL)
    private Map<String, String> parameters;

    /**
     * Default constructor for deserialization
     */
    public KomodoStorageAttributes() {
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
     * @return the type
     */
    public String getStorageType() {
        return this.storageType;
    }

    /**
     * @param storageType the type to set
     */
    public void setStorageType(String storageType) {
        this.storageType = storageType;
    }

    /**
     * @return the artifact path
     */
    public String getArtifactPath() {
        return this.artifactPath;
    }

    /**
     * @param artifactPath the artifact path to set
     */
    public void setArtifactPath(String artifactPath) {
        this.artifactPath = artifactPath;
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
     * @return {@link Properties} instance of the parameters
     */
    public Properties convertParameters() {
        Properties props = new Properties();
        if (this.parameters == null)
            return props;

        for (Map.Entry<String, String> entry : this.parameters.entrySet())
            props.setProperty(entry.getKey(), entry.getValue());

        return props;
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
        int result = 1;
        result = prime * result + ((artifactPath == null) ? 0 : artifactPath.hashCode());
        result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
        result = prime * result + ((storageType == null) ? 0 : storageType.hashCode());
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
        KomodoStorageAttributes other = (KomodoStorageAttributes)obj;
        if (artifactPath == null) {
            if (other.artifactPath != null)
                return false;
        } else if (!artifactPath.equals(other.artifactPath))
            return false;
        if (parameters == null) {
            if (other.parameters != null)
                return false;
        } else if (!parameters.equals(other.parameters))
            return false;
        if (storageType == null) {
            if (other.storageType != null)
                return false;
        } else if (!storageType.equals(other.storageType))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoStorageAttributes [storageType=" + storageType + ", artifactPath=" + artifactPath + ", parameters="
               + parameters + "]";
    }
}
