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
 * Object to be serialised by GSON that encapsulates properties for Dataservice update request
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoDataserviceUpdateAttributes implements KRestEntity {

    /**
     * Label for the DataService name
     */
    public static final String DATASERVICE_NAME_LABEL = "dataserviceName"; //$NON-NLS-1$

    /**
     * Label for the viewTablePath used for the update
     */
    public static final String DATASERVICE_VIEW_TABLE_PATH_LABEL = "viewTablePath"; //$NON-NLS-1$

    /**
     * Label for the modelSourcePath used for the update
     */
    public static final String DATASERVICE_MODEL_SOURCE_PATH_LABEL = "modelSourcePath"; //$NON-NLS-1$

    @JsonProperty(DATASERVICE_NAME_LABEL)
    private String dataserviceName;

    @JsonProperty(DATASERVICE_VIEW_TABLE_PATH_LABEL)
    private String viewTablePath;

    @JsonProperty(DATASERVICE_MODEL_SOURCE_PATH_LABEL)
    private String modelSourcePath;

    /**
     * Default constructor for deserialization
     */
    public KomodoDataserviceUpdateAttributes() {
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
     * @return dataserviceName
     */
    public String getDataserviceName() {
        return dataserviceName;
    }

    /**
     * @param dataserviceName the dataservice name
     */
    public void setDataserviceName(String dataserviceName) {
        this.dataserviceName = dataserviceName;
    }

    /**
     * @return view table path
     */
    public String getViewTablePath() {
        return viewTablePath;
    }

    /**
     * @param viewTablePath the view table path
     */
    public void setViewTablePath(String viewTablePath) {
        this.viewTablePath = viewTablePath;
    }

    /**
     * @return model source path
     */
    public String getModelSourcePath() {
        return modelSourcePath;
    }

    /**
     * @param modelSourcePath the source model path
     */
    public void setModelSourcePath(String modelSourcePath) {
        this.modelSourcePath = modelSourcePath;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((viewTablePath == null) ? 0 : viewTablePath.hashCode());
        result = prime * result + ((modelSourcePath == null) ? 0 : modelSourcePath.hashCode());
        result = prime * result + ((dataserviceName == null) ? 0 : dataserviceName.hashCode());
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
        KomodoDataserviceUpdateAttributes other = (KomodoDataserviceUpdateAttributes)obj;
        if (viewTablePath == null) {
            if (other.viewTablePath != null)
                return false;
        } else if (!viewTablePath.equals(other.viewTablePath))
            return false;
        if (modelSourcePath == null) {
            if (other.modelSourcePath != null)
                return false;
        } else if (!modelSourcePath.equals(other.modelSourcePath))
            return false;
        if (dataserviceName == null) {
            if (other.dataserviceName != null)
                return false;
        } else if (!dataserviceName.equals(other.dataserviceName))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoDataserviceUpdateAttributes [dataserviceName=" + dataserviceName + ", viewTablePath=" + viewTablePath + ", modelSourcePath=" + modelSourcePath + "]";
    }
}
