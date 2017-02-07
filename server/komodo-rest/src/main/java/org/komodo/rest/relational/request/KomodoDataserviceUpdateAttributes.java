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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
     * Label for the tablePath used for the update
     */
    public static final String DATASERVICE_TABLE_PATH_LABEL = "tablePath"; //$NON-NLS-1$

    /**
     * Label for the modelSourcePath used for the update
     */
    public static final String DATASERVICE_MODEL_SOURCE_PATH_LABEL = "modelSourcePath"; //$NON-NLS-1$

    /**
     * Label for the column names to include in the service view
     */
    public static final String DATASERVICE_COLUMN_NAMES_LABEL = "columnNames"; //$NON-NLS-1$

    /**
     * Label for the rhTablePath used for the update
     */
    public static final String DATASERVICE_RH_TABLE_PATH_LABEL = "rhTablePath"; //$NON-NLS-1$

    /**
     * Label for the rhModelSourcePath used for the update
     */
    public static final String DATASERVICE_RH_MODEL_SOURCE_PATH_LABEL = "rhModelSourcePath"; //$NON-NLS-1$

    /**
     * Label for the rh column names to include in the service view
     */
    public static final String DATASERVICE_RH_COLUMN_NAMES_LABEL = "rhColumnNames"; //$NON-NLS-1$

    /**
     * Label for the service view join type
     */
    public static final String DATASERVICE_JOIN_TYPE_LABEL = "joinType"; //$NON-NLS-1$

    /**
     * Label for the criteria predicates for the service join
     */
    public static final String DATASERVICE_CRITERIA_PREDICATES_LABEL = "criteriaPredicates"; //$NON-NLS-1$

    /**
     * Label for the service view ddl
     */
    public static final String DATASERVICE_VIEW_DDL_LABEL = "viewDdl"; //$NON-NLS-1$

    @JsonProperty(DATASERVICE_NAME_LABEL)
    private String dataserviceName;

    @JsonProperty(DATASERVICE_TABLE_PATH_LABEL)
    private String tablePath;

    @JsonProperty(DATASERVICE_MODEL_SOURCE_PATH_LABEL)
    private String modelSourcePath;

    @JsonProperty(DATASERVICE_COLUMN_NAMES_LABEL)
    private List<String> columnNames;

    @JsonProperty(DATASERVICE_RH_TABLE_PATH_LABEL)
    private String rhTablePath;

    @JsonProperty(DATASERVICE_RH_MODEL_SOURCE_PATH_LABEL)
    private String rhModelSourcePath;

    @JsonProperty(DATASERVICE_RH_COLUMN_NAMES_LABEL)
    private List<String> rhColumnNames;

    @JsonProperty(DATASERVICE_JOIN_TYPE_LABEL)
    private String joinType;
    
    @JsonProperty(DATASERVICE_CRITERIA_PREDICATES_LABEL)
    private List<Map<String,String>> criteriaPredicates;

    @JsonProperty(DATASERVICE_VIEW_DDL_LABEL)
    private String viewDdl;

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
     * @return table path
     */
    public String getTablePath() {
        return tablePath;
    }

    /**
     * @param tablePath the table path
     */
    public void setTablePath(String tablePath) {
        this.tablePath = tablePath;
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
    
    /**
     * @return the column names
     */
    public List<String> getColumnNames() {
        if (columnNames == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.columnNames);
    }

    /**
     * set column names
     * @param colNames the column names
     */
    public void setColumnNames(List<String> colNames) {
        if (this.columnNames == null)
            this.columnNames = new ArrayList<>();

        this.columnNames.addAll(colNames);
    }

    /**
     * @return rhTable path
     */
    public String getRhTablePath() {
        return rhTablePath;
    }

    /**
     * @param rhTablePath the rhTable path
     */
    public void setRhTablePath(String rhTablePath) {
        this.rhTablePath = rhTablePath;
    }

    /**
     * @return RH model source path
     */
    public String getRhModelSourcePath() {
        return rhModelSourcePath;
    }

    /**
     * @param rhModelSourcePath the rh source model path
     */
    public void setRhModelSourcePath(String rhModelSourcePath) {
        this.rhModelSourcePath = rhModelSourcePath;
    }
    
    /**
     * @return the rh column names
     */
    public List<String> getRhColumnNames() {
        if (rhColumnNames == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.rhColumnNames);
    }

    /**
     * set rh column names
     * @param rhColNames the rh column names
     */
    public void setRhColumnNames(List<String> rhColNames) {
        if (this.rhColumnNames == null)
            this.rhColumnNames = new ArrayList<>();

        this.rhColumnNames.addAll(rhColNames);
    }
    
    /**
     * @return join type
     */
    public String getJoinType() {
        return joinType;
    }

    /**
     * @param joinType the join type
     */
    public void setJoinType(String joinType) {
        this.joinType = joinType;
    }

    /**
     * @return the criteria predicates
     */
    public List<Map<String,String>> getCriteriaPredicates() {
        if (criteriaPredicates == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.criteriaPredicates);
    }

    /**
     * set the criteria predicates
     * @param predicates the predicates
     */
    public void setCriteriaPredicates(List<Map<String,String>> predicates) {
        if (this.criteriaPredicates == null)
            this.criteriaPredicates = new ArrayList<>();

        this.criteriaPredicates.addAll(predicates);
    }
    
    /**
     * @return view ddl
     */
    public String getViewDdl() {
        return viewDdl;
    }

    /**
     * @param ddl the view ddl
     */
    public void setViewDdl(String ddl) {
        this.viewDdl = ddl;
    }


    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dataserviceName == null) ? 0 : dataserviceName.hashCode());
        result = prime * result + ((tablePath == null) ? 0 : tablePath.hashCode());
        result = prime * result + ((modelSourcePath == null) ? 0 : modelSourcePath.hashCode());
        result = prime * result + ((columnNames == null) ? 0 : columnNames.hashCode());
        result = prime * result + ((rhTablePath == null) ? 0 : rhTablePath.hashCode());
        result = prime * result + ((rhModelSourcePath == null) ? 0 : rhModelSourcePath.hashCode());
        result = prime * result + ((rhColumnNames == null) ? 0 : rhColumnNames.hashCode());
        result = prime * result + ((joinType == null) ? 0 : joinType.hashCode());
        result = prime * result + ((criteriaPredicates == null) ? 0 : criteriaPredicates.hashCode());
        result = prime * result + ((viewDdl == null) ? 0 : viewDdl.hashCode());
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
        if (dataserviceName == null) {
            if (other.dataserviceName != null)
                return false;
        } else if (!dataserviceName.equals(other.dataserviceName))
            return false;
        if (tablePath == null) {
            if (other.tablePath != null)
                return false;
        } else if (!tablePath.equals(other.tablePath))
            return false;
        if (modelSourcePath == null) {
            if (other.modelSourcePath != null)
                return false;
        } else if (!modelSourcePath.equals(other.modelSourcePath))
            return false;
        if (columnNames == null) {
            if (other.columnNames != null)
                return false;
        } else if (!columnNames.equals(other.columnNames))
            return false;
        if (rhTablePath == null) {
            if (other.rhTablePath != null)
                return false;
        } else if (!rhTablePath.equals(other.rhTablePath))
            return false;
        if (rhModelSourcePath == null) {
            if (other.rhModelSourcePath != null)
                return false;
        } else if (!rhModelSourcePath.equals(other.rhModelSourcePath))
            return false;
        if (rhColumnNames == null) {
            if (other.rhColumnNames != null)
                return false;
        } else if (!rhColumnNames.equals(other.rhColumnNames))
            return false;
        if (joinType == null) {
            if (other.joinType != null)
                return false;
        } else if (!joinType.equals(other.joinType))
            return false;
        if (criteriaPredicates == null) {
            if (other.criteriaPredicates != null)
                return false;
        } else if (!criteriaPredicates.equals(other.criteriaPredicates))
            return false;
        if (viewDdl == null) {
            if (other.viewDdl != null)
                return false;
        } else if (!viewDdl.equals(other.viewDdl))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KomodoDataserviceUpdateAttributes [dataserviceName=" + dataserviceName + ", tablePath=" + tablePath + ", modelSourcePath=" + modelSourcePath);
        if(rhTablePath!=null) {
            sb.append(", rhTablePath =" + rhTablePath);
        }
        if(rhModelSourcePath!=null) {
            sb.append(", rhModelSourcePath =" + rhModelSourcePath);
        }
        if(columnNames!=null) {
            sb.append(", columnNames length =" + columnNames.size());
        }
        if(rhColumnNames!=null) {
            sb.append(", rhColumnNames length =" + rhColumnNames.size());
        }
        if(joinType!=null) {
            sb.append(", joinType =" + joinType);
        }
        if(criteriaPredicates!=null) {
            sb.append(", criteriaPredicates length =" + criteriaPredicates.size());
        }
        if(viewDdl!=null) {
            sb.append(", viewDdl =" + viewDdl);
        }
        sb.append("]");
        return sb.toString();
    }
}
