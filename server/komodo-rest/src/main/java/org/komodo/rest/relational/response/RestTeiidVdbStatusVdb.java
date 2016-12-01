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
package org.komodo.rest.relational.response;

import java.util.List;
import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * Object to be serialised by GSON that encapsulates the status of a deployed vdb
 */
@JsonSerialize( include = Inclusion.NON_NULL )
public class RestTeiidVdbStatusVdb implements KRestEntity {

    /**
     * Label for the name
     */
    public static final String TEIID_VDB_STATUS_NAME = "name";

    /**
     * Label for the deployed name
     */
    public static final String TEIID_VDB_STATUS_DEPLOYED_NAME = "deployedName";

    /**
     * Label for the version
     */
    public static final String TEIID_VDB_STATUS_VERSION = "version";

    /**
     * Label for the active state
     */
    public static final String TEIID_VDB_STATUS_ACTIVE = "active";

    /**
     * Label for the loading state
     */
    public static final String TEIID_VDB_STATUS_LOADING = "loading";

    /**
     * Label for the failed state
     */
    public static final String TEIID_VDB_STATUS_FAILED = "failed";

    /**
     * Label for the errors list
     */
    public static final String TEIID_VDB_STATUS_ERROR = "errors";

    private String name;

    private String deployedName;

    private String version;

    private boolean active;

    private boolean failed;

    private boolean loading;

    private List<String> errors;

    /**
     * Default constructor for deserialization
     */
    public RestTeiidVdbStatusVdb() {
        // do nothing
    }

    public RestTeiidVdbStatusVdb(TeiidVdb vdb) {
        name = vdb.getName();
        deployedName = vdb.getDeployedName();
        version = vdb.getVersion();
        active = vdb.isActive();
        failed = vdb.hasFailed();
        loading = vdb.isLoading();
        errors = vdb.getValidityErrors();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDeployedName() {
        return deployedName;
    }

    public void setDeployedName(String deployedName) {
        this.deployedName = deployedName;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public boolean isFailed() {
        return failed;
    }

    public void setFailed(boolean failed) {
        this.failed = failed;
    }

    public boolean isLoading() {
        return loading;
    }

    public void setLoading(boolean loading) {
        this.loading = loading;
    }

    public List<String> getErrors() {
        return errors;
    }

    public void setErrors(List<String> errors) {
        this.errors = errors;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (active ? 1231 : 1237);
        result = prime * result + ((deployedName == null) ? 0 : deployedName.hashCode());
        result = prime * result + ((errors == null) ? 0 : errors.hashCode());
        result = prime * result + (failed ? 1231 : 1237);
        result = prime * result + (loading ? 1231 : 1237);
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((version == null) ? 0 : version.hashCode());
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
        RestTeiidVdbStatusVdb other = (RestTeiidVdbStatusVdb)obj;
        if (active != other.active)
            return false;
        if (deployedName == null) {
            if (other.deployedName != null)
                return false;
        } else if (!deployedName.equals(other.deployedName))
            return false;
        if (errors == null) {
            if (other.errors != null)
                return false;
        } else if (!errors.equals(other.errors))
            return false;
        if (failed != other.failed)
            return false;
        if (loading != other.loading)
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (version != other.version)
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "RestTeiidVdbStatusProps [name=" + name + ", deployedName=" + deployedName + ", version=" + version + ", active="
               + active + ", failed=" + failed + ", loading=" + loading + ", errors=" + errors + "]";
    }
}
