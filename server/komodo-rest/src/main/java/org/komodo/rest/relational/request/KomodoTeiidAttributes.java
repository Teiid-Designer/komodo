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
 * Object to be serialised by GSON that encapsulates a Teiid credential object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoTeiidAttributes implements KRestEntity {

    /**
     * Label for the admin user name
     */
    public static final String ADMIN_USER_LABEL = "adminUser"; //$NON-NLS-1$

    /**
     * Label for the admin password
     */
    public static final String ADMIN_PASSWD_LABEL = "adminPasswd"; //$NON-NLS-1$

    /**
     * Label for the admin port
     */
    public static final String ADMIN_PORT_LABEL = "adminPort"; //$NON-NLS-1$

    /**
     * Label for the admin secure flag
     */
    public static final String ADMIN_SECURE_LABEL = "adminSecure"; //$NON-NLS-1$

    /**
     * Label for the jdbc user name
     */
    public static final String JDBC_USER_LABEL = "jdbcUser"; //$NON-NLS-1$

    /**
     * Label for the jdbc password
     */
    public static final String JDBC_PASSWD_LABEL = "jdbcPasswd"; //$NON-NLS-1$

    /**
     * Label for the jdbc port
     */
    public static final String JDBC_PORT_LABEL = "jdbcPort"; //$NON-NLS-1$

    /**
     * Label for the jdbc secure flag
     */
    public static final String JDBC_SECURE_LABEL = "jdbcSecure"; //$NON-NLS-1$

    @JsonProperty(ADMIN_USER_LABEL)
    private String adminUser;

    @JsonProperty(ADMIN_PASSWD_LABEL)
    private String adminPasswd;

    @JsonProperty(ADMIN_PORT_LABEL)
    private int adminPort = -1;

    @JsonProperty(ADMIN_SECURE_LABEL)
    private Boolean adminSecure;

    @JsonProperty(JDBC_USER_LABEL)
    private String jdbcUser;

    @JsonProperty(JDBC_PASSWD_LABEL)
    private String jdbcPasswd;

    @JsonProperty(JDBC_PORT_LABEL)
    private int jdbcPort = -1;

    @JsonProperty(JDBC_SECURE_LABEL)
    private Boolean jdbcSecure;

    /**
     * Default constructor for deserialization
     */
    public KomodoTeiidAttributes() {
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
     * @return admin user
     */
    public String getAdminUser() {
        return adminUser;
    }

    /**
     * @param adminUser
     */
    public void setAdminUser(String adminUser) {
        this.adminUser = adminUser;
    }

    /**
     * @return admin password
     */
    public String getAdminPasswd() {
        return adminPasswd;
    }

    /**
     * @param adminPasswd
     */
    public void setAdminPasswd(String adminPasswd) {
        this.adminPasswd = adminPasswd;
    }

    /**
     * @return admin port
     */
    public int getAdminPort() {
        return adminPort;
    }

    /**
     * @param adminPort
     */
    public void setAdminPort(int adminPort) {
        this.adminPort = adminPort;
    }

    /**
     * @return admin secure
     */
    public Boolean isAdminSecure() {
        return adminSecure;
    }

    /**
     * @param adminSecure
     */
    public void setAdminSecure(boolean adminSecure) {
        this.adminSecure = adminSecure;
    }

    /**
     * @return jdbc user
     */
    public String getJdbcUser() {
        return jdbcUser;
    }

    /**
     * @param jdbcUser
     */
    public void setJdbcUser(String jdbcUser) {
        this.jdbcUser = jdbcUser;
    }

    /**
     * @return jdbc password
     */
    public String getJdbcPasswd() {
        return jdbcPasswd;
    }

    /**
     * @param jdbcPasswd
     */
    public void setJdbcPasswd(String jdbcPasswd) {
        this.jdbcPasswd = jdbcPasswd;
    }

    /**
     * @return jdbc port
     */
    public int getJdbcPort() {
        return jdbcPort;
    }

    /**
     * @param jdbcPort
     */
    public void setJdbcPort(int jdbcPort) {
        this.jdbcPort = jdbcPort;
    }

    /**
     * @return jdbc secure
     */
    public Boolean isJdbcSecure() {
        return jdbcSecure;
    }

    /**
     * @param jdbcSecure
     */
    public void setJdbcSecure(boolean jdbcSecure) {
        this.jdbcSecure = jdbcSecure;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((adminPasswd == null) ? 0 : adminPasswd.hashCode());
        result = prime * result + adminPort;
        result = prime * result + ((adminSecure == null) ? 0 : adminSecure.hashCode());
        result = prime * result + ((adminUser == null) ? 0 : adminUser.hashCode());
        result = prime * result + ((jdbcPasswd == null) ? 0 : jdbcPasswd.hashCode());
        result = prime * result + jdbcPort;
        result = prime * result + ((jdbcSecure == null) ? 0 : jdbcSecure.hashCode());
        result = prime * result + ((jdbcUser == null) ? 0 : jdbcUser.hashCode());
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
        KomodoTeiidAttributes other = (KomodoTeiidAttributes)obj;
        if (adminPasswd == null) {
            if (other.adminPasswd != null)
                return false;
        } else if (!adminPasswd.equals(other.adminPasswd))
            return false;
        if (adminPort != other.adminPort)
            return false;
        if (adminSecure == null) {
            if (other.adminSecure != null)
                return false;
        } else if (!adminSecure.equals(other.adminSecure))
            return false;
        if (adminUser == null) {
            if (other.adminUser != null)
                return false;
        } else if (!adminUser.equals(other.adminUser))
            return false;
        if (jdbcPasswd == null) {
            if (other.jdbcPasswd != null)
                return false;
        } else if (!jdbcPasswd.equals(other.jdbcPasswd))
            return false;
        if (jdbcPort != other.jdbcPort)
            return false;
        if (jdbcSecure == null) {
            if (other.jdbcSecure != null)
                return false;
        } else if (!jdbcSecure.equals(other.jdbcSecure))
            return false;
        if (jdbcUser == null) {
            if (other.jdbcUser != null)
                return false;
        } else if (!jdbcUser.equals(other.jdbcUser))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoTeiidAttributes [adminUser=" + adminUser + ", adminPasswd=" + adminPasswd + ", adminSecure=" + adminSecure
               + ", jdbcUser=" + jdbcUser + ", jdbcPasswd=" + jdbcPasswd + ", jdbcSecure=" + jdbcSecure + "]";
    }
}
