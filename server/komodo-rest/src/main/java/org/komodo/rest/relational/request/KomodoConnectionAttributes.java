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
import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;


/**
 * Object to be serialised by GSON that encapsulates a connection object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoConnectionAttributes implements KRestEntity {

    /**
     * Label for the jndi
     */
    public static final String JNDI_LABEL = "jndiName"; //$NON-NLS-1$

    /**
     * Label for the jdbc
     */
    public static final String JDBC_LABEL = "jdbc"; //$NON-NLS-1$

    /**
     * Label for the driver
     */
    public static final String DRIVER_LABEL = "driverName"; //$NON-NLS-1$

    /**
     * Label for the parameters
     */
    public static final String PARAMETERS_LABEL = "parameters"; //$NON-NLS-1$

    @JsonProperty(JNDI_LABEL)
    private String jndi;

    @JsonProperty(JDBC_LABEL)
    private boolean jdbc;

    @JsonProperty(DRIVER_LABEL)
    private String driver;

    @JsonProperty(PARAMETERS_LABEL)
    private Map<String, Object> parameters;

    /**
     * Default constructor for deserialization
     */
    public KomodoConnectionAttributes() {
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
     * @return the jndi
     */
    public String getJndi() {
        return this.jndi;
    }

    /**
     * @param Jndi the Jndi to set
     */
    public void setJndi(String Jndi) {
        this.jndi = Jndi;
    }

    /**
     * @return the jdbc
     */
    public boolean isJdbc() {
        return this.jdbc;
    }

    /**
     * @param jdbc the jdbc to set
     */
    public void setJdbc(boolean jdbc) {
        this.jdbc = jdbc;
    }

    /**
     * @return the Driver
     */
    public String getDriver() {
        return this.driver;
    }

    /**
     * @param Driver the Driver to set
     */
    public void setDriver(String Driver) {
        this.driver = Driver;
    }

    /**
     * @return the parameters
     */
    public Map<String, Object> getParameters() {
        if (parameters == null)
            return Collections.emptyMap();

        return Collections.unmodifiableMap(this.parameters);
    }

    /**
     * Add a parameter with value
     * @param name the name
     * @param value the value
     */
    public void setParameter(String name, Object value) {
        if (this.parameters == null)
            this.parameters = new HashMap<>();

        this.parameters.put(name, value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((driver == null) ? 0 : driver.hashCode());
        result = prime * result + ((jndi == null) ? 0 : jndi.hashCode());
        result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
        result = prime * result + (jdbc ? 1231 : 1237);
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
        KomodoConnectionAttributes other = (KomodoConnectionAttributes)obj;
        if (driver == null) {
            if (other.driver != null)
                return false;
        } else if (!driver.equals(other.driver))
            return false;
        if (jndi == null) {
            if (other.jndi != null)
                return false;
        } else if (!jndi.equals(other.jndi))
            return false;
        if (parameters == null) {
            if (other.parameters != null)
                return false;
        } else if (!parameters.equals(other.parameters))
            return false;
        if (jdbc != other.jdbc)
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoConnectionAttributes [JndiId=" + this.jndi + ", jdbc=" + this.jdbc + ", Driver=" + this.driver + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    }
}
