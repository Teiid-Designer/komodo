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

import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;

/**
 * A {@link ConnectionJdbcInfo} that can be used by GSON to build a JSON document representation.
 */
public class RestConnectionJdbcInfo implements KRestEntity {

    /**
     * Label used to describe supports catalogs
     */
    public static final String SUPPORTS_CATALOGS_LABEL = "supportsCatalogs";

    /**
     * Label used to describe supports schemas
     */
    public static final String SUPPORTS_SCHEMAS_LABEL = "supportsSchemas";
    
    /**
     * Label used to describe readonly
     */
    public static final String READONLY_LABEL = "readonly";
    
    /**
     * Label used to describe product name
     */
    public static final String PRODUCT_NAME_LABEL = "productName";
    
    /**
     * Label used to describe product version
     */
    public static final String PRODUCT_VERSION_LABEL = "productVersion";
    
    /**
     * Label used to describe driver major version
     */
    public static final String DRIVER_MAJOR_VERSION_LABEL = "driverMajorVersion";
    
    /**
     * Label used to describe driver minor version
     */
    public static final String DRIVER_MINOR_VERSION_LABEL = "driverMinorVersion";
    
    /**
     * Label used to describe driver name
     */
    public static final String DRIVER_NAME_LABEL = "driverName";
    
    /**
     * Label used to describe database url
     */
    public static final String DATABASE_URL_LABEL = "databaseUrl";
    
    /**
     * Label used to describe username
     */
    public static final String USERNAME_LABEL = "username";


    private boolean supportsCatalogs;

    private boolean supportsSchemas;

    private boolean readonly;

    private String productName;

    private String productVersion;

    private int driverMajorVersion;

    private int driverMinorVersion;

    private String driverName;

    private String driverUrl;

    private String username;

    /**
     * Constructor for use when deserializing
     */
    public RestConnectionJdbcInfo() {
        super();
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    /**
     * @return the supportsCatalogs
     */
    public boolean isSupportsCatalogs() {
        return this.supportsCatalogs;
    }

    /**
     * @param supportsCatalogs the supportsCatalogs to set
     */
    public void setSupportsCatalogs(boolean supportsCatalogs) {
        this.supportsCatalogs = supportsCatalogs;
    }

    /**
     * @return the supportsSchemas
     */
    public boolean isSupportsSchemas() {
        return this.supportsSchemas;
    }

    /**
     * @param supportsSchemas the supportsSchemas to set
     */
    public void setSupportsSchemas(boolean supportsSchemas) {
        this.supportsSchemas = supportsSchemas;
    }

    /**
     * @return the readonly
     */
    public boolean isReadonly() {
        return this.readonly;
    }

    /**
     * @param readonly the readonly to set
     */
    public void setReadonly(boolean readonly) {
        this.readonly = readonly;
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return this.productName;
    }

    /**
     * @param productName the productName to set
     */
    public void setProductName(String productName) {
        this.productName = productName;
    }

    /**
     * @return the productVersion
     */
    public String getProductVersion() {
        return this.productVersion;
    }

    /**
     * @param productVersion the productVersion to set
     */
    public void setProductVersion(String productVersion) {
        this.productVersion = productVersion;
    }

    /**
     * @return the driverMajorVersion
     */
    public int getDriverMajorVersion() {
        return this.driverMajorVersion;
    }

    /**
     * @param driverMajorVersion the driverMajorVersion to set
     */
    public void setDriverMajorVersion(int driverMajorVersion) {
        this.driverMajorVersion = driverMajorVersion;
    }

    /**
     * @return the driverMinorVersion
     */
    public int getDriverMinorVersion() {
        return this.driverMinorVersion;
    }

    /**
     * @param driverMinorVersion the driverMinorVersion to set
     */
    public void setDriverMinorVersion(int driverMinorVersion) {
        this.driverMinorVersion = driverMinorVersion;
    }

    /**
     * @return the driverName
     */
    public String getDriverName() {
        return this.driverName;
    }

    /**
     * @param driverName the driverName to set
     */
    public void setDriverName(String driverName) {
        this.driverName = driverName;
    }

    /**
     * @return the driverUrl
     */
    public String getDriverUrl() {
        return this.driverUrl;
    }

    /**
     * @param driverUrl the driverUrl to set
     */
    public void setDriverUrl(String driverUrl) {
        this.driverUrl = driverUrl;
    }

    /**
     * @return the username
     */
    public String getUsername() {
        return this.username;
    }

    /**
     * @param username the username to set
     */
    public void setUsername(String username) {
        this.username = username;
    }

}
