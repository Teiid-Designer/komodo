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

import java.net.URI;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.teiid.Teiid;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 * A Teiid object that can be used by GSON to build a JSON document representation.
 */
public class RestTeiid extends RestBasicEntity {

    /**
     * Label used to describe teiid version
     */
    public static final String VERSION_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.VERSION);

    /**
     * Label used to describe teiid host property
     */
    public static final String HOST_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.HOST);

    /**
     * Label used to describe teiid admin port
     */
    public static final String ADMIN_PORT_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.ADMIN_PORT);

    /**
     * Label used to describe teiid admin password
     */
    public static final String ADMIN_PSWD_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.ADMIN_PSWD);

    /**
     * Label used to describe teiid admin user
     */
    public static final String ADMIN_USER_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.ADMIN_USER);

    /**
     * Label used to describe teiid admin secure flag
     */
    public static final String ADMIN_SECURE_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.ADMIN_SECURE);

    /**
     * Label used to describe teiid jdbc port
     */
    public static final String JDBC_PORT_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.JDBC_PORT);

    /**
     * Label used to describe teiid jdbc password
     */
    public static final String JDBC_PSWD_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.JDBC_PSWD);

    /**
     * Label used to describe teiid jdbc user
     */
    public static final String JDBC_USER_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.JDBC_USER);

    /**
     * Label used to describe teiid jdbc secure flag
     */
    public static final String JDBC_SECURE_LABEL = KomodoService.protectPrefix(KomodoLexicon.Teiid.JDBC_SECURE);

    /**
     * Constructor for use when deserializing
     */
    public RestTeiid() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param vdb the vdb
     * @param exportXml whether xml should be exported
     * @param uow the transaction
     *
     * @throws KException if error occurs
     */
    public RestTeiid(URI baseUri, Teiid teiid, UnitOfWork uow) throws KException {
        super(baseUri, teiid, uow);

        setHost(teiid.getHost(uow));
        TeiidVersion version = teiid.getVersion(uow);
        setVersion(version != null ? version.toString() : null);

        setAdminPort(teiid.getAdminPort(uow));
        setAdminPasswd(teiid.getAdminPassword(uow));
        setAdminUser(teiid.getAdminUser(uow));
        setAdminSecure(teiid.isAdminSecure(uow));

        setJdbcPort(teiid.getJdbcPort(uow));
        setJdbcPasswd(teiid.getJdbcPassword(uow));
        setJdbcUser(teiid.getJdbcUsername(uow));
        setJdbcSecure(teiid.isJdbcSecure(uow));
    }

    public String getHost() {
        Object host = tuples.get(HOST_LABEL);
        return host != null ? host.toString() : null;
    }

    public void setHost(String hostname) {
        tuples.put(HOST_LABEL, hostname);
    }

    /**
     * @return the version
     */
    public String getVersion() {
        Object version = tuples.get(VERSION_LABEL);
        return version != null ? version.toString() : null;
    }

    public void setVersion(String version) {
        tuples.put(VERSION_LABEL, version);
    }

    public int getAdminPort() {
        Object adminPort = tuples.get(ADMIN_PORT_LABEL);
        return adminPort != null ?
                Integer.parseInt(adminPort.toString()) : TeiidAdminInfo.Util.defaultPort(getVersion());
    }

    public void setAdminPort(int adminPort) {
        tuples.put(ADMIN_PORT_LABEL, adminPort);
    }

    public String getAdminPasswd() {
        Object adminPswd = tuples.get(ADMIN_PSWD_LABEL);
        return adminPswd != null ? adminPswd.toString() : null;
    }

    public void setAdminPasswd(String adminPassword) {
        tuples.put(ADMIN_PSWD_LABEL, adminPassword);
    }

    public String getAdminUser() {
        Object adminUser = tuples.get(ADMIN_USER_LABEL);
        return adminUser != null ? adminUser.toString() : null;
    }

    public void setAdminUser(String adminUser) {
        tuples.put(ADMIN_USER_LABEL, adminUser);
    }

    public boolean isAdminSecure() {
        Object adminSecure = tuples.get(ADMIN_SECURE_LABEL);
        return adminSecure != null ? Boolean.parseBoolean(adminSecure.toString()) : false;
    }

    public void setAdminSecure(boolean adminSecure) {
        tuples.put(ADMIN_SECURE_LABEL, adminSecure);
    }

    public int getJdbcPort() {
        Object jdbcPort = tuples.get(JDBC_PORT_LABEL);
        return jdbcPort != null ? Integer.parseInt(jdbcPort.toString()) : TeiidJdbcInfo.DEFAULT_PORT;
    }

    public void setJdbcPort(int jdbcPort) {
        tuples.put(JDBC_PORT_LABEL, jdbcPort);
    }

    public String getJdbcPasswd() {
        Object jdbcPswd = tuples.get(JDBC_PSWD_LABEL);
        return jdbcPswd != null ? jdbcPswd.toString() : null;
    }

    public void setJdbcPasswd(String jdbcPassword) {
        tuples.put(JDBC_PSWD_LABEL, jdbcPassword);
    }

    public String getJdbcUser() {
        Object jdbcPswd = tuples.get(JDBC_USER_LABEL);
        return jdbcPswd != null ? jdbcPswd.toString() : null;
    }

    public void setJdbcUser(String jdbcUser) {
        tuples.put(JDBC_USER_LABEL, jdbcUser);
    }

    public boolean isJdbcSecure() {
        Object jdbcSecure = tuples.get(JDBC_SECURE_LABEL);
        return jdbcSecure != null ? Boolean.parseBoolean(jdbcSecure.toString()) : false;
    }

    public void setJdbcSecure(boolean jdbcSecure) {
        tuples.put(JDBC_SECURE_LABEL, jdbcSecure);
    }
}
