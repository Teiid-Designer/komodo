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

import java.net.URI;
import java.util.Properties;

import org.komodo.relational.connection.Connection;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A connection that can be used by GSON to build a JSON document representation.
 */
public final class RestConnection extends RestBasicEntity {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.JNDI_NAME);

    /**
     * Label used to describe driver name
     */
    public static final String DRIVER_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.DRIVER_NAME);

    /**
     * Label used to describe jdbc
     */
    public static final String JDBC_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.TYPE);

    /**
     * An empty array of connections.
     */
    public static final RestConnection[] NO_CONNECTIONS = new RestConnection[0];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestConnection() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param connection the connection
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestConnection(URI baseUri, Connection connection, UnitOfWork uow) throws KException {
        super(baseUri, connection, uow, false);

        setJndiName(connection.getJndiName(uow));
        setDriverName(connection.getDriverName(uow));
        setJdbc(connection.isJdbc(uow));

        addExecutionProperties(uow, connection);

        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());
        URI parentUri = getUriBuilder().connectionParentUri(connection, uow);
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().connectionUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().connectionUri(LinkType.PARENT, settings)));
        createChildLink();
    }

    /**
     * @return the jndi name (can be empty)
     */
    public String getJndiName() {
        Object jndiName = tuples.get(JNDI_NAME_LABEL);
        return jndiName != null ? jndiName.toString() : null;
    }

    /**
     * @param jndiName
     *        the new jndi name (can be empty)
     */
    public void setJndiName(final String jndiName) {
        tuples.put(JNDI_NAME_LABEL, jndiName);
    }

    /**
     * @return the driver name (can be empty)
     */
    public String getDriverName() {
        Object driver = tuples.get(DRIVER_NAME_LABEL);
        return driver != null ? driver.toString() : null;
    }

    /**
     * @param driver
     *        the new driver
     */
    public void setDriverName(final String driver) {
        tuples.put(DRIVER_NAME_LABEL, driver);
    }

    /**
     * @return whether connection is JDBC
     */
    public boolean isJdbc() {
        Object jdbc = tuples.get(JDBC_LABEL);
        return jdbc != null ? Boolean.parseBoolean(jdbc.toString()) : false;
    }

    /**
     * @param jdbc
     */
    public void setJdbc(boolean jdbc) {
        tuples.put(JDBC_LABEL, jdbc);
    }

}
