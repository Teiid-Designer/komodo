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
import java.util.Properties;

import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A VDB model table that can be used by GSON to build a JSON model source representation.
 */
public final class RestVdbModelTable extends RestBasicEntity {

    /**
     * Label used to describe cardinality
     */
    public static final String CARDINALITY_LABEL = "Cardinality"; //$NON-NLS-1$

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModelTable() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param table the table
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbModelTable(URI baseUri, Table table, UnitOfWork uow) throws KException {
        super(baseUri, table, uow, false);

        setCardinality(table.getCardinality(uow));
        
        Model model = ancestor(table, Model.class, uow);
        ArgCheck.isNotNull(model);
        String modelName = model.getName(uow);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.MODEL_NAME, modelName);
        getUriBuilder().addSetting(settings, SettingNames.TABLE_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbModelTableUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbModelTableUri(LinkType.PARENT, settings)));
        createChildLink();
    }

    /**
     * @return the table cardinality
     */
    public long getCardinality() {
        Object adminPort = tuples.get(CARDINALITY_LABEL);
        return adminPort != null ? Long.parseLong(adminPort.toString()) : 1;
    }

    /**
     * @param cardinality the table cardinality
     */
    public void setCardinality(long cardinality) {
        tuples.put(CARDINALITY_LABEL, cardinality);
    }

}
