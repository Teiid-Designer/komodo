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
package org.komodo.rest.relational.dataservice;

import java.net.URI;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A Dataservice that can be used by GSON to build a JSON document representation.
 */
public final class RestDataservice extends RestBasicEntity {

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.encode(KomodoLexicon.LibraryComponent.DESCRIPTION);

    /**
     * Constructor for use when deserializing
     */
    public RestDataservice() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param dataService the dataService
     * @param exportXml whether xml should be exported
     * @param uow the transaction
     *
     * @throws KException if error occurs
     */
    public RestDataservice(URI baseUri, Dataservice dataService, boolean exportXml, UnitOfWork uow) throws KException {
        super(baseUri, dataService, uow, false);

        setDescription(dataService.getDescription(uow));

        addExecutionProperties(uow, dataService);

        Properties settings = getUriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, getId());
        URI parentUri = getUriBuilder().dataserviceParentUri(dataService, uow);
        getUriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, parentUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().dataserviceUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().dataserviceUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.VDBS, getUriBuilder().dataserviceUri(LinkType.VDBS, settings)));
    }
    
    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }


}
