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
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.Vdb;
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
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(KomodoLexicon.LibraryComponent.DESCRIPTION);

    /**
     * Label used to describe dataservice view modelName
     */
    public static final String DATASERVICE_VIEW_MODEL_LABEL = "serviceViewModel"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice viewName
     */
    public static final String DATASERVICE_VIEW_LABEL = "serviceView"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice vdbName
     */
    public static final String DATASERVICE_VDB_NAME_LABEL = "serviceVdbName"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice vdbVersion
     */
    public static final String DATASERVICE_VDB_VERSION_LABEL = "serviceVdbVersion"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice driver total
     */
    public static final String DATASERVICE_DRIVER_TOTAL_LABEL = "drivers"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice connection total
     */
    public static final String DATASERVICE_CONNECTION_TOTAL_LABEL = "connections"; //$NON-NLS-1$

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

        Vdb serviceVdb = dataService.getServiceVdb(uow);
        if (serviceVdb != null) {
            setServiceVdbName(serviceVdb.getVdbName( uow ));
            setServiceVdbVersion(Integer.toString(serviceVdb.getVersion( uow )));
            setServiceViewModel(dataService.getServiceViewModelName(uow));
            setServiceViewName(dataService.getServiceViewName(uow));
        }

        Datasource[] connections = dataService.getConnections(uow);
        setConnectionTotal(connections != null ? connections.length : 0);

        Driver[] drivers = dataService.getDrivers(uow);
        setDriverTotal(drivers != null ? drivers.length : 0);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().dataserviceUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().dataserviceUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.VDBS, getUriBuilder().dataserviceUri(LinkType.VDBS, settings)));
        addLink(new RestLink(LinkType.CONNECTIONS, getUriBuilder().dataserviceUri(LinkType.CONNECTIONS, settings)));
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

    /**
     * @return the service view model name (can be empty)
     */
    public String getServiceViewModel() {
        Object modelName = tuples.get(DATASERVICE_VIEW_MODEL_LABEL);
        return modelName != null ? modelName.toString() : null;
    }

    /**
     * @param modelName the view model name to set
     */
    public void setServiceViewModel(String modelName) {
        tuples.put(DATASERVICE_VIEW_MODEL_LABEL, modelName);
    }

    /**
     * @return the service view name (can be empty)
     */
    public String getServiceViewName() {
        Object viewName = tuples.get(DATASERVICE_VIEW_LABEL);
        return viewName != null ? viewName.toString() : null;
    }

    /**
     * @param viewName the service view name to set
     */
    public void setServiceViewName(String viewName) {
        tuples.put(DATASERVICE_VIEW_LABEL, viewName);
    }

    /**
     * @return the service vdb name (can be empty)
     */
    public String getServiceVdbName() {
        Object serviceVdbName = tuples.get(DATASERVICE_VDB_NAME_LABEL);
        return serviceVdbName != null ? serviceVdbName.toString() : null;
    }

    /**
     * @param serviceVdbName the service vdb name to set
     */
    public void setServiceVdbName(String serviceVdbName) {
        tuples.put(DATASERVICE_VDB_NAME_LABEL, serviceVdbName);
    }

    /**
     * @return the service vdb version (can be empty)
     */
    public String getServiceVdbVersion() {
        Object version = tuples.get(DATASERVICE_VDB_VERSION_LABEL);
        return version != null ? version.toString() : "1"; //$NON-NLS-1$
    }

    /**
     * @param version the version to set
     */
    public void setServiceVdbVersion( final String version) {
        tuples.put(DATASERVICE_VDB_VERSION_LABEL, version);
    }

    /**
     * @return the number of connections (can be empty)
     */
    public int getConnectionTotal() {
        Object total = tuples.get(DATASERVICE_CONNECTION_TOTAL_LABEL);
        return total != null ? Integer.parseInt(total.toString()) : 0;
    }

    /**
     * @param set the number of connections
     */
    public void setConnectionTotal(int total) {
        tuples.put(DATASERVICE_CONNECTION_TOTAL_LABEL, total);
    }

    /**
     * @return the number of drivers (can be empty)
     */
    public int getDriverTotal() {
        Object total = tuples.get(DATASERVICE_DRIVER_TOTAL_LABEL);
        return total != null ? Integer.parseInt(total.toString()) : 0;
    }

    /**
     * @param set the number of drivers
     */
    public void setDriverTotal(int total) {
        tuples.put(DATASERVICE_DRIVER_TOTAL_LABEL, total);
    }
}
