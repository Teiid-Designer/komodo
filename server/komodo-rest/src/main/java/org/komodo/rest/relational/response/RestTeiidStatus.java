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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 * A Teiid Status object that can be used by GSON to build a JSON document representation.
 */
public final class RestTeiidStatus extends RestTeiid {

    /**
     * Label used to describe whether teiid instance is available
     */
    public static final String TEIID_INSTANCE_AVAILABLE_LABEL = "instanceAvailable";

    /**
     * Label used to describe the version of teiid used to build this
     */
    public static final String TEIID_BUILT_VERSION_LABEL = "builtVersion";

    /**
     * Label used to describe teiid connection url
     */
    public static final String TEIID_CONNECTION_URL_LABEL = "connectionUrl";

    /**
     * Label used to describe whether teiid is connected
     */
    public static final String TEIID_CONNECTED_LABEL = "connected";

    /**
     * Label used to describe the teiid connection error
     */
    public static final String TEIID_CONNECTION_ERROR_LABEL = "connectionError";

    /**
     * Label used to describe number of data sources
     */
    public static final String TEIID_DATA_SOURCE_SIZE_LABEL = "dataSourceSize";

    /**
     * Label used to describe the names of the available data sources
     */
    public static final String TEIID_DATA_SOURCE_NAMES_LABEL = "dataSourceNames";

    /**
     * Label used to describe number of data source drivers
     */
    public static final String TEIID_DATA_SOURCE_DRIVER_SIZE_LABEL = "dataSourceDriverSize";

    /**
     * Label used to describe the names of the available data source drivers
     */
    public static final String TEIID_DATA_SOURCE_DRIVERS_LABEL = "dataSourceDrivers";

    /**
     * Label used to describe number of translators
     */
    public static final String TEIID_TRANSLATOR_SIZE_LABEL = "translatorSize";

    /**
     * Label used to describe the names of the available translators
     */
    public static final String TEIID_TRANSLATOR_NAMES_LABEL = "translatorNames";

    /**
     * Label used to describe number of vdbs
     */
    public static final String TEIID_VDB_SIZE_LABEL = "vdbSize";

    /**
     * Label used to describe the names of the available vdbs
     */
    public static final String TEIID_VDB_NAMES_LABEL = "vdbNames";

    private List<RestConnectionDriver> sourceDrivers = new ArrayList<>();

    /**
     * Constructor for use when deserializing
     */
    public RestTeiidStatus() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param teiid the teiid object
     * @param uow the transaction
     *
     * @throws KException if error occurs
     */
    public RestTeiidStatus(URI baseUri, Teiid teiid, UnitOfWork uow) throws KException {
        super(baseUri, teiid, uow);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().teiidStatusUri()));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().cachedTeiidUri(teiid.getId(uow))));
        removeLink(LinkType.CHILDREN);

        setBuiltWithVersion(new DefaultTeiidVersion(super.getVersion()));

        synchronized (TeiidInstance.TEIID_INSTANCE_LOCK) {
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);
            setTeiidInstanceAvailable(teiidInstance != null);

            if (teiidInstance == null)
                return;

            try {
                teiidInstance.connect();

                setVersion(teiidInstance.getVersion().toString());
                setConnectionUrl(teiidInstance.getUrl());
                setConnected(teiidInstance.isConnected());
                setConnectionError(teiidInstance.getConnectionError());

                Collection<TeiidDataSource> dataSources = teiidInstance.getDataSources();
                setDataSourceSize(dataSources.size());
                setDataSourcesNames(dataSources);

                Collection<ConnectionDriver> dataSourceDrivers = teiidInstance.getDataSourceDrivers();
                setDataSourceDriverSize(dataSourceDrivers.size());
                if (dataSourceDrivers == null || dataSourceDrivers.isEmpty())
                    this.sourceDrivers = Collections.emptyList();
                else {
                    this.sourceDrivers = new ArrayList<>();
                    for (ConnectionDriver driver : dataSourceDrivers) {
                        this.sourceDrivers.add(new RestConnectionDriver(driver));
                    }
                }

                Collection<TeiidTranslator> translators = teiidInstance.getTranslators();
                setTranslatorSize(translators.size());
                setTranslatorNames(translators);

                Collection<TeiidVdb> vdbs = teiidInstance.getVdbs();
                setVdbSize(vdbs.size());
                setVdbNames(vdbs);
            } catch (Exception ex) {
                throw new KException(ex);
            }
        }
    }

    public String getBuiltWithVersion() {
        Object versionObj = tuples.get(TEIID_BUILT_VERSION_LABEL);
        return versionObj != null ? versionObj.toString() : null;
    }

    protected void setBuiltWithVersion(TeiidVersion teiidVersion) {
        tuples.put(TEIID_BUILT_VERSION_LABEL, teiidVersion.toString());
    }

    public boolean isTeiidInstanceAvailable() {
        Object hasTeiidInstance = tuples.get(TEIID_INSTANCE_AVAILABLE_LABEL);
        return hasTeiidInstance != null ? Boolean.parseBoolean(hasTeiidInstance.toString()) : false;
    }

    protected void setTeiidInstanceAvailable(boolean hasTeiidInstance) {
        tuples.put(TEIID_INSTANCE_AVAILABLE_LABEL, hasTeiidInstance);
    }

    public String getConnectionUrl() {
        Object url = tuples.get(TEIID_CONNECTION_URL_LABEL);
        return url != null ? url.toString() : null;
    }

    protected void setConnectionUrl(String url) {
        tuples.put(TEIID_CONNECTION_URL_LABEL, url);
    }

    public boolean isConnected() {
        Object connected = tuples.get(TEIID_CONNECTED_LABEL);
        return connected != null ? Boolean.parseBoolean(connected.toString()) : false;        
    }

    protected void setConnected(boolean connected) {
        tuples.put(TEIID_CONNECTED_LABEL, connected);
    }

    public String getConnectionError() {
        Object error = tuples.get(TEIID_CONNECTION_ERROR_LABEL);
        return error != null ? error.toString() : null;        
    }

    protected void setConnectionError(String connectionError) {
        tuples.put(TEIID_CONNECTION_ERROR_LABEL, connectionError);
    }

    public int getDataSourceSize() {
        Object size = tuples.get(TEIID_DATA_SOURCE_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setDataSourceSize(int size) {
        tuples.put(TEIID_DATA_SOURCE_SIZE_LABEL, size);
    }

    public String[] getDataSourceNames() {
        Object names = tuples.get(TEIID_DATA_SOURCE_NAMES_LABEL);
        return names != null ? (String[]) names : EMPTY_ARRAY;
    }

    protected void setDataSourcesNames(Collection<TeiidDataSource> dataSources) {
        List<String> names = new ArrayList<String>(dataSources.size());
        for (TeiidDataSource source : dataSources)
            names.add(source.getName());

        tuples.put(TEIID_DATA_SOURCE_NAMES_LABEL, names.toArray(new String[0]));
    }

    public int getDataSourceDriverSize() {
        Object size = tuples.get(TEIID_DATA_SOURCE_DRIVER_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setDataSourceDriverSize(int size) {
        tuples.put(TEIID_DATA_SOURCE_DRIVER_SIZE_LABEL, size);
    }

    public List<RestConnectionDriver> getDataSourceDrivers() {
        return this.sourceDrivers;
    }

    public void setDataSourceDrivers(RestConnectionDriver[] dataSourceDrivers) {
        if (dataSourceDrivers == null || dataSourceDrivers.length == 0)
            this.sourceDrivers = Collections.emptyList();

        this.sourceDrivers = new ArrayList<>();
        for (RestConnectionDriver driver : sourceDrivers) {
            this.sourceDrivers.add(driver);
        }
    }

    public int getTranslatorSize() {
        Object size = tuples.get(TEIID_TRANSLATOR_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setTranslatorSize(int size) {
        tuples.put(TEIID_TRANSLATOR_SIZE_LABEL, size);
    }

    public String[] getTranslatorNames() {
        Object names = tuples.get(TEIID_TRANSLATOR_NAMES_LABEL);
        return names != null ? (String[]) names : EMPTY_ARRAY;
    }

    protected void setTranslatorNames(Collection<TeiidTranslator> translators) {
        List<String> names = new ArrayList<String>(translators.size());
        for (TeiidTranslator tr : translators)
            names.add(tr.getName());

        tuples.put(TEIID_TRANSLATOR_NAMES_LABEL, names.toArray(new String[0]));
    }

    public int getVdbSize() {
        Object size = tuples.get(TEIID_VDB_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setVdbSize(int size) {
        tuples.put(TEIID_VDB_SIZE_LABEL, size);
    }

    public String[] getVdbNames() {
        Object names = tuples.get(TEIID_VDB_NAMES_LABEL);
        return names != null ? (String[]) names : EMPTY_ARRAY;
    }

    protected void setVdbNames(Collection<TeiidVdb> vdbs) {
        List<String> names = new ArrayList<String>(vdbs.size());
        for (TeiidVdb vdb : vdbs)
            names.add(vdb.getName());

        tuples.put(TEIID_VDB_NAMES_LABEL, names.toArray(new String[0]));
    }
}
