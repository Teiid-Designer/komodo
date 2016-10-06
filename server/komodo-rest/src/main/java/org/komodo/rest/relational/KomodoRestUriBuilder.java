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
package org.komodo.rest.relational;

import java.net.URI;
import java.util.Map;
import java.util.Properties;
import javax.ws.rs.core.UriBuilder;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Komodo REST URI builder.
 */
public final class KomodoRestUriBuilder implements KomodoRestV1Application.V1Constants {

    /**
     * Property names for the settings used in building Uris
     */
    public static enum SettingNames {
        /**
         * The path of the parent (stored as an URI)
         */
        PARENT_PATH,

        /**
         * The path of the parent of a vdb (stored as an URI)
         */
        VDB_PARENT_PATH,

        /**
         * The path of the parent of a dataservice (stored as an URI)
         */
        DATA_SERVICE_PARENT_PATH,

        /**
         * The path of the parent of a datasource (stored as an URI)
         */
        DATA_SOURCE_PARENT_PATH,

        /**
         * Name of the teiid object
         */
        TEIID_NAME,

        /**
         * Name of the vdb
         */
        VDB_NAME,

        /**
         * Name of the data service
         */
        DATA_SERVICE_NAME,

        /**
         * Name of the model
         */
        MODEL_NAME,

        /**
         * Name of the source
         */
        SOURCE_NAME,

        /**
         * Name of the translator
         */
        TRANSLATOR_NAME,

        /**
         * Indicates if adding Translators segment for VDB translators
         */
        ADD_TRANSLATORS_SEGMENT,

        /**
         * Name of the data source
         */
        DATA_SOURCE_NAME,

        /**
         * Name of the import
         */
        IMPORT_NAME,

        /**
         * Data role id
         */
        DATA_ROLE_ID,

        /**
         * Permission id
         */
        PERMISSION_ID,

        /**
         * Permission child id
         */
        PERMISSION_CHILD_ID,

        /**
         * Permission child type
         */
        PERMISSION_CHILD_TYPE;
    }

    private final URI baseUri;

    /**
     * @param baseUri
     *        the base URI used when building URIs (cannot be <code>null</code>)
     */
    public KomodoRestUriBuilder(final URI baseUri) {
        ArgCheck.isNotNull(baseUri, "baseUri"); //$NON-NLS-1$
        this.baseUri = baseUri;
    }

    private boolean isCachedTeiidFolder(UnitOfWork uow, KomodoObject kObject) throws KException {
        if (kObject == null)
            return false;

        Descriptor type = kObject.getPrimaryType(uow);
        if (type == null) return false;

        // Check this type is a folder, then check parent is CachedTeiid
        if(KomodoLexicon.Folder.NODE_TYPE.equals(type.getName())) {
            KomodoObject parentObj = kObject.getParent(uow);
            if(parentObj != null) {
                Descriptor parentType = parentObj.getPrimaryType(uow);
                return parentType!=null && KomodoLexicon.CachedTeiid.NODE_TYPE.equals(parentType.getName());
            }
        }
        return false;
    }

    private boolean isVdb(UnitOfWork uow, KomodoObject kObject) throws KException {
        if (kObject == null)
            return false;

        Descriptor type = kObject.getPrimaryType(uow);
        return VdbLexicon.Vdb.VIRTUAL_DATABASE.equals(type.getName());
    }

    private String setting(final Properties settings, SettingNames settingName) {
        String value = settings.getProperty(settingName.name());
        ArgCheck.isNotEmpty(value, settingName.name());
        return value;
    }

    /**
     * @param settings
     * @param defaultUri
     * @return the parent of the object
     */
    private URI parentUri(Properties settings) {
        Object uriObject = settings.get(SettingNames.PARENT_PATH.name());
        ArgCheck.isInstanceOf(URI.class, uriObject);
        return (URI) uriObject;
    }

    /**
     * @param settings
     * @return the parent of the VDB rather than the parent of the object
     */
    private URI vdbParentUri(Properties settings) {
        Object uriObject = settings.get(SettingNames.VDB_PARENT_PATH.name());
        ArgCheck.isInstanceOf(URI.class, uriObject);
        return (URI) uriObject;
    }

    private String vdbName(final Properties settings) {
        return setting(settings, SettingNames.VDB_NAME);
    }

    /**
     * @param parentUri the uri of the parent
     * @param dataserviceName the dataservice name
     * @param target the link type target
     * @return the URI for child objects of the given dataservice
     */
    public URI dataserviceChildGroupUri(URI parentUri, String dataserviceName, LinkType target) {
        return UriBuilder.fromUri(dataserviceUri(parentUri, dataserviceName))
                                   .path(target.uriName())
                                   .build();
    }

    /**
     * @param settings
     * @return the parent of the dataservice rather than the parent of the object
     */
    private URI dataserviceParentUri(Properties settings) {
        Object uriObject = settings.get(SettingNames.DATA_SERVICE_PARENT_PATH.name());
        ArgCheck.isInstanceOf(URI.class, uriObject);
        return (URI) uriObject;
    }

    /**
     * @param dataservice the dataservice
     * @param uow the transaction
     * @return the uri of the parent of the given data service
     * @throws KException if problem occurs
     */
    public URI dataserviceParentUri(Dataservice dataservice, UnitOfWork uow) throws KException {
        return workspaceDataservicesUri();
    }

    private String dataserviceName(final Properties settings) {
        return setting(settings, SettingNames.DATA_SERVICE_NAME);
    }

    private String modelName(final Properties settings) {
        return setting(settings, SettingNames.MODEL_NAME);
    }

    private String sourceName(final Properties settings) {
        return setting(settings, SettingNames.SOURCE_NAME);
    }

    private String dataRoleId(final Properties settings) {
        return setting(settings, SettingNames.DATA_ROLE_ID);
    }

    private String permissionId(final Properties settings) {
        return setting(settings, SettingNames.PERMISSION_ID);
    }

    /**
     * @param the parent URI of the vdb
     * @param parentVdb the parent vdb
     * @param groupLink the type of group the child belongs to
     * @param childName the name of the model
     * @param settings the settings for additional properties
     * @return the URI for the given model
     */
    private URI vdbChildUri(URI parentUri, String parentVdb, LinkType groupLink, String childName) {
        URI groupUri = vdbChildGroupUri(parentUri, parentVdb, groupLink);
        return UriBuilder.fromUri(groupUri).path(childName).build();
    }

    /**
     * @return the base uri
     */
    public URI baseUri() {
        return baseUri;
    }

    /**
     * @return the URI to use when requesting a collection of Dataservices in the workspace (never <code>null</code>)
     */
    public URI workspaceDataservicesUri() {
        return UriBuilder.fromUri(this.baseUri)
                                   .path(WORKSPACE_SEGMENT)
                                   .path(DATA_SERVICES_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting a collection of Datasources in the workspace (never <code>null</code>)
     */
    public URI workspaceDatasourcesUri() {
        return UriBuilder.fromUri(this.baseUri)
                                   .path(WORKSPACE_SEGMENT)
                                   .path(DATA_SOURCES_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting a collection of Drivers in the workspace (never <code>null</code>)
     */
    public URI workspaceDriversUri() {
        return UriBuilder.fromUri(this.baseUri)
                                   .path(WORKSPACE_SEGMENT)
                                   .path(DRIVERS_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting a collection of VDBs in the workspace (never <code>null</code>)
     */
    public URI workspaceVdbsUri() {
        return UriBuilder.fromUri(this.baseUri)
                                   .path(WORKSPACE_SEGMENT)
                                   .path(VDBS_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting the teiid cache  (never <code>null</code>)
     */
    public URI teiidCacheUri() {
        return UriBuilder.fromUri(this.baseUri)
                                     .path(TEIID_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting the teiid cache  (never <code>null</code>)
     */
    public URI teiidStatusUri() {
        return UriBuilder.fromUri(this.baseUri)
                                     .path(TEIID_SEGMENT)
                                     .path(STATUS_SEGMENT)
                                     .build();
    }

    /**
     * @return the URI to use when requesting the teiid cache  (never <code>null</code>)
     */
    public URI teiidVdbStatusUri() {
        return UriBuilder.fromUri(this.baseUri)
                                     .path(TEIID_SEGMENT)
                                     .path(STATUS_SEGMENT)
                                     .path(VDBS_SEGMENT)
                                     .build();
    }

    /**
     * @return the URI to use when requesting a specific cached teiid  (never <code>null</code>)
     */
    public URI cachedTeiidUri(String teiidId) {
        return UriBuilder.fromUri(teiidCacheUri())
                                     .path(teiidId).build();
    }

    /**
     * @return the URI to use when requesting a collection of VDBs in a teiid cache (never <code>null</code>)
     */
    public URI cacheTeiidVdbsUri(String teiidId) {
        return UriBuilder.fromUri(cachedTeiidUri(teiidId))
                                   .path(VDBS_SEGMENT).build();
    }

    /**
     * @param properties the search parameter properties
     * @return the URI to use when searching the workspace
     */
    public URI searchUri(KomodoProperties properties) {
        UriBuilder builder = UriBuilder.fromUri(this.baseUri)
                                    .path(WORKSPACE_SEGMENT)
                                    .path(SEARCH_SEGMENT);

        for (Map.Entry<String, Object> entry : properties.entrySet()) {
            builder.queryParam(entry.getKey(), entry.getValue().toString());
        }

        return builder.build();
    }

    /**
     * @param properties the search paramater properties
     * @return the URI to use where requesting the collection of saved searches
     */
    public URI savedSearchCollectionUri(KomodoProperties properties) {
        UriBuilder builder = UriBuilder.fromUri(this.baseUri)
                                                      .path(WORKSPACE_SEGMENT)
                                                      .path(SEARCH_SEGMENT)
                                                      .path(SAVED_SEARCHES_SEGMENT);

        for (Map.Entry<String, Object> entry : properties.entrySet()) {
            builder.queryParam(entry.getKey(), entry.getValue().toString());
        }

        return builder.build();
    }

    /**
     * @param the uri of the parent
     * @param vdbName the vdb name
     * @param target the link type target
     * @return the URI for child objects of the given vdb
     */
    public URI vdbChildGroupUri(URI parentUri, String vdbName, LinkType target) {
        return UriBuilder.fromUri(vdbUri(parentUri, vdbName))
                                   .path(target.uriName())
                                   .build();
    }

    /**
     * @param vdb
     * @param uow
     * @return the uri of the parent of the given vdb
     * @throws KException
     */
    public URI vdbParentUri(Vdb vdb, UnitOfWork uow) throws KException {
        KomodoObject parent = vdb.getParent(uow);
        if (isCachedTeiidFolder(uow, parent)) {
            KomodoObject cachedTeiid = parent.getParent(uow);
            return cacheTeiidVdbsUri(cachedTeiid.getName(uow));
        }

        return workspaceVdbsUri();
    }

    /**
     * @param vdbName the vdb name
     * @param settings the settings
     * @return the URI for the given vdb
     */
    public URI vdbUri(final URI parentUri, final String vdbName) {
        return UriBuilder.fromUri(parentUri).path(vdbName).build();
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbUri(final LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$

        URI parentUri = vdbParentUri(settings);
        URI result = null;

        switch (linkType) {
            case SELF:
            {
                String vdbName = vdbName(settings);
                result = vdbUri(parentUri, vdbName);
                break;
            }
            case PARENT:
            {
                result = parentUri;
                break;
            }
            case IMPORTS:
            case MODELS:
            case TRANSLATORS:
            case DATA_ROLES:
            {
                String vdbName = vdbName(settings);
                result = vdbChildGroupUri(parentUri, vdbName, linkType);
                break;
            }
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbModelUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String parentName = vdbName(settings);

        String modelName = modelName(settings);
        URI myModelUri = vdbChildUri(vdbBaseUri, parentName, LinkType.MODELS, modelName);

        switch (linkType) {
            case SELF:
                result = myModelUri;
                break;
            case PARENT:
                result = vdbUri(vdbBaseUri, parentName);
                break;
            case SOURCES:
                result = UriBuilder.fromUri(myModelUri).path(linkType.uriName()).build();
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB model source URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbModelSourceUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String vdbName = vdbName(settings);
        URI modelUri = vdbChildUri(vdbBaseUri, vdbName, LinkType.MODELS, modelName(settings));

        switch (linkType) {
            case SELF:
                String sourceName = sourceName(settings);
                result = UriBuilder.fromUri(modelUri).path(LinkType.SOURCES.uriName()).path(sourceName).build();
                break;
            case PARENT:
                result = modelUri;
                break;
            case REFERENCE:
                String translatorName = setting(settings, SettingNames.TRANSLATOR_NAME);
                result = vdbChildUri(vdbBaseUri, vdbName, LinkType.TRANSLATORS, translatorName);
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param translator
     * @param uow
     * @return the uri of the parent of the given translator
     * @throws KException
     */
    public URI vdbTranslatorParentUri(Translator translator, UnitOfWork uow) throws KException {
        KomodoObject parent = translator.getParent(uow);
        if (isCachedTeiidFolder(uow, parent)) {
            return cachedTeiidUri(parent.getName(uow));
        }
        else if (isVdb(uow, parent)) {
            String vdbName = parent.getName(uow);
            return vdbUri(workspaceVdbsUri(), vdbName);
        }

        throw new KException("Translator has an invalid parent");
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbTranslatorUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI parentUri = parentUri(settings);

        switch (linkType) {
            case SELF:
                String name = setting(settings, SettingNames.TRANSLATOR_NAME);
                // Adds translators segment if supplied.
                if(settings.containsKey(SettingNames.ADD_TRANSLATORS_SEGMENT.name())) {
                    result = UriBuilder.fromUri(parentUri)
                    .path(TRANSLATORS_SEGMENT)
                    .path(name).build();
                } else {
                    result = UriBuilder.fromUri(parentUri)
                    .path(name).build();
                }
                break;
            case PARENT:
                result = parentUri;
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbImportUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String parentVdb = vdbName(settings);

        switch (linkType) {
            case SELF:
                String importName = setting(settings, SettingNames.IMPORT_NAME);
                result = vdbChildUri(vdbBaseUri, parentVdb, LinkType.IMPORTS, importName);
                break;
            case PARENT:
                result = vdbUri(vdbBaseUri, parentVdb);
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB data role URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbDataRoleUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        URI myDataRoleUri = vdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);

        switch (linkType) {
            case SELF:
                result = myDataRoleUri;
                break;
            case PARENT:
                result = vdbUri(vdbBaseUri, parentVdb);
                break;
            case PERMISSIONS:
                result = UriBuilder.fromUri(myDataRoleUri).path(linkType.uriName()).build();
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB permission URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbPermissionUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);
        URI myDataRoleUri = vdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);
        URI myPermUri = UriBuilder.fromUri(myDataRoleUri)
                                                   .path(LinkType.PERMISSIONS.uriName())
                                                   .path(permissionId)
                                                   .build();

        switch (linkType) {
            case SELF:
                result = myPermUri;
                break;
            case PARENT:
                result = myDataRoleUri;
                break;
            case CONDITIONS:
            case MASKS:
                result = UriBuilder.fromUri(myPermUri).path(linkType.uriName()).build();
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the permission child URI for the specified VDB (never <code>null</code>)
     */
    public URI vdbPermissionChildUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = vdbParentUri(settings);
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);

        URI myDataRoleUri = vdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);
        URI myPermUri = UriBuilder.fromUri(myDataRoleUri)
                                                   .path(LinkType.PERMISSIONS.uriName())
                                                   .path(permissionId)
                                                   .build();

        switch (linkType) {
            case SELF:
                String childType = setting(settings, SettingNames.PERMISSION_CHILD_TYPE);
                String childId = setting(settings, SettingNames.PERMISSION_CHILD_ID);
                result = UriBuilder.fromUri(myPermUri)
                                            .path(childType)
                                            .path(childId)
                                            .build();
                break;
            case PARENT:
                result = myPermUri;
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param parentUri the parent URI
     * @param dataserviceName the dataservice name
     * @return the URI for the given dataservice
     */
    public URI dataserviceUri(final URI parentUri, final String dataserviceName) {
        return UriBuilder.fromUri(parentUri).path(dataserviceName).build();
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the dataservice URI for the specified dataservice (never <code>null</code>)
     */
    public URI dataserviceUri(final LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$

        URI parentUri = dataserviceParentUri(settings);
        URI result = null;

        switch (linkType) {
            case SELF:
            {
                String dataserviceName = dataserviceName(settings);
                result = dataserviceUri(parentUri, dataserviceName);
                break;
            }
            case PARENT:
            {
                result = parentUri;
                break;
            }
            case CONNECTIONS:
            case VDBS:
            {
                String dataserviceName = dataserviceName(settings);
                result = dataserviceChildGroupUri(parentUri, dataserviceName, linkType);
                break;
            }
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param data source
     * @param uow
     * @return the uri of the parent of the given data source
     * @throws KException
     */
    public URI dataSourceParentUri(Datasource dataSource, UnitOfWork uow) throws KException {
        KomodoObject parent = dataSource.getParent(uow);
        if (isCachedTeiidFolder(uow, parent)) {
            return cachedTeiidUri(parent.getName(uow));
        }

        return workspaceDatasourcesUri();
    }

    /**
     * @param linkType
     * @param settings
     * @return data source URIs for the given link type and settings
     */
    public URI dataSourceUri(LinkType linkType, Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI parentUri = parentUri(settings);

        switch (linkType) {
            case SELF:
                String name = setting(settings, SettingNames.DATA_SOURCE_NAME);
                result = UriBuilder.fromUri(parentUri)
                                   .path(name).build();
                break;
            case PARENT:
                result = parentUri;
                break;
            default:
                throw new RuntimeException("LinkType " + linkType + " not handled"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert(result != null);
        return result;
    }

    /**
     * @param settingName setting name
     * @param value the value
     * @return the new settings object
     */
    public Properties createSettings(SettingNames settingName, String value) {
        Properties properties = new Properties();
        properties.setProperty(settingName.name(), value);
        return properties;
    }

    /**
     * @param settings the settings
     * @param settingName setting name
     * @param value the value
     */
    public void addSetting(Properties settings, SettingNames settingName, String value) {
        settings.setProperty(settingName.name(), value);
    }

    /**
     * @param settings the settings
     * @param settingName setting name
     * @param value the value
     */
    public void addSetting(Properties settings, SettingNames settingName, URI value) {
        settings.put(settingName.name(), value);
    }
}
