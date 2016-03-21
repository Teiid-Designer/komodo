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
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * Komodo REST URI builder.
 */
public final class KomodoRestUriBuilder implements KomodoRestV1Application.V1Constants {

    /**
     * Property names for the settings used in building Uris
     */
    public static enum SettingNames {
        /**
         * The path of the parent of a vdb (stored as a URI)
         */
        VDB_PARENT_PATH,

        /**
         * Name of the teiid object
         */
        TEIID_NAME,

        /**
         * Name of the vdb
         */
        VDB_NAME,

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

    private String setting(final Properties settings, SettingNames settingName) {
        String value = settings.getProperty(settingName.name());
        ArgCheck.isNotEmpty(value, settingName.name());
        return value;
    }

    private URI parentUri(Properties settings, URI defaultUri) {
        Object uriObject = settings.get(SettingNames.VDB_PARENT_PATH);
        URI parentUri = defaultUri;
        if (uriObject != null)
            parentUri = (URI) uriObject;
        return parentUri;
    }

    private String vdbName(final Properties settings) {
        return setting(settings, SettingNames.VDB_NAME);
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

    private URI buildVdbTranslatorUri(final Properties settings) {
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);
        String translatorName = setting(settings, SettingNames.TRANSLATOR_NAME);
        URI myTranslatorUri = generateVdbChildUri(vdbBaseUri, parentVdb, LinkType.TRANSLATORS, translatorName);
        return myTranslatorUri;
    }

    /**
     * @return the base uri
     */
    public URI baseUri() {
        return baseUri;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param settings
     *        configuration settings for this uri
     * @return the VDB URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbUri(final LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$

        URI parentUri = parentUri(settings, generateVdbsUri());
        URI result = null;

        switch (linkType) {
            case SELF:
            {
                String vdbName = vdbName(settings);
                result = generateVdbUri(parentUri, vdbName);
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
                result = generateVdbChildGroupUri(parentUri, vdbName, linkType);
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
    public URI buildVdbModelUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentName = vdbName(settings);

        String modelName = modelName(settings);
        URI myModelUri = generateVdbChildUri(vdbBaseUri, parentName, LinkType.MODELS, modelName);

        switch (linkType) {
            case SELF:
                result = myModelUri;
                break;
            case PARENT:
                result = generateVdbUri(vdbBaseUri, parentName);
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
    public URI buildVdbModelSourceUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        URI modelUri = generateVdbChildUri(vdbBaseUri, vdbName(settings), LinkType.MODELS, modelName(settings));

        switch (linkType) {
            case SELF:
                String sourceName = sourceName(settings);
                result = UriBuilder.fromUri(modelUri).path(LinkType.SOURCES.uriName()).path(sourceName).build();
                break;
            case PARENT:
                result = modelUri;
                break;
            case REFERENCE:
                result = buildVdbTranslatorUri(settings);
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
    public URI buildVdbTranslatorUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);

        switch (linkType) {
            case SELF:
                result = buildVdbTranslatorUri(settings);
                break;
            case PARENT:
                result = generateVdbUri(vdbBaseUri, parentVdb);
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
    public URI buildVdbImportUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);

        switch (linkType) {
            case SELF:
                String importName = setting(settings, SettingNames.IMPORT_NAME);
                result = generateVdbChildUri(vdbBaseUri, parentVdb, LinkType.IMPORTS, importName);
                break;
            case PARENT:
                result = generateVdbUri(vdbBaseUri, parentVdb);
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
    public URI buildVdbDataRoleUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        URI myDataRoleUri = generateVdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);

        switch (linkType) {
            case SELF:
                result = myDataRoleUri;
                break;
            case PARENT:
                result = generateVdbUri(vdbBaseUri, parentVdb);
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
    public URI buildVdbPermissionUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);
        URI myDataRoleUri = generateVdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);
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
    public URI buildVdbPermissionChildUri(LinkType linkType, final Properties settings) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotNull(settings, "settings"); //$NON-NLS-1$)

        URI result = null;
        URI vdbBaseUri = parentUri(settings, generateVdbsUri());
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);

        URI myDataRoleUri = generateVdbChildUri(vdbBaseUri, parentVdb, LinkType.DATA_ROLES, dataRoleId);
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
     * @return the URI to use when requesting a collection of VDBs in the workspace (never <code>null</code>)
     */
    public URI generateVdbsUri() {
        return UriBuilder.fromUri(this.baseUri)
                                   .path(WORKSPACE_SEGMENT)
                                   .path(VDBS_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting the teiid cache  (never <code>null</code>)
     */
    public URI generateTeiidCacheUri() {
        return UriBuilder.fromUri(this.baseUri)
                                     .path(TEIID_SEGMENT).build();
    }

    /**
     * @return the URI to use when requesting a specific cached teiid  (never <code>null</code>)
     */
    public URI generateCachedTeiidUri(String teiidId) {
        return UriBuilder.fromUri(generateTeiidCacheUri())
                                     .path(teiidId).build();
    }

    /**
     * @param properties the search parameter properties
     * @return the URI to use when searching the workspace
     */
    public URI generateSearchUri(KomodoProperties properties) {
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
    public URI generateSavedSearchCollectionUri(KomodoProperties properties) {
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
     * @param vdbName the vdb name
     * @param settings the settings
     * @return the URI for the given vdb
     */
    public URI generateVdbUri(final URI parentUri, final String vdbName) {
        return UriBuilder.fromUri(parentUri).path(vdbName).build();
    }

    /**
     * @param the uri of the parent
     * @param vdbName the vdb name
     * @param target the link type target
     * @return the URI for child objects of the given vdb
     */
    public URI generateVdbChildGroupUri(URI parentUri, String vdbName, LinkType target) {
        return UriBuilder.fromUri(generateVdbUri(parentUri, vdbName))
                                   .path(target.uriName())
                                   .build();
    }

    /**
     * @param the parent URI of the vdb
     * @param parentVdb the parent vdb
     * @param groupLink the type of group the child belongs to
     * @param childName the name of the model
     * @param settings the settings for additional properties
     * @return the URI for the given model
     */
    public URI generateVdbChildUri(URI parentUri, String parentVdb, LinkType groupLink, String childName) {
        URI groupUri = generateVdbChildGroupUri(parentUri, parentVdb, groupLink);
        return UriBuilder.fromUri(groupUri).path(childName).build();
    }

    /**
     * @param vdb
     * @param uow
     * @return the uri of the parent of the given vdb
     * @throws KException
     */
    public URI generateVdbParentUri(Vdb vdb, UnitOfWork uow) throws KException {
        KomodoObject parent = vdb.getParent(uow);
        URI parentUri;
        if (parent != null && KomodoType.CACHED_TEIID.equals(parent.getTypeIdentifier(uow))) {
            parentUri = UriBuilder.fromUri(generateCachedTeiidUri(parent.getName(uow)))
                                                   .path(VDBS_SEGMENT).build();
        }
        else
            parentUri = generateVdbsUri();

        return parentUri;
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
}
