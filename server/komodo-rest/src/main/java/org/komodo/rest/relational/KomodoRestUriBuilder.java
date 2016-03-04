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
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink.LinkType;
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
        String parentVdb = vdbName(settings);
        String translatorName = setting(settings, SettingNames.TRANSLATOR_NAME);
        URI myTranslatorUri = generateVdbChildUri(parentVdb, LinkType.TRANSLATORS, translatorName);
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

        URI result = null;

        switch (linkType) {
            case SELF:
            {
                String vdbName = vdbName(settings);
                result = generateVdbUri(vdbName);
                break;
            }
            case PARENT:
            {
                result = generateVdbsUri();
                break;
            }
            case IMPORTS:
            case MODELS:
            case TRANSLATORS:
            case DATA_ROLES:
            {
                String vdbName = vdbName(settings);
                result = generateVdbChildGroupUri(vdbName, linkType);
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
        String parentName = vdbName(settings);

        String modelName = modelName(settings);
        URI myModelUri = generateVdbChildUri(parentName, LinkType.MODELS, modelName);

        switch (linkType) {
            case SELF:
                result = myModelUri;
                break;
            case PARENT:
                result = generateVdbUri(parentName);
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
        URI modelUri = generateVdbChildUri(vdbName(settings), LinkType.MODELS, modelName(settings));

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
        String parentVdb = vdbName(settings);

        switch (linkType) {
            case SELF:
                result = buildVdbTranslatorUri(settings);
                break;
            case PARENT:
                result = generateVdbUri(parentVdb);
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

        String parentVdb = vdbName(settings);

        switch (linkType) {
            case SELF:
                String importName = setting(settings, SettingNames.IMPORT_NAME);
                result = generateVdbChildUri(parentVdb, LinkType.IMPORTS, importName);
                break;
            case PARENT:
                result = generateVdbUri(parentVdb);
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

        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        URI myDataRoleUri = generateVdbChildUri(parentVdb, LinkType.DATA_ROLES, dataRoleId);

        switch (linkType) {
            case SELF:
                result = myDataRoleUri;
                break;
            case PARENT:
                result = generateVdbUri(parentVdb);
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
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);
        URI myDataRoleUri = generateVdbChildUri(parentVdb, LinkType.DATA_ROLES, dataRoleId);
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
        String parentVdb = vdbName(settings);
        String dataRoleId = dataRoleId(settings);
        String permissionId = permissionId(settings);

        URI myDataRoleUri = generateVdbChildUri(parentVdb, LinkType.DATA_ROLES, dataRoleId);
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
     * @return the URI for the given vdb
     */
    public URI generateVdbUri(final String vdbName) {
        return UriBuilder.fromUri(generateVdbsUri()).path(vdbName).build();
    }

    /**
     * @param vdbName the vdb name
     * @param target the link type target
     * @return the URI for child objects of the given vdb
     */
    public URI generateVdbChildGroupUri(String vdbName, LinkType target) {
        return UriBuilder.fromUri(generateVdbUri(vdbName))
                                   .path(target.uriName())
                                   .build();
    }

    /**
     * @param parentVdb the parent vdb
     * @param groupLink the type of group the child belongs to
     * @param childName the name of the model
     * @return the URI for the given model
     */
    public URI generateVdbChildUri(String parentVdb, LinkType groupLink, String childName) {
        URI groupUri = generateVdbChildGroupUri(parentVdb, groupLink);
        return UriBuilder.fromUri(groupUri).path(childName).build();
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
