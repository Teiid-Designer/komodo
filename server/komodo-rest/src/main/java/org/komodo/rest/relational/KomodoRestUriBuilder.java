/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import java.util.Map;
import javax.ws.rs.core.UriBuilder;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.utils.ArgCheck;

/**
 * Komodo REST URI builder.
 */
public final class KomodoRestUriBuilder implements KomodoRestV1Application.V1Constants {

    private final URI baseUri;

    /**
     * @param baseUri
     *        the base URI used when building URIs (cannot be <code>null</code>)
     */
    public KomodoRestUriBuilder(final URI baseUri) {
        ArgCheck.isNotNull(baseUri, "baseUri"); //$NON-NLS-1$
        this.baseUri = baseUri;
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
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @return the VDB URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbUri(final LinkType linkType, final String vdbName) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(vdbName, "vdbName"); //$NON-NLS-1$

        URI result = null;

        switch (linkType) {
            case SELF:
                result = generateVdbUri(vdbName);
                break;
            case PARENT:
                result = generateVdbsUri();
                break;
            case IMPORTS:
            case MODELS:
            case TRANSLATORS:
            case DATA_ROLES:
                result = generateVdbChildGroupUri(vdbName, linkType);
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param modelName
     *        the Vdb model name
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbModelUri(LinkType linkType, String parentVdb, String modelName) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(modelName, "modelName"); //$NON-NLS-1$

        URI result = null;
        URI vdbUri = generateVdbUri(parentVdb);
        URI myModelUri = generateVdbChildUri(parentVdb, LinkType.MODELS, modelName);

        switch (linkType) {
            case SELF:
                result = myModelUri;
                break;
            case PARENT:
                result = vdbUri;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param modelName
     *        the Vdb model name
     * @param sourceName
     *        the source name
     * @return the VDB model source URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbModelSourceUri(LinkType linkType, String parentVdb, String modelName, String sourceName) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(modelName, "modelName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(sourceName, "sourceName"); //$NON-NLS-1$

        URI result = null;
        URI modelUri = generateVdbChildUri(parentVdb, LinkType.MODELS, modelName);

        switch (linkType) {
            case SELF:
                result = UriBuilder.fromUri(modelUri).path(LinkType.SOURCES.uriName()).path(sourceName).build();
                break;
            case PARENT:
                result = modelUri;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param translatorName
     *        the Vdb translator name
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbTranslatorUri(LinkType linkType, String parentVdb, String translatorName) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(translatorName, "translatorName"); //$NON-NLS-1$

        URI result = null;
        URI vdbUri = generateVdbUri(parentVdb);
        URI myTranslatorUri = generateVdbChildUri(parentVdb, LinkType.TRANSLATORS, translatorName);

        switch (linkType) {
            case SELF:
                result = myTranslatorUri;
                break;
            case PARENT:
                result = vdbUri;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param importName
     *        the Vdb translator name
     * @return the VDB model URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbImportUri(LinkType linkType, String parentVdb, String importName) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(importName, "importName"); //$NON-NLS-1$

        URI result = null;
        URI vdbUri = generateVdbUri(parentVdb);
        URI myImportUri = generateVdbChildUri(parentVdb, LinkType.IMPORTS, importName);

        switch (linkType) {
            case SELF:
                result = myImportUri;
                break;
            case PARENT:
                result = vdbUri;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param dataRoleId
     *        the Vdb data role name
     * @return the VDB data role URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbDataRoleUri(LinkType linkType, String parentVdb, String dataRoleId) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataRoleId, "dataRoleId"); //$NON-NLS-1$

        URI result = null;
        URI vdbUri = generateVdbUri(parentVdb);
        URI myDataRoleUri = generateVdbChildUri(parentVdb, LinkType.DATA_ROLES, dataRoleId);

        switch (linkType) {
            case SELF:
                result = myDataRoleUri;
                break;
            case PARENT:
                result = vdbUri;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param dataRoleId
     *        the Vdb data role id
     * @param permissionId
     *        the Vdb permission id
     * @return the VDB permission URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbPermissionUri(LinkType linkType, String parentVdb, String dataRoleId, String permissionId) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataRoleId, "dataRoleId"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(permissionId, "permissionId"); //$NON-NLS-1$

        URI result = null;
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
     * @param parentVdb
     *        the parent VDB name (cannot be empty)
     * @param dataRoleId
     *        the Vdb data role id
     * @param permissionId
     *        the Vdb permission id
     * @param childType
     *        the type of the child
     * @param childId
     *        the child id of the permission
     * @return the permission child URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbPermissionChildUri(LinkType linkType, String parentVdb,
                                                                  String dataRoleId, String permissionId, LinkType childType, String childId) {
        ArgCheck.isNotNull(linkType, "linkType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataRoleId, "dataRoleId"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(permissionId, "permissionId"); //$NON-NLS-1$
        ArgCheck.isNotNull(childType, "childType"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(childId, "childId"); //$NON-NLS-1$

        URI result = null;
        URI myDataRoleUri = generateVdbChildUri(parentVdb, LinkType.DATA_ROLES, dataRoleId);
        URI myPermUri = UriBuilder.fromUri(myDataRoleUri)
                                                   .path(LinkType.PERMISSIONS.uriName())
                                                   .path(permissionId)
                                                   .build();

        switch (linkType) {
            case SELF:
                result = UriBuilder.fromUri(myPermUri)
                                            .path(childType.uriName())
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

}
