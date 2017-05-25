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
package org.komodo.rest.service;

import static org.komodo.rest.relational.RelationalMessages.Error.SEARCH_SERVICE_DELETE_SEARCH_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.SEARCH_SERVICE_GET_SEARCH_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.SEARCH_SERVICE_SAVE_SEARCH_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.SEARCH_SERVICE_WKSP_SEARCHES_ERROR;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.KomodoTypeRegistry;
import org.komodo.repository.KomodoTypeRegistry.TypeIdentifier;
import org.komodo.repository.search.ComparisonOperator;
import org.komodo.repository.search.ContainsClause;
import org.komodo.repository.search.ObjectSearcher;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoSearcherAttributes;
import org.komodo.rest.relational.response.KomodoSavedSearcher;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.jcr.api.JcrConstants;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.SEARCH_SEGMENT)
@Api(tags = {V1Constants.SEARCH_SEGMENT})
public final class KomodoSearchService extends KomodoService {

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoSearchService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * @param type
     * @return if type is ktype then return its modeshape equivalent
     */
    private String convertType(String type) {
        if (type == null)
            return JcrConstants.NT_UNSTRUCTURED;

        KomodoType kType = KomodoType.getKomodoType(type);
        if (kType == null || KomodoType.UNKNOWN.equals(kType))
            return type; // Not a komodo type

        TypeIdentifier identifier = KomodoTypeRegistry.getInstance().getIdentifier(kType);
        String lexiconType = identifier.getLexiconType();
        if (lexiconType != null)
            return lexiconType;

        return JcrConstants.NT_UNSTRUCTURED;
    }

    private ObjectSearcher createObjectSearcher(String type, String parent, String ancestor,
                                                                             String path, String contains, String name) {
        final String ALIAS = "nt";  //$NON-NLS-1$
        ObjectSearcher os = new ObjectSearcher(this.repo);

        os.setFromType(convertType(type), ALIAS);

        LogicalOperator operator = null;
        if (parent != null) {
            os.addWhereParentClause(null, ALIAS, parent, true);
            operator = LogicalOperator.AND;
        }

        if (ancestor != null) {
            os.addWhereParentClause(null, ALIAS, ancestor, false);
            operator = LogicalOperator.AND;
        }

        if (path != null) {
            os.addWherePathClause(operator, ALIAS, path);
            operator = LogicalOperator.AND;
        }

        if (contains != null) {
            /*
             * No longer necessary due to modeshape 4.4.0+
             *
            //
            // The jcr:name is not included in full-text searches in modeshape versions prior to 4.0
            // so we need to create a combination clause that checks the properties and the name
            // of each node. Cannot use jcr:name since its a modeshape pseudo-column but can use
            // NAME(node).
            //
            ContainsClause clause1 = new ContainsClause(null, ALIAS, STAR, KeywordCriteria.ANY, STAR + contains + STAR);
            // Use custom function property
            CompareClause clause2 = new CompareClause(LogicalOperator.OR, null,
                                                                                  "NAME" + OPEN_BRACKET + ALIAS + CLOSE_BRACKET, //$NON-NLS-1$
                                                                                  ComparisonOperator.LIKE, PERCENT + contains + PERCENT);

            os.addWhereParanthesisClause(operator, clause1, clause2);
            */
            ContainsClause clause1 = new ContainsClause(operator, ALIAS, STAR, KeywordCriteria.ANY, STAR + contains + STAR);
            os.addWhereClause(clause1);

            operator = LogicalOperator.AND;
        }

        if (name != null) {
            os.addWhereCompareClause(operator, ALIAS, "mode:localName", ComparisonOperator.LIKE, name); //$NON-NLS-1$
            operator = LogicalOperator.AND;
        }

        return os;
    }

    private Response checkSearchAttributes(String searchName, String type, String path, String parent,
                                                                     String ancestor, String contains, String objectName,
                                                                     List<MediaType> mediaTypes) {
        if (searchName == null && type == null && path == null &&
            parent == null && ancestor == null && contains == null && objectName == null) {

            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.SEARCH_SERVICE_NO_PARAMETERS_ERROR);
        }

        if (parent != null && ancestor != null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.SEARCH_SERVICE_PARENT_ANCESTOR_EXCLUSIVE_ERROR);
        }

        return Response.ok().build();
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param searchName
     *        the request search name parameter
     * @param type
     *        the request type parameter
     * @param parent
     *        the request parent parameter
     * @param ancestor
     *        the request ancestor parameter
     * @param path
     *        the request path of specific object
     * @param contains
     *        the request contains parameter
     * @param objectName
     *        the request name parameter
     * @return a JSON document representing the results of a search in the Komodo workspace
     *                  (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem conducting the search
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Search the workspace using criteria",
                             response = RestBasicEntity[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response searchWorkspace( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(value = "Execute a search already saved in the repository",
                             required = false)
                              @QueryParam(value = SEARCH_SAVED_NAME_PARAMETER) String searchName,
                             @ApiParam(value = "Type of object to search for",
                                                required = false)
                             @QueryParam(value = SEARCH_TYPE_PARAMETER) String type,
                             @ApiParam(value = "The data path of the parent object",
                                                required = false)
                             @QueryParam(value = SEARCH_PARENT_PARAMETER) String parent,
                             @ApiParam(value = "The data path of the ancestor object",
                                                required = false)
                             @QueryParam(value = SEARCH_ANCESTOR_PARAMETER) String ancestor,
                             @ApiParam(value = "The data path of a specific object",
                                                required = false)
                             @QueryParam(value = SEARCH_PATH_PARAMETER) String path,
                             @ApiParam(value = "Search term for object with a property that contains value",
                                                required = false)
                             @QueryParam(value = SEARCH_CONTAINS_PARAMETER) String contains,
                             @ApiParam(value = "The name of an object. Can use '%' as wildcards for broadening searches",
                                                required = false)
                             @QueryParam(value = SEARCH_OBJECT_NAME_PARAMETER) String objectName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        Response response = checkSearchAttributes(searchName, type, path, parent, ancestor,
                                                                              contains, objectName, mediaTypes);
        if (response.getStatus() != Status.OK.getStatusCode())
            return response;

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "objectFromWorkspace", true); //$NON-NLS-1$

            ObjectSearcher os;
            if (searchName != null) {
                os = new ObjectSearcher(repo);
                os.read(uow, searchName);
            } else {
                os = createObjectSearcher(type, parent, ancestor, path, contains, objectName);
            }

            // Execute the search
            List<KomodoObject> searchObjects = os.searchObjects(uow);

            // Convert the results into rest objects for the response
            List<RestBasicEntity> entities = new ArrayList<>();
            for (KomodoObject kObject : searchObjects) {
                RestBasicEntity entity = entityFactory.create(kObject, uriInfo.getBaseUri(), uow);
                if (entity != null) // if kType in UNKNOWN then the entity is not created
                    entities.add(entity);
            }

            return commit( uow, mediaTypes, entities );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, SEARCH_SERVICE_GET_SEARCH_ERROR);
        }
    }

    /**
     * Advanced version of search that uses post with a request body.
     * Allows inclusion of {@link ObjectSearcher#getParameters()} in the
     * json representation.
     *
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param searchAttributes
     *        the Search Attributes JSON representation (cannot be <code>null</code>)
     * @return a JSON document representing the results of a search in the Komodo workspace
     *                  (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem conducting the search
     */
    @POST
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Advanced search of the workspace where the criteria is encapsulated in the request body",
                             response = RestBasicEntity[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response searchWorkspace( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "" + 
                                               "JSON of the possible search attributes:<br>" +
                                               OPEN_PRE_TAG +
                                               OPEN_BRACE + BR +
                                               NBSP + "searchName: \"If specified, use the saved search of this name\"" + COMMA + BR +
                                               NBSP + "type: \"The type of object to search for (FROM type)\"" + COMMA + BR +
                                               NBSP + "parent: \"Limits search to directly beneath this object path\"" + COMMA + BR +
                                               NBSP + "ancestor: \"Limits search to descendents of this object path\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(parent and ancestor are mutually exclusive)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "path: \"Specifies path of the target object" + COMMA + BR +
                                               NBSP + "contain: \"Value that result properties will contain\"" + COMMA + BR +
                                               NBSP + "objectName: \"Value that result names will match\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(Can be a wildcard using '%' symbol)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "Search parameters can be added to above keys" + COMMA + BR +
                                               NBSP + "by specifying them in braces {param1}." + COMMA + BR +
                                               NBSP + "Values of parameters can be included using" + COMMA + BR +
                                               NBSP + "key:value properties." + BR +
                                               NBSP + CLOSE_BRACE + BR +
                                               CLOSE_BRACE +
                                               CLOSE_PRE_TAG,
                                       required = true
                             )
                             final String searchAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoSearcherAttributes sa;
        try {
            sa = KomodoJsonMarshaller.unmarshall(searchAttributes, KomodoSearcherAttributes.class);
            Response response = checkSearchAttributes(sa.getSearchName(), sa.getType(), sa.getPath(), sa.getParent(),
                                                                                  sa.getAncestor(), sa.getContains(), sa.getObjectName(), mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.SEARCH_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "objectFromWorkspace", true); //$NON-NLS-1$

            ObjectSearcher os;
            if (sa.getSearchName() != null) {
                os = new ObjectSearcher(repo);
                os.read(uow, sa.getSearchName());
            } else {
                os = createObjectSearcher(sa.getType(), sa.getParent(), sa.getAncestor(),
                                                          sa.getPath(), sa.getContains(), sa.getObjectName());
            }

            // Resolve any parameters if applicable
            for(Map.Entry<String, String> parameter : sa.getParameters().entrySet()) {
                String value = parameter.getValue();

                // Maybe a KType used here so convert them
                value = convertType(value);

                os.setParameterValue(parameter.getKey(), value);
            }

            // Execute the search
            List<KomodoObject> searchObjects = os.searchObjects(uow);

            // Convert the results into rest objects for the response
            List<RestBasicEntity> entities = new ArrayList<>();
            for (KomodoObject kObject : searchObjects) {
                RestBasicEntity entity = entityFactory.create(kObject, uriInfo.getBaseUri(), uow);
                if (entity != null) // if kType in UNKNOWN then the entity is not created
                    entities.add(entity);
            }

            return commit( uow, mediaTypes, entities );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, SEARCH_SERVICE_GET_SEARCH_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the results of a search in the Komodo workspace
     *                  (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the saved searches
     */
    @GET
    @Path(V1Constants.SAVED_SEARCHES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Fetch saved searches from the workspace",
                             response = RestBasicEntity[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSavedSearches( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "searchesFromWorkspace", true); //$NON-NLS-1$

            String searchesGroupPath = repo.komodoSearches(uow).getAbsolutePath();
            KomodoObject searchesGroup = repo.getFromWorkspace(uow, searchesGroupPath);

            List<KomodoSavedSearcher> entities = new ArrayList<KomodoSavedSearcher>();
            if (searchesGroup != null) {
                KomodoObject[] searches = searchesGroup.getChildrenOfType(uow, KomodoLexicon.Search.NODE_TYPE);

                for (KomodoObject kObject : searches) {
                    String name = kObject.getName(uow);
                    try {
                        ObjectSearcher os = new ObjectSearcher(repo);
                        os.read(uow, name);

                        KomodoSavedSearcher kso = new KomodoSavedSearcher();
                        kso.setName(name);
                        kso.setQuery(os.toString());
                        kso.setParameters(os.getParameters());

                        entities.add(kso);
                    } catch (KException ex) {
                        // Ignore if this search in invalid
                    }
                }
            }

            return commit(uow, mediaTypes, entities);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, SEARCH_SERVICE_WKSP_SEARCHES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param searchAttributes
     *        the Search Attributes JSON representation (cannot be <code>null</code>)
     * @return a JSON document representing the search attributes passed from the request
     *                  (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem performing the save
     */
    @POST
    @Path(V1Constants.SAVED_SEARCHES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Save a search to the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response saveSearch( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "" + 
                                               "JSON of the search attributes:<br>" +
                                               OPEN_PRE_TAG +
                                               OPEN_BRACE + BR +
                                               NBSP + "searchName: \"The name to save the search to\"" + COMMA + BR +
                                               NBSP + "type: \"The type of object to search for (FROM type)\"" + COMMA + BR +
                                               NBSP + "parent: \"Limits search to directly beneath this object path\"" + COMMA + BR +
                                               NBSP + "ancestor: \"Limits search to descendents of this object path\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(parent and ancestor are mutually exclusive)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "path: \"Specifies path of the target object" + COMMA + BR +
                                               NBSP + "contain: \"Value that result properties will contain\"" + COMMA + BR +
                                               NBSP + "objectName: \"Value that result names will match\"" + COMMA + BR +
                                               NBSP + OPEN_PRE_CMT + "(Can be a wildcard using '%' symbol)" + CLOSE_PRE_CMT + BR +
                                               NBSP + "Search parameters can be added to above keys" + BR +
                                               NBSP + "by specifying them in braces {param1}." + BR +
                                               NBSP + CLOSE_BRACE + BR +
                                               CLOSE_BRACE +
                                               CLOSE_PRE_TAG,
                                       required = true
                             )
                             final String searchAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        KomodoSearcherAttributes sa;
        ObjectSearcher os;
        try {
            sa = KomodoJsonMarshaller.unmarshall(searchAttributes, KomodoSearcherAttributes.class);
            os = createObjectSearcher(sa.getType(), sa.getParent(), sa.getAncestor(),
                                                      sa.getPath(), sa.getContains(), sa.getObjectName());
        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "writeSearchToWorkspace", false); //$NON-NLS-1$
            os.write(uow, sa.getSearchName());

            return commit(uow, mediaTypes, sa);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, SEARCH_SERVICE_SAVE_SEARCH_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param searchName
     *        the name of the saved search to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path(V1Constants.SAVED_SEARCHES_SEGMENT + FORWARD_SLASH + "{searchName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a search from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteSavedSearch( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "Name of the saved search to be deleted",
                                       required = true
                             )
                             final @PathParam( "searchName" ) String searchName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeSearchFromWorkspace", false); //$NON-NLS-1$

            String searchesGroupPath = repo.komodoSearches(uow).getAbsolutePath();
            KomodoObject searchesGroup = repo.getFromWorkspace(uow, searchesGroupPath);

            if (searchesGroup == null)
                return Response.noContent().build();

            KomodoObject search = searchesGroup.getChild(uow, searchName);
            if (search == null)
                return Response.noContent().build();

            search.remove(uow);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(searchName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, SEARCH_SERVICE_DELETE_SEARCH_ERROR);
        }
    }
}
