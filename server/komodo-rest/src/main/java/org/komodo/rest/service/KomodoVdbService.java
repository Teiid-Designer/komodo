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

import static org.komodo.rest.Messages.General.GET_OPERATION_NAME;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_DATA_SOURCE_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_NAME_VALIDATION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_COLUMNS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_CONDITIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_CONDITION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_DATA_ROLES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_DATA_ROLE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORTS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_IMPORT_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MASKS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MASK_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODELS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_MODEL_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_PERMISSIONS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_PERMISSION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_SOURCE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TABLES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATORS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_TRANSLATOR_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDBS_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.Messages;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestBasicEntity.ResourceNotFound;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import com.google.common.base.Objects;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.VDBS_SEGMENT)
@Api(tags = {V1Constants.VDBS_SEGMENT})
public final class KomodoVdbService extends KomodoService {

    private static final int ALL_AVAILABLE = -1;
    private static final String VDB_PATH_DEFAULT = "defaultPath";  //$NON-NLS-1$
    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoVdbService( final KEngine engine ) throws WebApplicationException {
        super( engine );
    }

    /**
     * Create a new Vdb in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @param vdbJson
     *        the Vdb JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new vdb (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the VDB
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a vdb in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createVdb( final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam(
                                         value = "Name of the Vdb to be created",
                                         required = true
                               )
                               final @PathParam( "vdbName" ) String vdbName,
                               @ApiParam(
                                         value = "" + 
                                                 "JSON of the properties of the Vdb to add:<br>" +
                                                 OPEN_PRE_TAG +
                                                 OPEN_BRACE + BR +
                                                 NBSP + "keng\\_\\_id: \"name of the Vdb\"" + COMMA + BR +
                                                 NBSP + "vdb\\_\\_name: \"name of the Vdb\"" + COMMA + BR +
                                                 NBSP + "keng\\_\\_dataPath: \"path of Vdb to create\"" + COMMA + BR +
                                                 NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\")" + CLOSE_PRE_CMT + BR +
                                                 NBSP + "vdb\\_\\_originalFile: \"original file location\"" + COMMA + BR +
                                                 NBSP + OPEN_PRE_CMT + "(same value as keng\\_\\_dataPath)" + CLOSE_PRE_CMT + BR +
                                                 NBSP + "keng\\_\\_kType: \"Vdb\"" + BR +
                                                 CLOSE_BRACE +
                                                 CLOSE_PRE_TAG,
                                         required = true
                               )
                               final String vdbJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the Vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_VDB_NAME);
        }

        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );
        final String jsonVdbName = restVdb.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonVdbName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_VDB_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = vdbName.equals( jsonVdbName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_NAME_DONT_MATCH_ERROR, vdbName, jsonVdbName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createVdb", false ); //$NON-NLS-1$
            
            // Error if the repo already contains a vdb with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, vdbName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_ALREADY_EXISTS, vdbName);
            }
            
            // create new Vdb
            return doAddVdb( uow, uriInfo.getBaseUri(), mediaTypes, restVdb );
            
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_ERROR, vdbName);
        }
    }
    
    private Response doAddVdb( final UnitOfWork uow,
                               final URI baseUri,
                               final List<MediaType> mediaTypes,
                               final RestVdb restVdb ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restVdb != null );

        final String vdbName = restVdb.getId();
        try {
            final Vdb vdb = getWorkspaceManager(uow).createVdb( uow, null, vdbName, restVdb.getOriginalFilePath() );

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, vdb, restVdb);

            final RestVdb entity = entityFactory.create(vdb, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_ERROR, vdbName ), e );
        }
    }

    /**
     * Update a Vdb in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @param vdbJson
     *        the vdb JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated vdb (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the vdb
     */
    @PUT
    @Path( StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a vdb in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateVdb( final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam(
                                         value = "Name of the Vdb to be updated",
                                         required = true
                               )
                               final @PathParam( "vdbName" ) String vdbName,
                               @ApiParam(
                                         value = "" + 
                                                 "JSON of the properties of the Vdb to update:<br>" +
                                                 OPEN_PRE_TAG +
                                                 OPEN_BRACE + BR +
                                                 NBSP + "keng\\_\\_id: \"name of the Vdb\"" + COMMA + BR +
                                                 NBSP + "vdb\\_\\_name: \"name of the Vdb\"" + COMMA + BR +
                                                 NBSP + "keng\\_\\_dataPath: \"path of Vdb to update\"" + COMMA + BR +
                                                 NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\")" + CLOSE_PRE_CMT + BR +
                                                 NBSP + "vdb\\_\\_originalFile: \"original file location\"" + COMMA + BR +
                                                 NBSP + OPEN_PRE_CMT + "(same value as keng\\_\\_dataPath)" + CLOSE_PRE_CMT + BR +
                                                 NBSP + "keng\\_\\_kType: \"Vdb\"" + BR +
                                                 CLOSE_BRACE +
                                                 CLOSE_PRE_TAG,
                                         required = true
                               )
                               final String vdbJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_VDB_NAME);
        }


        final RestVdb restVdb = KomodoJsonMarshaller.unmarshall( vdbJson, RestVdb.class );
        final String jsonVdbName = restVdb.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonVdbName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_VDB_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateVdb", false ); //$NON-NLS-1$

            WorkspaceManager wMgr = getWorkspaceManager(uow);
            final boolean exists = wMgr.hasChild( uow, vdbName );
            // Error if the specified service does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_DNE);
            }

            // must be an update
            final KomodoObject kobject = wMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = wMgr.resolve( uow, kobject, Vdb.class );

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, vdb, restVdb);

            // rename if names did not match
            final boolean namesMatch = vdbName.equals( jsonVdbName );
            if ( !namesMatch ) {
                vdb.rename( uow, jsonVdbName );
            }

            KomodoProperties properties = new KomodoProperties();
            final RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateVdb: vdb '{0}' entity was updated", vdb.getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_ERROR);
        }
    }

    /**
     * Clone a Vdb in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the Vdb name (cannot be empty)
     * @param newVdbName
     *        the new Vdb name (cannot be empty)
     * @return a JSON representation of the new Vdb (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Vdb
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CLONE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Clone a VDB in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response cloneVdb( final @Context HttpHeaders headers,
                              final @Context UriInfo uriInfo,
                              @ApiParam(
                                        value = "Name of the Vdb to be cloned",
                                        required = true
                              )
                              final @PathParam( "vdbName" ) String vdbName,
                              @ApiParam(
                                        value = "name of the new Vdb", 
                                        required = true
                              )
                              final String newVdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the Vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CLONE_MISSING_NAME);
        }

        // Error if the new Vdb name is missing
        if ( StringUtils.isBlank( newVdbName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CLONE_MISSING_NEW_NAME);
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = vdbName.equals( newVdbName );
        if ( namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CLONE_SAME_NAME_ERROR, newVdbName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "cloneVdb", false ); //$NON-NLS-1$
            
            WorkspaceManager wMgr = getWorkspaceManager(uow);
            // Error if the repo already contains a vdb with the supplied name.
            if ( wMgr.hasChild( uow, newVdbName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CLONE_ALREADY_EXISTS);
            }
            
            // Get the existing VDB to clone
            final KomodoObject kobject = wMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb oldVdb = wMgr.resolve( uow, kobject, Vdb.class );

            // Get VDB content
            oldVdb.setVdbName(uow, newVdbName);  // Change VDBName to new name so xml has desired new name
            boolean oldHadDeploymentProperty = oldVdb.hasProperty(uow, "deployment-name"); //$NON-NLS-1$
            if(oldHadDeploymentProperty) {
                oldVdb.setProperty(uow, "deployment-name", newVdbName + "-vdb.xml");  //$NON-NLS-1$ //$NON-NLS-2$                
            }
            
            byte[] vdbXml = oldVdb.export(uow, null); 
            InputStream vdbStream = new ByteArrayInputStream(vdbXml);
            oldVdb.setVdbName(uow, vdbName);     // Change VDBName back to original name (TODO fix importer to handle this)
            if(oldHadDeploymentProperty) {
                oldVdb.setProperty(uow, "deployment-name", vdbName + "-vdb.xml");  //$NON-NLS-1$ //$NON-NLS-2$
            }

            // Import to create a new Vdb in the workspace
            VdbImporter importer = new VdbImporter(this.repo);
            final ImportOptions importOptions = new ImportOptions();
            importOptions.setOption( OptionKeys.NAME, newVdbName );
            ImportMessages importMessages = new ImportMessages();
            importer.importVdb(uow, vdbStream, this.repo.komodoWorkspace(uow), importOptions, importMessages);

            if(importMessages.hasError()) {
                LOGGER.debug("cloneVDB for '{0}' failed", newVdbName); //$NON-NLS-1$
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CLONE_VDB_ERROR, vdbName);
            }
                        
            // Get the newly created VDB
            final KomodoObject kobject2 = wMgr.getChild( uow, newVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = wMgr.resolve( uow, kobject2, Vdb.class );

            final RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_CLONE_VDB_ERROR, vdbName);
        }
    }
    
    // Sets VDB properties using the supplied RestVdb object
    private void setProperties(final UnitOfWork uow, Vdb vdb, RestVdb restVdb) throws KException {
        // 'New' = requested RestVdb properties
        String newDescription = restVdb.getDescription();
        String newConnType = restVdb.getConnectionType();
        String newOrigFilePath = restVdb.getOriginalFilePath();
        int newVersion = restVdb.getVersion();
        List<RestProperty> newProperties = restVdb.getProperties();

        // 'Old' = current Vdb properties
        String oldDescription = vdb.getDescription(uow);
        String oldConnType = vdb.getConnectionType(uow);
        String oldOrigFilePath = vdb.getOriginalFilePath(uow);
        int oldVersion = vdb.getVersion(uow);

        // Description
        if ( !StringUtils.equals(newDescription, oldDescription) ) {
            vdb.setDescription( uow, newDescription );
        } 
        // ConnectionType
        if ( !StringUtils.equals(newConnType, oldConnType) ) {
            vdb.setConnectionType( uow, newConnType );
        } 
        // Original FilePath
        if ( !StringUtils.equals(newOrigFilePath, oldOrigFilePath) ) {
            String origFilePath = (newOrigFilePath==null) ? VDB_PATH_DEFAULT : newOrigFilePath; 
            vdb.setOriginalFilePath( uow, origFilePath );
        } 
        // version
        if ( newVersion != oldVersion ) {
            vdb.setVersion( uow, newVersion );
        } 
        
        // Set new properties
        for(RestProperty newProp : newProperties) {
            vdb.setProperty(uow, newProp.getName(), newProp.getValue());
        }
    }

    // Sets Model properties using the supplied RestVdbModel object
    private void setProperties(final UnitOfWork uow, Model model, RestVdbModel restVdbModel) throws KException {
        // 'New' = requested RestVdbModel properties
        String newDescription = restVdbModel.getDescription();
        boolean newIsVisible = restVdbModel.isVisible();
        List<RestProperty> newProperties = restVdbModel.getProperties();

        // 'Old' = current Model properties
        String oldDescription = model.getDescription(uow);
        boolean oldIsVisible = model.isVisible(uow);

        // Description
        if ( !StringUtils.equals(newDescription, oldDescription) ) {
            model.setDescription( uow, newDescription );
        } 

        // IsVisible
        if ( newIsVisible != oldIsVisible ) {
            model.setVisible(uow, newIsVisible);
        }
        
        // model type
        final Type newModelType = restVdbModel.getModelType();
        
        if ( !Objects.equal( newModelType, model.getModelType( uow ) ) ) {
            model.setModelType( uow, newModelType );
        }
        
        // metadata type
        final String newMetadataType = restVdbModel.getMetadataType();
        
        if ( !Objects.equal( newMetadataType, model.getMetadataType( uow ) ) ) {
            model.setMetadataType( uow, newMetadataType );
        }
        
        // model definition
        final String newDdl = restVdbModel.getDdl();
        
        if ( !Objects.equal( newDdl, model.getModelDefinition( uow ) ) ) {
            model.setModelDefinition( uow, newDdl );
        }
        
        // Set new properties
        for(RestProperty newProp : newProperties) {
            model.setProperty(uow, newProp.getName(), newProp.getValue());
        }
    }

    // Sets ModelSource properties using the supplied RestVdbModel object
    private void setProperties(final UnitOfWork uow, ModelSource modelSource, RestVdbModelSource restVdbModelSource) throws KException {
        // 'New' = requested RestVdbModelSource properties
        String newTranslator = restVdbModelSource.getTranslator();
        String newJndi = restVdbModelSource.getJndiName();
        List<RestProperty> newProperties = restVdbModelSource.getProperties();

        // 'Old' = current ModelSource properties
        String oldTranslator = modelSource.getTranslatorName(uow);
        String oldJndi = modelSource.getJndiName(uow);

        // Translator
        if ( !StringUtils.equals(newTranslator, oldTranslator) ) {
            modelSource.setTranslatorName( uow, newTranslator );
        } 
        // JNDI
        if ( !StringUtils.equals(newJndi, oldJndi) ) {
            modelSource.setJndiName( uow, newJndi );
        } 
        // Set new properties
        for(RestProperty newProp : newProperties) {
            modelSource.setProperty(uow, newProp.getName(), newProp.getValue());
        }
    }

    private Model findModel(UnitOfWork uow, List<MediaType> mediaTypes,
                                                String modelName, Vdb vdb) throws KException {
        if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
            return null;
        }

        KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        Model model = getWorkspaceManager(uow).resolve( uow, kModel, Model.class );
        LOGGER.debug( "Model '{0}' was found", modelName ); //$NON-NLS-1$
        return model;
    }

    private Table findTable(UnitOfWork uow, List<MediaType> mediaTypes, String tableName, Model model) throws KException {
    	Table[] tables = model.getTables(uow, tableName);
    	if(tables.length == 0) {
    		return null;
    	}
    	LOGGER.debug( "Table '{0}' was found", tableName ); //$NON-NLS-1$
    	return tables[0];
    }
    
    private DataRole findDataRole(UnitOfWork uow, List<MediaType> mediaTypes,
                                                      String dataRoleId, Vdb vdb) throws KException {
        DataRole[] dataRoles = vdb.getDataRoles(uow);
        if (dataRoles == null || dataRoles.length == 0) {
            return null;
        }

        DataRole dataRole = null;
        for (DataRole drole : dataRoles) {
            if (dataRoleId.equals(drole.getName(uow))) {
                dataRole = drole;
                break;
            }
        }

        return dataRole;
    }

    private Permission findPermission(UnitOfWork uow, List<MediaType> mediaTypes,
                                                              String permissionId, DataRole dataRole, Vdb vdb) throws KException {
        Permission[] permissions = dataRole.getPermissions(uow);
        if (permissions == null || permissions.length == 0) {
            return null;
        }

        Permission permission = null;
        for (Permission perm : permissions) {
            if (permissionId.equals(perm.getName(uow))) {
                permission = perm;
                break;
            }
        }

        return permission;
    }
    
    /**
     * Delete the specified Vdb from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the Vdb to be removed (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{vdbName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a vdb from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteVdb( final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam(
                                         value = "Name of the Vdb to be removed",
                                         required = true
                               )
                               final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_VDB_NAME);
        }
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeVdbFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager mgr = getWorkspaceManager(uow);
            KomodoObject vdb = mgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            
            if (vdb == null)
                return Response.noContent().build();

            mgr.delete(uow, vdb);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(vdbName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_DELETE_VDB_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the VDBs in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of vdbs",
                            response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbs( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find VDBs
            uow = createTransaction(principal, "getVdbs", true ); //$NON-NLS-1$
            Vdb[] vdbs = null;

            WorkspaceManager wsMgr = getWorkspaceManager(uow);
            if ( StringUtils.isBlank( searchPattern ) ) {
                vdbs = wsMgr.findVdbs( uow );
                LOGGER.debug( "getVdbs:found '{0}' VDBs", vdbs.length ); //$NON-NLS-1$
            } else {
                final String[] vdbPaths = wsMgr.findByType( uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, null, searchPattern, false );

                if ( vdbPaths.length == 0 ) {
                    vdbs = Vdb.NO_VDBS;
                } else {
                    vdbs = new Vdb[ vdbPaths.length ];
                    int i = 0;

                    for ( final String path : vdbPaths ) {
                        vdbs[ i++ ] = wsMgr.resolve( uow, new ObjectImpl( wsMgr.getRepository(), path, 0 ), Vdb.class );
                    }

                    LOGGER.debug( "getVdbs:found '{0}' VDBs using pattern '{1}'", vdbs.length, searchPattern ); //$NON-NLS-1$
                }
            }

            int start = 0;

            { // start query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.START );

                if ( qparam != null ) {

                    try {
                        start = Integer.parseInt( qparam );

                        if ( start < 0 ) {
                            start = 0;
                        }
                    } catch ( final Exception e ) {
                        start = 0;
                    }
                }
            }

            int size = ALL_AVAILABLE;

            { // size query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.SIZE );

                if ( qparam != null ) {

                    try {
                        size = Integer.parseInt( qparam );

                        if ( size <= 0 ) {
                            size = ALL_AVAILABLE;
                        }
                    } catch ( final Exception e ) {
                        size = ALL_AVAILABLE;
                    }
                }
            }

            final List< RestVdb > entities = new ArrayList< >();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, false);
            for ( final Vdb vdb : vdbs ) {
                if ( ( start == 0 ) || ( i >= start ) ) {
                    if ( ( size == ALL_AVAILABLE ) || ( entities.size() < size ) ) {
                        RestVdb entity = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
                        entities.add(entity);
                        LOGGER.debug("getVdbs:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
                    } else {
                        break;
                    }
                }

                ++i;
            }

            // create response
            return commit( uow, mediaTypes, entities );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_VDBS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find vdb by name", response = RestVdb.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getVdb", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, mediaTypes.contains(MediaType.APPLICATION_XML_TYPE));
            final RestVdb restVdb = entityFactory.create(vdb, uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", vdb.getName(uow)); //$NON-NLS-1$
            return commit( uow, mediaTypes, restVdb );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_VDB_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all models belonging to the vdb", response = RestVdb.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No models could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModels( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getModels", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model[] models = vdb.getModels(uow);
            if (models == null)
                models = new Model[0];

            List<RestVdbModel> restModels = new ArrayList<>(models.length);
            for (Model model : models) {
                RestVdbModel entity = entityFactory.create(model, uriInfo.getBaseUri(), uow);
                restModels.add(entity);
                LOGGER.debug("getModels:Model from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restModels );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_MODELS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB model (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the named model belonging to the vdb", response = RestVdbModel.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModel( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model to be fetched", required = true)
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getModel", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            RestVdbModel restModel = entityFactory.create(model, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getModel:Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restModel);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_MODEL_ERROR, modelName, vdbName);
        }
    }

    /**
     * Create a new Model in the specified VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @param modelName
     *        the model name (cannot be empty)
     * @param modelJson
     *        the Model JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new model (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Model
     */
    @POST
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a Model in a VDB")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createModel( final @Context HttpHeaders headers,
                                 final @Context UriInfo uriInfo,
                                 @ApiParam(
                                           value = "Name of the Vdb",
                                           required = true
                                 )
                                 final @PathParam( "vdbName" ) String vdbName,
                                 @ApiParam(
                                           value = "Name of the Model",
                                           required = true
                                 )
                                 final @PathParam( "modelName" ) String modelName,
                                 @ApiParam(
                                           value = "" + 
                                                   "JSON of the properties of the Model to add:<br>" +
                                                   OPEN_PRE_TAG +
                                                   OPEN_BRACE + BR +
                                                   NBSP + "keng\\_\\_id: \"name of the Model\"" + COMMA + BR +
                                                   NBSP + "keng\\_\\_dataPath: \"path of Model to create\"" + COMMA + BR +
                                                   NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\\\\{modelName\\}\")" + CLOSE_PRE_CMT + BR +
                                                   NBSP + "keng\\_\\_kType: \"Model\"" + BR +
                                                   CLOSE_BRACE +
                                                   CLOSE_PRE_TAG,
                                           required = true
                                 )
                                 final String modelJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the VDB name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_VDB_NAME);
        }

        // Error if the Model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_MODEL_NAME);
        }

        final RestVdbModel restVdbModel = KomodoJsonMarshaller.unmarshall( modelJson, RestVdbModel.class );
        final String jsonModelName = restVdbModel.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonModelName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_MODEL_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = modelName.equals( jsonModelName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MODEL_NAME_DONT_MATCH_ERROR, modelName, jsonModelName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createModel", false ); //$NON-NLS-1$
            
            // Get the specified VDB parent
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            KomodoObject kobject = mgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );
            
            if (vdb == null)
                return Response.noContent().build();
            
            // Error if the VDB already contains a Model with the supplied name.
            if( vdb.getModels(uow, modelName).length != 0 ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_MODEL_ALREADY_EXISTS, modelName);
            }
            
            // create a new Model in the VDB
            return doAddModel( uow, uriInfo.getBaseUri(), mediaTypes, vdb, restVdbModel );
            
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_MODEL_ERROR, vdbName);
        }
    }
    
    private Response doAddModel( final UnitOfWork uow,
                               final URI baseUri,
                               final List<MediaType> mediaTypes,
                               Vdb vdb,
                               final RestVdbModel restVdbModel ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( vdb != null );
        assert( restVdbModel != null );

        final String modelName = restVdbModel.getId();
        try {
            Model newModel = vdb.addModel(uow, modelName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, newModel, restVdbModel);

            final RestVdbModel entity = entityFactory.create(newModel, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_MODEL_ERROR, modelName ), e );
        }
    }

    /**
     * Update a Model in the specified VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @param modelName
     *        the model name (cannot be empty)
     * @param modelJson
     *        the model JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated model (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the model
     */
    @PUT
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a Model in the VDB")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateModel( final @Context HttpHeaders headers,
                                 final @Context UriInfo uriInfo,
                                 @ApiParam(
                                           value = "Name of the Vdb",
                                           required = true
                                 )
                                 final @PathParam( "vdbName" ) String vdbName,
                                 @ApiParam(
                                           value = "Name of the Model to be updated",
                                           required = true
                                 )
                                 final @PathParam( "modelName" ) String modelName,
                                 @ApiParam(
                                           value = "" + 
                                                   "JSON of the properties of the Model to update:<br>" +
                                                   OPEN_PRE_TAG +
                                                   OPEN_BRACE + BR +
                                                   NBSP + "keng\\_\\_id: \"name of the Model\"" + COMMA + BR +
                                                   NBSP + "keng\\_\\_dataPath: \"path of Model to update\"" + COMMA + BR +
                                                   NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\\\\{modelName\\}\")" + CLOSE_PRE_CMT + BR +
                                                   NBSP + "keng\\_\\_kType: \"Model\"" + BR +
                                                   CLOSE_BRACE +
                                                   CLOSE_PRE_TAG,
                                           required = true
                                 )
                                 final String modelJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_VDB_NAME);
        }

        // Error if the model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_MODEL_NAME);
        }

        final RestVdbModel restVdbModel = KomodoJsonMarshaller.unmarshall( modelJson, RestVdbModel.class );
        final String jsonModelName = restVdbModel.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonModelName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_MODEL_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateVdbModel", false ); //$NON-NLS-1$

            // Get the specified VDB parent
            KomodoObject kobject = getWorkspaceManager(uow).getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = getWorkspaceManager(uow).resolve( uow, kobject, Vdb.class );
            
            if (vdb == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_DNE);
            }
            
            // Error if the VDB already contains a Model with the supplied name.
            Model[] models = vdb.getModels(uow, modelName);
            if( models.length == 0 ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_MODEL_ALREADY_EXISTS, modelName);
            }

            // Transfers the properties from the rest object to the model
            setProperties(uow, models[0], restVdbModel);

            KomodoProperties properties = new KomodoProperties();
            final RestVdbModel entity = entityFactory.create(models[0], uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateVdbModel: VdbModel '{0}' entity was updated", models[0].getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_MODEL_ERROR);
        }
    }

    /**
     * Delete the specified Model from the specified VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the Vdb (cannot be <code>null</code>)
     * @param modelName
     *        the name of the model to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.MODEL_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a Model from the VDB")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteModel( final @Context HttpHeaders headers,
                                 final @Context UriInfo uriInfo,
                                 @ApiParam(
                                           value = "Name of the Vdb",
                                           required = true
                                 )
                                 final @PathParam( "vdbName" ) String vdbName,
                                 @ApiParam(
                                           value = "Name of the Model to remove",
                                           required = true
                                 )
                                 final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the Vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_VDB_NAME);
        }
        
        // Error if the Model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_MODEL_NAME);
        }
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeModelFromVdb", false); //$NON-NLS-1$

            // Get the specified VDB parent
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            KomodoObject kobject = mgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );
            
            if (vdb == null)
                return Response.noContent().build();
            
            Model[] models = vdb.getModels(uow, modelName);
            if(models.length==0) {
                return Response.noContent().build();
            }
            
            vdb.removeModel(uow, modelName);
            
            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(modelName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_DELETE_VDB_MODEL_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translators (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all translators belonging to the Vdb", response = RestVdbTranslator[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No translators could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslators( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the VDB", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getTranslators", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null)
                translators = new Translator[0];

            List<RestVdbTranslator> restTranslators = new ArrayList<>(translators.length);
            for (Translator translator : translators) {
                RestVdbTranslator entity = entityFactory.create(translator, uriInfo.getBaseUri(), uow);
                restTranslators.add(entity);
                LOGGER.debug("getTranslators:Translator from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restTranslators );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_TRANSLATORS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param translatorName
     *        the id of the translator being retrieved (cannot be empty)
     * @return the JSON representation of the VDB translator (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATORS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.TRANSLATOR_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the named translator belonging to the vdb", response = RestVdbTranslator.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No translator could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslator( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the translator to be fetched", required = true)
                            final @PathParam( "translatorName" ) String translatorName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getTranslator", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Translator[] translators = vdb.getTranslators(uow);
            if (translators == null || translators.length == 0) {
                LOGGER.debug("getTranslators:No translators found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, TRANSLATORS_SEGMENT, translatorName);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Translator translator = null;
            for (Translator tr : translators) {
                if (translatorName.equals(tr.getName(uow))) {
                    translator = tr;
                    break;
                }
            }

            // make sure source exists
            if (translator == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(uri(vdbName, V1Constants.TRANSLATORS_SEGMENT, translatorName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbTranslator restTranslator = entityFactory.create(translator, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getTranslator:Translator '{0}' from VDB '{1}' entity was constructed", translatorName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restTranslator);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_TRANSLATOR_ERROR, translatorName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all imports belonging to the vdb", response = RestVdbImport[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No imports could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getImports( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getImports", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null)
                imports = new VdbImport[0];

            List<RestVdbImport> restImports = new ArrayList<>(imports.length);
            for (VdbImport vdbImport : imports) {
                RestVdbImport entity = entityFactory.create(vdbImport, uriInfo.getBaseUri(), uow);
                restImports.add(entity);
                LOGGER.debug("getImports:Import from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restImports);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_IMPORTS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param importName
     *        the id of the import being retrieved (cannot be empty)
     * @return the JSON representation of the VDB import (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORTS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.IMPORT_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the named vdb import belonging to the vdb", response = RestVdbImport.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No import could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getImport( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Vdb import to be fetched", required = true)
                            final @PathParam( "importName" ) String importName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getImport", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            VdbImport[] imports = vdb.getImports(uow);
            if (imports == null || imports.length == 0) {
                LOGGER.debug("getImport:No import found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, TRANSLATORS_SEGMENT, importName);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            VdbImport vdbImport = null;
            for (VdbImport imp : imports) {
                if (importName.equals(imp.getName(uow))) {
                    vdbImport = imp;
                    break;
                }
            }

            // make sure source exists
            if (vdbImport == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(uri(vdbName, IMPORTS_SEGMENT, importName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbImport restImport = entityFactory.create(vdbImport, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getImport:Import '{0}' from VDB '{1}' entity was constructed", importName, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restImport);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_IMPORT_ERROR, importName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all data roles belonging to the vdb", response = RestBasicEntity[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No data roles could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataRoles( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataRoles", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole[] dataRoles = vdb.getDataRoles(uow);
            if (dataRoles == null)
                dataRoles = new DataRole[0];

            List<RestVdbDataRole> restDataRoles = new ArrayList<>(dataRoles.length);
            for (DataRole dataRole : dataRoles) {
                RestVdbDataRole entity = entityFactory.create(dataRole, uriInfo.getBaseUri(), uow);
                restDataRoles.add(entity);
                LOGGER.debug("getDataRoles:Data role from VDB '{0}' entity was constructed", vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restDataRoles);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_DATA_ROLES_ERROR, vdbName);
        }
    }

    /**
     * Create a data role in the specified VDB.
     * 
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @param dataRoleName
     *        the data role name (cannot be empty)
     * @param dataRoleJson
     *        the data role JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the data role
     */
    @POST
    @Path( V1Constants.VDB_PLACEHOLDER
           + StringConstants.FORWARD_SLASH
           + V1Constants.DATA_ROLES_SEGMENT
           + StringConstants.FORWARD_SLASH
           + V1Constants.DATA_ROLE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a VDB data role")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No VDB could be found with specified name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createDataRole( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam( value = "Name of the VDB", required = true ) 
                                    final @PathParam( "vdbName" ) String vdbName,
                                    @ApiParam( value = "Name of the data role", required = true )
                                    final @PathParam( "dataRoleId" ) String dataRoleName,
                                    @ApiParam( value = ""
                                                       + "JSON of the properties of the data role to add:<br>"
                                                       + OPEN_PRE_TAG
                                                       + OPEN_BRACE
                                                       + BR
                                                       + NBSP
                                                       + "keng\\_\\_id: \"name of the data role\""
                                                       + COMMA
                                                       + BR
                                                       + NBSP
                                                       + "keng\\_\\_dataPath: \"path of the data role to create\""
                                                       + COMMA
                                                       + BR
                                                       + NBSP
                                                       + OPEN_PRE_CMT
                                                       + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\\vdb:dataRoles\\\\{dataRoleName\\}\")"
                                                       + CLOSE_PRE_CMT
                                                       + BR
                                                       + NBSP
                                                       + "keng\\_\\_kType: \"DataRole\""
                                                       + BR
                                                       + CLOSE_BRACE
                                                       + CLOSE_PRE_TAG, required = true )
                                    final String dataRoleJson ) throws KomodoRestException {
        final SecurityPrincipal principal = checkSecurityContext( headers );
        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final List< MediaType > mediaTypes = headers.getAcceptableMediaTypes();
        if ( !isAcceptable( mediaTypes, MediaType.APPLICATION_JSON_TYPE ) ) {
            return notAcceptableMediaTypesBuilder().build();
        }

        // Error if the VDB name is missing
        if ( StringUtils.isBlank( vdbName ) ) {
            return createErrorResponseWithForbidden( mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_VDB_NAME );
        }

        // Error if the data role name is missing
        if ( StringUtils.isBlank( dataRoleName ) ) {
            return createErrorResponseWithForbidden( mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_DATA_ROLE_NAME );
        }

        final RestVdbDataRole restDataRole = KomodoJsonMarshaller.unmarshall( dataRoleJson, RestVdbDataRole.class );
        final String jsonDataRoleName = restDataRole.getId();

        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonDataRoleName ) ) {
            return createErrorResponseWithForbidden( mediaTypes,
                                                     RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_DATA_ROLE_NAME );
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataRoleName.equals( jsonDataRoleName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden( mediaTypes,
                                                     RelationalMessages.Error.VDB_SERVICE_DATA_ROLE_NAME_DONT_MATCH_ERROR,
                                                     dataRoleName,
                                                     jsonDataRoleName );
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "createDataRole", false ); //$NON-NLS-1$
            
            // Get the specified VDB
            final WorkspaceManager mgr = getWorkspaceManager( uow );
            final KomodoObject kobject = mgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );
            
            // Error if VDB is not found
            if ( vdb == null ) {
                return Response.noContent().build();
            }
            
            // Error if the VDB already contains a data role with the supplied name.
            if ( vdb.getDataRoles( uow, dataRoleName ).length != 0 ) {
                return createErrorResponseWithForbidden( mediaTypes,
                                                         RelationalMessages.Error.VDB_SERVICE_DATA_ROLE_ALREADY_EXISTS,
                                                         dataRoleName );
            }
            
            // create a new Model in the VDB
            return doAddDataRole( uow, uriInfo.getBaseUri(), mediaTypes, vdb, restDataRole );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( mediaTypes,
                                                     e,
                                                     RelationalMessages.Error.VDB_SERVICE_CREATE_DATA_ROLE_ERROR,
                                                     vdbName,
                                                     dataRoleName );
        }
    }
    
    private Response doAddDataRole( final UnitOfWork uow,
                                    final URI baseUri,
                                    final List< MediaType > mediaTypes,
                                    final Vdb vdb,
                                    final RestVdbDataRole restDataRole ) throws KomodoRestException {
        assert ( !uow.isRollbackOnly() );
        assert ( uow.getState() == State.NOT_STARTED );
        assert ( vdb != null );
        assert ( restDataRole != null );

        final String dataRoleName = restDataRole.getId();

        try {
            final DataRole newDataRole = vdb.addDataRole( uow, dataRoleName );
            setDataRoleState( uow, newDataRole, restDataRole );

            final RestVdbDataRole entity = this.entityFactory.create( newDataRole, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );

            return response;
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.VDB_SERVICE_CREATE_DATA_ROLE_ERROR,
                                                                         dataRoleName ),
                                           e );
        }
    }

    private void setDataRoleState( final UnitOfWork uow,
                                   final DataRole dataRole,
                                   final RestVdbDataRole restVdbDataRole ) throws KException {
        { // description
            final String proposed = restVdbDataRole.getDescription();
            final String current = dataRole.getDescription( uow );

            if ( !StringUtils.equals( proposed, current ) ) {
                dataRole.setDescription( uow, proposed );
            }
        }

        { // allowCreateTempTables
            final boolean proposed = restVdbDataRole.isAllowCreateTempTables();
            final boolean current = dataRole.isAllowCreateTempTables( uow );

            if ( proposed != current ) {
                dataRole.setAllowCreateTempTables( uow, proposed );
            }
        }

        { // anyAuthenticated
            final boolean proposed = restVdbDataRole.isAnyAuthenticated();
            final boolean current = dataRole.isAnyAuthenticated( uow );

            if ( proposed != current ) {
                dataRole.setAnyAuthenticated( uow, proposed );
            }
        }

        { // grantAll
            final boolean proposed = restVdbDataRole.isGrantAll();
            final boolean current = dataRole.isGrantAll( uow );

            if ( proposed != current ) {
                dataRole.setGrantAll( uow, proposed );
            }
        }

        { // mappedRoles
            final String[] proposed = restVdbDataRole.getMappedRoles(); // never null
            Arrays.sort( proposed );

            final String[] current = dataRole.getMappedRoles( uow ); // never null
            Arrays.sort( current );

            if ( !Arrays.equals( proposed, current ) ) {
                // remove old
                if ( current.length != 0 ) {
                    for ( final String role : current ) {
                        dataRole.removeMappedRole( uow, role );
                    }
                }

                // add new
                if ( proposed.length != 0 ) {
                    for ( final String role : proposed ) {
                        dataRole.addMappedRole( uow, role );
                    }
                }
            }
        }

        { // permissions
            final RestVdbPermission[] permissions = restVdbDataRole.getPermissions();
            
            if ( permissions.length != 0 ) {
                for ( final RestVdbPermission restPermission : permissions ) {
                    final Permission permission = dataRole.addPermission( uow, restPermission.getName() );
                    permission.setAllowAlter( uow, restPermission.isAllowAlter() );
                    permission.setAllowCreate( uow, restPermission.isAllowCreate() );
                    permission.setAllowDelete( uow, restPermission.isAllowDelete() );
                    permission.setAllowExecute( uow, restPermission.isAllowExecute() );
                    permission.setAllowLanguage( uow, restPermission.isAllowLanguage() );
                    permission.setAllowRead( uow, restPermission.isAllowRead() );
                    permission.setAllowUpdate( uow, restPermission.isAllowUpdate() );
                    // TODO process conditions
                    // TODO process masks
                }
            }
        }
    }

    /**
     * Delete the specified data role from the specified VDB
     * 
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the name of the VDB (cannot be <code>null</code>)
     * @param dataRoleName
     *        the name of the data role to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path( V1Constants.VDB_PLACEHOLDER
           + StringConstants.FORWARD_SLASH
           + V1Constants.DATA_ROLES_SEGMENT
           + StringConstants.FORWARD_SLASH
           + V1Constants.DATA_ROLE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Delete a data role from the VDB" )
    @ApiResponses( value = { @ApiResponse( code = 404, message = "No VDB could be found with name" ),
                             @ApiResponse( code = 404, message = "No data role could be found with name" ),
                             @ApiResponse( code = 406, message = "Only JSON is returned by this operation" ),
                             @ApiResponse( code = 403, message = "An error has occurred." ) } )
    public Response deleteDataRole( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam( value = "Name of the VDB", required = true ) final @PathParam( "vdbName" ) String vdbName,
                                    @ApiParam( value = "Name of the data role to remove", required = true ) final @PathParam( "dataRoleId" ) String dataRoleName ) throws KomodoRestException {
        final SecurityPrincipal principal = checkSecurityContext( headers );
        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final List< MediaType > mediaTypes = headers.getAcceptableMediaTypes();
        if ( !isAcceptable( mediaTypes, MediaType.APPLICATION_JSON_TYPE ) ) {
            return notAcceptableMediaTypesBuilder().build();
        }

        // Error if the Vdb name is missing
        if ( StringUtils.isBlank( vdbName ) ) {
            return createErrorResponseWithForbidden( mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_VDB_NAME );
        }

        // Error if the data role name is missing
        if ( StringUtils.isBlank( dataRoleName ) ) {
            return createErrorResponseWithForbidden( mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_DATA_ROLE_NAME );
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "deleteDataRole", false ); //$NON-NLS-1$

            // Get the specified VDB
            final WorkspaceManager mgr = getWorkspaceManager( uow );
            final KomodoObject kobject = mgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            final Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );

            if ( vdb == null ) {
                return Response.noContent().build();
            }

            final DataRole[] dataRoles = vdb.getDataRoles( uow, dataRoleName );
            if ( dataRoles.length == 0 ) {
                return Response.noContent().build();
            }

            vdb.removeDataRole( uow, dataRoleName );

            final KomodoStatusObject kso = new KomodoStatusObject( RelationalMessages.getString( RelationalMessages.Info.DELETE_STATUS_TITLE ) );
            kso.addAttribute( dataRoleName, RelationalMessages.getString( RelationalMessages.Info.DELETE_STATUS_MSG ) );

            return commit( uow, mediaTypes, kso );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( mediaTypes,
                                                     e,
                                                     RelationalMessages.Error.VDB_SERVICE_DELETE_DATA_ROLE_ERROR,
                                                     dataRoleName );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the named data role belonging to the vdb", response = RestVdbDataRole.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No data role could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataRole( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role to be fetched", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataRole", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null) {
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);
            }

            RestBasicEntity restDataRole = entityFactory.create(dataRole, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getDataRole:data role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restDataRole);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_DATA_ROLE_ERROR, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all ModelSources belonging to the Vdb Model", response = RestVdbModelSource[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No sources could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModelSources( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model to get its Sources", required = true)
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getSources", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            ModelSource[] sources = model.getSources(uow);
            if (sources == null)
                sources = new ModelSource[0];

            List<RestVdbModelSource> restSources = new ArrayList<>(sources.length);
            for (ModelSource source : sources) {
                RestVdbModelSource entity = entityFactory.create(source, uriInfo.getBaseUri(), uow);
                restSources.add(entity);
                LOGGER.debug("getSources:Source from Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restSources );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_SOURCES_ERROR, modelName, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the id of the source being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the ModelSource belonging to the Vdb Model", response = RestVdbModelSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 404, message = "No source could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModelSource( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model", required = true)
                            final @PathParam( "modelName" ) String modelName,
                            @ApiParam(value = "Name of the Model Source to be fetched", required = true)
                            final @PathParam( "sourceName" ) String sourceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getSource", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            ModelSource[] sources = model.getSources(uow);
            if (sources == null || sources.length == 0) {
                LOGGER.debug("getSource:No sources found for model '{0}' in vdb '{1}'", modelName, vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, MODELS_SEGMENT, modelName, SOURCES_SEGMENT);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            //
            // Using model.getSources rather than model.getChild bypasses the VdbModelSources
            // collection object that hangs underneath the model.
            //
            ModelSource source = null;
            for (ModelSource src : sources) {
                if (sourceName.equals(src.getName(uow))) {
                    source = src;
                    break;
                }
            }

            // make sure source exists
            if (source == null) {
                return commit(uow, mediaTypes,
                              new ResourceNotFound(uri(vdbName, MODELS_SEGMENT,
                                                                           modelName, SOURCES_SEGMENT,
                                                                           sourceName),
                                                   Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbModelSource restSource = entityFactory.create(source, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getSource:Source '{0}' from Model '{1}' from VDB '{2}' entity was constructed", //$NON-NLS-1$
                         sourceName, modelName, vdbName);

            return commit( uow, mediaTypes, restSource);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_SOURCE_ERROR, sourceName, modelName, vdbName);
        }
    }

    /**
     * Create a new model source in the specified vdb model.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the id of the source being retrieved (cannot be empty)
     * @param sourceJson
     *        the ModelSource JSON representation (cannot be <code>null</code>)
     * @return the JSON representation of the Model source (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB model or constructing the JSON representation
     */
    @POST
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a ModelSource within a Vdb Model", response = RestVdbModelSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createModelSource( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model", required = true)
                            final @PathParam( "modelName" ) String modelName,
                            @ApiParam(value = "Name of the Model Source to be created", required = true)
                            final @PathParam( "sourceName" ) String sourceName,
                            @ApiParam(
                                      value = "" + 
                                              "JSON of the properties of the Model Source to add:<br>" +
                                              OPEN_PRE_TAG +
                                              OPEN_BRACE + BR +
                                              NBSP + "keng\\_\\_id: \"name of the VdbModelSource\"" + COMMA + BR +
                                              NBSP + "keng\\_\\_dataPath: \"path of VdbModelSource to create\"" + COMMA + BR +
                                              NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\\\\{modelName\\}\\vdb:sources\\\\{sourceName\\}\")" + CLOSE_PRE_CMT + BR +
                                              NBSP + "keng\\_\\_kType: \"VdbModelSource\"" + COMMA + BR +
                                              NBSP + "vdb\\_\\_sourceJndiName: \"the jndi name\"" + COMMA + BR +
                                              NBSP + "vdb\\_\\_sourceTranslator: \"the translator name\"" + BR +
                                              CLOSE_BRACE +
                                              CLOSE_PRE_TAG,
                                      required = true
                            )
                            final String sourceJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the VDB name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_VDB_NAME);
        }

        // Error if the Model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_MODEL_NAME);
        }

        // Error if the ModelSource name is missing
        if (StringUtils.isBlank( sourceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_CREATE_MISSING_MODEL_SOURCE_NAME);
        }

        final RestVdbModelSource restVdbModelSource = KomodoJsonMarshaller.unmarshall( sourceJson, RestVdbModelSource.class );
        final String jsonModelSourceName = restVdbModelSource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonModelSourceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_MODEL_SOURCE_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = sourceName.equals( jsonModelSourceName );
        if ( !namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MODEL_SOURCE_NAME_DONT_MATCH_ERROR, sourceName, jsonModelSourceName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createModelSource", false ); //$NON-NLS-1$
            
            // Get the specified VDB parent
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            KomodoObject kobject = mgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );
            
            if (vdb == null)
                return Response.noContent().build();
            
            Model[] models = vdb.getModels(uow, modelName);
            if (models.length == 0) {
                return Response.noContent().build();
            }
            Model model = models[0];
            // Error if the VDB model already contains a Source with the supplied name.
            if( model.getSources(uow, sourceName).length != 0 ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_MODEL_SOURCE_ALREADY_EXISTS, sourceName);
            }
            
            // create a new ModelSource in the VDB Model
            return doAddModelSource( uow, uriInfo.getBaseUri(), mediaTypes, model, restVdbModelSource );
            
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_MODEL_SOURCE_ERROR, vdbName);
        }
    }
    
    private Response doAddModelSource( final UnitOfWork uow,
                                 final URI baseUri,
                                 final List<MediaType> mediaTypes,
                                 Model model,
                                 final RestVdbModelSource restVdbModelSource ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( model != null );
        assert( restVdbModelSource != null );

        final String sourceName = restVdbModelSource.getId();
        try {
            ModelSource newSource = model.addSource(uow, sourceName);

            // Transfers the properties from the rest object to the created model source
            setProperties(uow, newSource, restVdbModelSource);

            final RestVdbModelSource entity = entityFactory.create(newSource, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.VDB_SERVICE_CREATE_VDB_MODEL_SOURCE_ERROR, sourceName ), e );
        }
    }
    
    /**
     * Update a model source in the specified VDB model.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the id of the source being retrieved (cannot be empty)
     * @param sourceJson
     *        the ModelSource JSON representation (cannot be <code>null</code>)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @PUT
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update the ModelSource belonging to the Vdb Model", response = RestVdbModelSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 404, message = "No source could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateModelSource( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(value = "Name of the Vdb", required = true)
                                       final @PathParam( "vdbName" ) String vdbName,
                                       @ApiParam(value = "Name of the Model", required = true)
                                       final @PathParam( "modelName" ) String modelName,
                                       @ApiParam(value = "Name of the Model Source to be updated", required = true)
                                       final @PathParam( "sourceName" ) String sourceName,
                                       @ApiParam(
                                                 value = "" + 
                                                         "JSON of the properties of the Model Source to update:<br>" +
                                                         OPEN_PRE_TAG +
                                                         OPEN_BRACE + BR +
                                                         NBSP + "keng\\_\\_id: \"name of the VdbModelSource\"" + COMMA + BR +
                                                         NBSP + "keng\\_\\_dataPath: \"path of VdbModelSource to update\"" + COMMA + BR +
                                                         NBSP + OPEN_PRE_CMT + "(eg keng\\_\\_dataPath: \"tko:komodo\\tko:workspace\\\\{username\\}\\\\{vdbName\\}\\\\{modelName\\}\\vdb:sources\\\\{sourceName\\}\")" + CLOSE_PRE_CMT + BR +
                                                         NBSP + "keng\\_\\_kType: \"VdbModelSource\"" + COMMA + BR +
                                                         NBSP + "vdb\\_\\_sourceJndiName: \"the jndi name\"" + COMMA + BR +
                                                         NBSP + "vdb\\_\\_sourceTranslator: \"the translator name\"" + BR +
                                                         CLOSE_BRACE +
                                                         CLOSE_PRE_TAG,
                                                 required = true
                                       )
                                       final String sourceJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_VDB_NAME);
        }

        // Error if the model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_MODEL_NAME);
        }

        // Error if the modelSource name is missing
        if (StringUtils.isBlank( sourceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_MISSING_MODEL_SOURCE_NAME);
        }

        final RestVdbModelSource restVdbModelSource = KomodoJsonMarshaller.unmarshall( sourceJson, RestVdbModelSource.class );
        final String jsonModelSourceName = restVdbModelSource.getId();
        // Error if the name is missing from the supplied json body
        if ( StringUtils.isBlank( jsonModelSourceName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_MISSING_JSON_MODEL_SOURCE_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateModelSource", false ); //$NON-NLS-1$

            // Get the specified VDB parent
            KomodoObject kobject = getWorkspaceManager(uow).getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = getWorkspaceManager(uow).resolve( uow, kobject, Vdb.class );
            
            if (vdb == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_DNE);
            }
            
            // Error if the VDB already contains a Model with the supplied name.
            Model[] models = vdb.getModels(uow, modelName);
            if( models.length == 0 ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_MODEL_DNE);
            }
            Model model = models[0];

            // Error if the VDB Model already contains a Source with the supplied name.
            ModelSource[] sources = model.getSources(uow, sourceName);
            if( sources.length == 0 ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_VDB_MODEL_SOURCE_ALREADY_EXISTS, modelName);
            }

            // Transfers the properties from the rest object to the model
            setProperties(uow, sources[0], restVdbModelSource);

            KomodoProperties properties = new KomodoProperties();
            final RestVdbModelSource entity = entityFactory.create(sources[0], uriInfo.getBaseUri(), uow, properties);
            LOGGER.debug("updateVdbModelSource: VdbModelSource '{0}' entity was updated", sources[0].getName(uow)); //$NON-NLS-1$
            final Response response = commit( uow, headers.getAcceptableMediaTypes(), entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_UPDATE_VDB_MODEL_SOURCE_ERROR);
        }
    }
    
    /**
     * Delete the ModelSource in the specified Vdb Model
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param sourceName
     *        the id of the source being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @DELETE
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.SOURCES_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.SOURCE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete the ModelSource in the specified Vdb Model", response = RestVdbModelSource.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 404, message = "No source could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteModelSource( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model", required = true)
                            final @PathParam( "modelName" ) String modelName,
                            @ApiParam(value = "Name of the Model Source to be deleted", required = true)
                            final @PathParam( "sourceName" ) String sourceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the Vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_VDB_NAME);
        }
        
        // Error if the Model name is missing
        if (StringUtils.isBlank( modelName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_MODEL_NAME);
        }
        
        // Error if the ModelSource name is missing
        if (StringUtils.isBlank( sourceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_SERVICE_DELETE_MISSING_MODEL_SOURCE_NAME);
        }
        
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeModelSourceFromModel", false); //$NON-NLS-1$

            // Get the specified VDB parent
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            KomodoObject kobject = mgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb vdb = mgr.resolve( uow, kobject, Vdb.class );
            
            if (vdb == null)
                return Response.noContent().build();
            
            Model[] models = vdb.getModels(uow, modelName);
            if(models.length==0) {
                return Response.noContent().build();
            }
            Model model = models[0];
            
            model.removeSource(uow, sourceName);
            
            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(modelName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_DELETE_VDB_MODEL_SOURCE_ERROR);
        }
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @return the JSON representation of the Model tables (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.TABLES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all tables of the model belonging to the vdb", response = RestVdbModelTable[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No tables could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModelTables( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model to get its tables", required = true)
                            final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getTables", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            Table[] tables = model.getTables(uow);
            if (tables == null)
            	tables = new Table[0];

            List<RestVdbModelTable> restTables = new ArrayList<>(tables.length);
            for (Table table : tables) {
                RestVdbModelTable entity = entityFactory.create(table, uriInfo.getBaseUri(), uow);
                restTables.add(entity);
                LOGGER.debug("getTables:Table from Model '{0}' from VDB '{1}' entity was constructed", modelName, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restTables );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_TABLES_ERROR, vdbName, modelName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param modelName
     *        the id of the model being retrieved (cannot be empty)
     * @param tableName
     *        the id of the table being retrieved (cannot be empty)
     * @return the JSON representation of the Table Columns (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.TABLES_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.TABLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.COLUMNS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all columns of the specified vdb model table", response = RestVdbModelTableColumn[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 200, message = "No tables could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getModelTableColumns( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Model", required = true)
                            final @PathParam( "modelName" ) String modelName,
        	                @ApiParam(value = "Name of the Table to get its columns", required = true)
                            final @PathParam( "tableName" ) String tableName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getTables", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            Model model = findModel(uow, mediaTypes, modelName, vdb);
            if (model == null) {
                return commitNoModelFound(uow, mediaTypes, modelName, vdbName);
            }

            Table table = findTable(uow, mediaTypes, tableName, model);
            if (table == null) {
                return commitNoTableFound(uow, mediaTypes, tableName, modelName, vdbName);
            }

            Column[] columns = table.getColumns(uow);
            if (columns == null)
            	columns = new Column[0];

            List<RestVdbModelTableColumn> restColumns = new ArrayList<>(columns.length);
            for (Column column : columns) {
                RestVdbModelTableColumn entity = entityFactory.create(column, uriInfo.getBaseUri(), uow);
                restColumns.add(entity);
                LOGGER.debug("getColumns:Column from Table '{0}' from Model '{1}' entity was constructed", tableName, modelName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restColumns );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_COLUMNS_ERROR, vdbName, modelName, tableName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permissions (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find all permissions belonging to the vdb data role", response = RestVdbPermission[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb or data role could be found with given names"),
        @ApiResponse(code = 200, message = "No permissions could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getPermissions( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role to get its permissions", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getPermissions", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null) {
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);
            }

            Permission[] permissions = dataRole.getPermissions(uow);
            if (permissions == null)
                permissions = new Permission[0];

            List<RestVdbPermission> restPermissions = new ArrayList<>(permissions.length);
            for (Permission permission : permissions) {
                RestVdbPermission entity = entityFactory.create(permission, uriInfo.getBaseUri(), uow);
                restPermissions.add(entity);
                LOGGER.debug("getPermissions:Permission from Data role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restPermissions);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_PERMISSIONS_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the named permission belonging to the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getPermission( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Name of the Permission to be fetched", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getPermission", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            RestVdbPermission restPermission = entityFactory.create(permission, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getPermission:permission '{0}' from data role '{1}' from VDB '{2}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restPermission);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_PERMISSION_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission's conditions (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the conditions belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with given names"),
        @ApiResponse(code = 200, message = "No conditions could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConditions( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Name of the Permission to get its Conditions", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getConditions", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Condition[] conditions = permission.getConditions(uow);
            List<RestVdbCondition> restConditions = new ArrayList<>(conditions.length);
            for (Condition condition : conditions) {
                RestVdbCondition entity = entityFactory.create(condition, uriInfo.getBaseUri(), uow);
                restConditions.add(entity);
                LOGGER.debug("getConditions:Condition from Permission from Data Role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restConditions);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_CONDITIONS_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @param conditionId
     *        the id of the condition being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission condition (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.CONDITION_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the condition belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role, permission or condition could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getCondition( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Name of the Permission", required = true)
                            final @PathParam( "permissionId" ) String permissionId,
                            @ApiParam(value = "Name of the Condition to be fetched", required = true)
                            final @PathParam( "conditionId" ) String conditionId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getCondition", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Condition[] conditions = permission.getConditions(uow);
            if (conditions == null || conditions.length == 0) {
                LOGGER.debug("getConition:No conditions found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, DATA_ROLES_SEGMENT,
                                                          dataRoleId, PERMISSIONS_SEGMENT,
                                                          permissionId, CONDITIONS_SEGMENT,
                                                          conditionId);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Condition condition = null;
            for (Condition con : conditions) {
                if (conditionId.equals(con.getName(uow))) {
                    condition = con;
                    break;
                }
            }

            if (condition == null) {
                LOGGER.debug("No condition '{0}' for permission '{1}' for data role '{2}' found for vdb '{3}'", //$NON-NLS-1$
                             conditionId, permissionId, dataRoleId, vdbName);
                return commit(uow, mediaTypes, new ResourceNotFound(
                                                                    uri(vdbName, DATA_ROLES_SEGMENT,
                                                                        dataRoleId, PERMISSIONS_SEGMENT,
                                                                        permissionId, CONDITIONS_SEGMENT,
                                                                        conditionId),
                                                                    Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbCondition restCondition = entityFactory.create(condition, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getCondition:condition '{0}' from permission '{1}' from data role '{2}' from VDB '{3}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restCondition);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_CONDITION_ERROR, conditionId, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission's masks (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MASKS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the masks belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role or permission could be found with given names"),
        @ApiResponse(code = 200, message = "No masks could be found but an empty list is returned"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getMasks( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Name of the Permission to get its Masks", required = true)
                            final @PathParam( "permissionId" ) String permissionId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getMasks", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Mask[] masks = permission.getMasks(uow);
            List<RestVdbMask> restMasks = new ArrayList<>(masks.length);
            for (Mask mask : masks) {
                RestVdbMask entity = entityFactory.create(mask, uriInfo.getBaseUri(), uow);
                restMasks.add(entity);
                LOGGER.debug("getMasks:Mask from Permission from Data Role '{0}' from VDB '{1}' entity was constructed", dataRoleId, vdbName); //$NON-NLS-1$
            }

            return commit( uow, mediaTypes, restMasks);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_MASKS_ERROR, permissionId, dataRoleId, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @param dataRoleId
     *        the id of the data role being retrieved (cannot be empty)
     * @param permissionId
     *        the id of the permission being retrieved (cannot be empty)
     * @param maskId
     *        the id of the mask being retrieved (cannot be empty)
     * @return the JSON representation of the VDB data role permission mask (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLES_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.DATA_ROLE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSIONS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.PERMISSION_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                V1Constants.MASKS_SEGMENT + StringConstants.FORWARD_SLASH +
                V1Constants.MASK_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the mask belonging to the permission of the data role of the vdb", response = RestVdbPermission.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb, data role, permission or mask could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getMask( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Name of the Vdb", required = true)
                            final @PathParam( "vdbName" ) String vdbName,
                            @ApiParam(value = "Name of the Data Role", required = true)
                            final @PathParam( "dataRoleId" ) String dataRoleId,
                            @ApiParam(value = "Name of the Permission", required = true)
                            final @PathParam( "permissionId" ) String permissionId,
                            @ApiParam(value = "Name of the Mask to be fetched", required = true)
                            final @PathParam( "maskId" ) String maskId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getMask", true ); //$NON-NLS-1$

            Vdb vdb = findVdb(uow, vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);

            DataRole dataRole = findDataRole(uow, mediaTypes, dataRoleId, vdb);
            if (dataRole == null)
                return commitNoDataRoleFound(uow, mediaTypes, dataRoleId, vdbName);

            Permission permission = findPermission(uow, mediaTypes, permissionId, dataRole, vdb);
            if (permission == null)
                return commitNoPermissionFound(uow, mediaTypes, permissionId, dataRoleId, vdbName);

            Mask[] masks = permission.getMasks(uow);
            if (masks == null || masks.length == 0) {
                LOGGER.debug("getConition:No masks found for vdb '{0}'", vdbName); //$NON-NLS-1$
                String resourceName = uri(vdbName, DATA_ROLES_SEGMENT,
                                                          dataRoleId, PERMISSIONS_SEGMENT,
                                                          permissionId, MASKS_SEGMENT,
                                                          maskId);
                return commit(uow, mediaTypes, new ResourceNotFound(resourceName, Messages.getString(GET_OPERATION_NAME)));
            }

            Mask mask = null;
            for (Mask con : masks) {
                if (maskId.equals(con.getName(uow))) {
                    mask = con;
                    break;
                }
            }

            if (mask == null) {
                LOGGER.debug("No mask '{0}' for permission '{1}' for data role '{2}' found for vdb '{3}'", //$NON-NLS-1$
                             maskId, permissionId, dataRoleId, vdbName);
                return commit(uow, mediaTypes, new ResourceNotFound(
                                                                    uri(vdbName, DATA_ROLES_SEGMENT,
                                                                        dataRoleId, PERMISSIONS_SEGMENT,
                                                                        permissionId, MASKS_SEGMENT,
                                                                        maskId),
                                                                    Messages.getString( GET_OPERATION_NAME)));
            }

            RestVdbMask restMask = entityFactory.create(mask, uriInfo.getBaseUri(), uow);
            LOGGER.debug("getMask:mask '{0}' from permission '{1}' from data role '{2}' from VDB '{3}' entity was constructed", permissionId, dataRoleId, vdbName); //$NON-NLS-1$

            return commit( uow, mediaTypes, restMask);

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, VDB_SERVICE_GET_MASK_ERROR, maskId, permissionId, dataRoleId, vdbName);
        }
    }

	/**
	 * @param headers
	 *            the request headers (never <code>null</code>)
	 * @param uriInfo
	 *            the request URI information (never <code>null</code>)
	 * @param vdbName
	 *            the VDB name being validated (cannot be empty)
	 * @return the response (never <code>null</code>) with an entity that is
	 *         either an empty string, when the name is valid, or an error
	 *         message
	 * @throws KomodoRestException
	 *             if there is a problem validating the VDB name or constructing
	 *             the response
	 */
    @GET
    @Path( V1Constants.NAME_VALIDATION_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the VDB name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 500, message = "The VDB name cannot be empty." )
    } )
    public Response validateVdbName( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam( value = "The VDB name being checked", required = true )
                                     final @PathParam( "vdbName" ) String vdbName ) throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName( vdbName );
        
        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "validateVdbName", true ); //$NON-NLS-1$

            // make sure an existing VDB does not have that name
            final Vdb vdb = findVdb( uow, vdbName );

            if ( vdb == null ) {
                // make sure an existing connection does not have the same name
                final Connection ds = findConnection( uow, vdbName );

                if ( ds == null ) {
                    // name is valid
                    return Response.ok().build();
                }

                // name is the same as an existing connection
                return Response.ok()
                               .entity( RelationalMessages.getString( VDB_DATA_SOURCE_NAME_EXISTS ) )
                               .build();
            }

            // name is the same as an existing VDB
            return Response.ok()
                           .entity( RelationalMessages.getString( VDB_NAME_EXISTS ) )
                           .build();
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( headers.getAcceptableMediaTypes(), 
                                                     e, 
                                                     VDB_NAME_VALIDATION_ERROR );
        }
    }
}
