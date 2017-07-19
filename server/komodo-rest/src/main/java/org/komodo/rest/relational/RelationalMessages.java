/*************************************************************************************
* JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.rest.relational;

import static org.komodo.spi.constants.StringConstants.CLOSE_ANGLE_BRACKET;
import static org.komodo.spi.constants.StringConstants.DOT;
import static org.komodo.spi.constants.StringConstants.OPEN_ANGLE_BRACKET;
import java.util.ArrayList;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.repository.KomodoType;

/**
 * Localized messages for the {@code server rest} project.
 */
public final class RelationalMessages {

    public enum Info {
        
        /**
         * A successfully deleted message.
         */
        DELETE_STATUS_MSG,
        
        /**
         * The title of a delete status.
         */
        DELETE_STATUS_TITLE,
        
        /**
         * Tag for VDB Active Status
         */
        VDB_STATUS_ACTIVE,

        /**
         * Tag for VDB Error Status
         */
        VDB_STATUS_ERROR,

        /**
         * Tag for VDB Loading Status
         */
        VDB_STATUS_LOADING,

        /**
         * Tag for VDB New Status
         */
        VDB_STATUS_NEW,

        /**
         * Tag for VDB Unknown Status
         */
        VDB_STATUS_UNKNOWN,

        /**
         * Message for VDB Active Status
         */
        VDB_STATUS_MSG_ACTIVE,

        /**
         * Message for VDB Error Status
         */
        VDB_STATUS_MSG_ERROR,

        /**
         * Message for VDB Loading Status
         */
        VDB_STATUS_MSG_LOADING,

        /**
         * Message for VDB New Status
         */
        VDB_STATUS_MSG_NEW,

        /**
         * Message for VDB Unknown Status
         */
        VDB_STATUS_MSG_UNKNOWN,

        /**
         * Message for VDB already deployed message
         */
        VDB_ALREADY_DEPLOYED,

        /**
         * Message for VDB already deployed by owner message
         */
        VDB_ALREADY_DEPLOYED_OWNER,

        /**
         * Title for the driver deployment status
         */
        DRIVER_DEPLOYMENT_STATUS_TITLE,

        /**
         * Driver successfully uploaded
         */
        DRIVER_SUCCESSFULLY_UPLOADED,

        /**
         * Driver successfully deployed
         */
        DRIVER_SUCCESSFULLY_DEPLOYED,

        /**
         * Driver undeployment request sent but not yet undeployed
         */
        DRIVER_UNDEPLOYMENT_REQUEST_SENT,

        /**
         * Driver successfully deployed
         */
        DRIVER_SUCCESSFULLY_UNDEPLOYED,
        
        /**
         * An import export service import success message
         */
        IMPORT_EXPORT_SERVICE_IMPORT_SUCCESS_MESSAGE,
        
        /**
         * VDB undeployment request sent but not yet undeployed
         */
        VDB_UNDEPLOYMENT_REQUEST_SENT,

        /**
         * Vdb successfully deployed
         */
        VDB_SUCCESSFULLY_UNDEPLOYED,

        /**
         * Connection undeployment request sent but not yet undeployed
         */
        CONNECTION_UNDEPLOYMENT_REQUEST_SENT,

        /**
         * Connection successfully deployed
         */
        CONNECTION_SUCCESSFULLY_UNDEPLOYED,
        
        /**
         * Data service deployable status title
         */
        DATA_SERVICE_DEPLOYABLE_STATUS_TITLE,

        /**
         * Data service status title
         */
        DATA_SERVICE_DEPLOYMENT_STATUS_TITLE,

        /**
         * Data service successfully deployed
         */
        DATA_SERVICE_SUCCESSFULLY_DEPLOYED,

        /**
         * Data service deployed with errors
         */
        DATA_SERVICE_DEPLOYED_WITH_ERRORS,

        /**
         * Connection status title
         */
        CONNECTION_DEPLOYMENT_STATUS_TITLE,

        /**
         * Connection successfully deployed
         */
        CONNECTION_SUCCESSFULLY_DEPLOYED,

        /**
         * Connection deployed with errors
         */
        CONNECTION_DEPLOYED_WITH_ERRORS,

        /**
         * Vdb status title
         */
        VDB_DEPLOYMENT_STATUS_TITLE,

        /**
         * Vdb successfully deployed
         */
        VDB_SUCCESSFULLY_DEPLOYED,

        /**
         * Vdb deployed with errors
         */
        VDB_DEPLOYED_WITH_ERRORS,

        /**
         * Vdb transfer to repo status title
         */
        VDB_TO_REPO_STATUS_TITLE,

        /**
         * Vdb transfer to repo success
         */
        VDB_TO_REPO_SUCCESS,

        /**
         * Connection transfer to repo status title
         */
        CONNECTION_TO_REPO_STATUS_TITLE,

        /**
         * Connection transfer to repo success
         */
        CONNECTION_TO_REPO_SUCCESS;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }
    }

    /**
     * Messages relating to errors.
     */
    public enum Error {

        /**
         * An error indicating a failure in authentication or authorization of the REST service
         */
        SECURITY_FAILURE_ERROR,

        /**
         * An error indicating a JSON document representing the Dataservices in the workspace could not be retrieved.
         */
        DATASERVICE_SERVICE_GET_DATASERVICES_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified Dataservice.
         */
        DATASERVICE_SERVICE_GET_DATASERVICE_ERROR,

        /**
         * An error indicating an error occurred trying to obtain a dataservice's connections
         */
        DATASERVICE_SERVICE_GET_CONNECTIONS_ERROR,

        /**
         * An error indicating an error due to missing tablePath
         */
        DATASERVICE_SERVICE_GET_JOIN_MISSING_TABLEPATH,
        
        /**
         * An error indicating an error occurred trying to obtain a dataservice's drivers
         */
        DATASERVICE_SERVICE_GET_DRIVERS_ERROR,

        /**
         * An error indicating a request to create a dataservice failed
         */
        DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR,

        /**
         * An error indicating a request to clone a dataservice failed
         */
        DATASERVICE_SERVICE_CLONE_DATASERVICE_ERROR,

        /**
         * An error indicating a request to delete a dataservice failed
         */
        DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR,
        
        /**
         * An error indicating a request to update a dataservice failed
         */
        DATASERVICE_SERVICE_UPDATE_DATASERVICE_ERROR,

        /**
         * An error indicating create attempt was missing a name
         */
        DATASERVICE_SERVICE_CREATE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a name
         */
        DATASERVICE_SERVICE_CLONE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a new service name
         */
        DATASERVICE_SERVICE_CLONE_MISSING_NEW_NAME,
 
        /**
         * An error indicating the desired new clone name is same as dataservice being cloned
         */
        DATASERVICE_SERVICE_CLONE_SAME_NAME_ERROR,
        
        /**
         * An error indicating an error occurred trying to find a service view info
         */
        DATASERVICE_SERVICE_FIND_VIEW_INFO_ERROR,

        /**
         * An error indicating an error occurred trying to find a matching source VDB
         */
        DATASERVICE_SERVICE_FIND_SOURCE_VDB_ERROR,

        /**
         * The Dataservice service cannot parse the request body
         */
        DATASERVICE_SERVICE_REQUEST_PARSING_ERROR,

        /**
         * An error indicating update attempt was missing a name
         */
        DATASERVICE_SERVICE_UPDATE_MISSING_NAME,
        
        /**
         * An error indicating update attempt was missing json arg
         */
        DATASERVICE_SERVICE_UPDATE_MISSING_JSON,
 
        /**
         * An error indicating the Dataservice name is missing from the input JSON document.
         */
        DATASERVICE_SERVICE_JSON_MISSING_NAME,

        /**
         * An error indicating that the service does not exist
         */
        DATASERVICE_SERVICE_SERVICE_DNE,
        
        /**
         * An error indicating the parameter and JSON dataservice name does not match for a dataservice being created.
         */
        DATASERVICE_SERVICE_SERVICE_NAME_ERROR,

        /**
         * An error indicating create attempt failed because same name already exists
         */
        DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS,
        
        /**
         * An error indicating clone attempt failed because same name already exists
         */
        DATASERVICE_SERVICE_CLONE_ALREADY_EXISTS,
        
        /**
         * An error indicating that a table does not exist
         */
        DATASERVICE_SERVICE_SOURCE_TABLE_DNE,
        
        /**
         * An error indicating that a model source does not exist
         */
        DATASERVICE_SERVICE_MODEL_SOURCE_DNE,
        
        /**
         * The dataservice service lacks at least one parameter
         */
        DATASERVICE_SERVICE_MISSING_PARAMETER_ERROR,

        /**
         * An error indicating set attempt failed because the dataservice name was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_NAME,
        
        /**
         * An error indicating set attempt failed because the tablePath was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_TABLEPATH,
        
        /**
         * An error indicating set attempt failed because the modelSource path was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_MODELSOURCE_PATH,
        
        /**
         * An error indicating set attempt failed because the Join type was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_JOIN_TYPE,
        
        /**
         * An error indicating set attempt failed because the Join criteria lh column was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_JOIN_LH_COLUMN,
        
        /**
         * An error indicating set attempt failed because the Join criteria rh column was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_JOIN_RH_COLUMN,
        
        /**
         * An error indicating set attempt failed because the expected view ddl was missing
         */
        DATASERVICE_SERVICE_SET_SERVICE_MISSING_VIEWDDL,
        
        /**
         * An error indicating set attempt failed
         */
        DATASERVICE_SERVICE_SET_SERVICE_ERROR,

        /**
         * A message indicating that a data service with the given name already exists.
         */
        DATASERVICE_SERVICE_NAME_EXISTS,

        /**
         * A message indicating an unexpected error occurred during name validation.
         */
        DATASERVICE_SERVICE_NAME_VALIDATION_ERROR,

        /**
         * An error indicating a JSON document representing the Connections in the workspace could not be retrieved.
         */
        CONNECTION_SERVICE_GET_CONNECTIONS_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified Connection.
         */
        CONNECTION_SERVICE_GET_CONNECTION_ERROR,

        /**
         * An error indicating a request to create a connection failed
         */
        CONNECTION_SERVICE_CREATE_CONNECTION_ERROR,

        /**
         * An error indicating a request to clone a connection failed
         */
        CONNECTION_SERVICE_CLONE_CONNECTION_ERROR,

        /**
         * An error indicating a request to delete a connection failed
         */
        CONNECTION_SERVICE_DELETE_CONNECTION_ERROR,
        
        /**
         * An error indicating a request to update a connection failed
         */
        CONNECTION_SERVICE_UPDATE_CONNECTION_ERROR,

        /**
         * An error indicating create attempt was missing a name
         */
        CONNECTION_SERVICE_CREATE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a name
         */
        CONNECTION_SERVICE_CLONE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a new connection name
         */
        CONNECTION_SERVICE_CLONE_MISSING_NEW_NAME,
 
        /**
         * An error indicating the desired new clone name is same as connection being cloned
         */
        CONNECTION_SERVICE_CLONE_SAME_NAME_ERROR,
        
        /**
         * An error indicating update attempt was missing a name
         */
        CONNECTION_SERVICE_UPDATE_MISSING_NAME,
        
        /**
         * An error indicating that the connection does not exist
         */
        CONNECTION_SERVICE_UPDATE_SOURCE_DNE,
        
        /**
         * An error indicating update attempt was missing json arg
         */
        CONNECTION_SERVICE_UPDATE_MISSING_JSON,
 
        /**
         * An error indicating the connection name is missing from the input JSON document.
         */
        CONNECTION_SERVICE_JSON_MISSING_NAME,

        /**
         * An error indicating the parameter and JSON connection name does not match for a connection being created.
         */
        CONNECTION_SERVICE_SOURCE_NAME_ERROR,

        /**
         * An error indicating create attempt failed because same name already exists
         */
        CONNECTION_SERVICE_CREATE_ALREADY_EXISTS,
        
        /**
         * An error indicating clone attempt failed because same name already exists
         */
        CONNECTION_SERVICE_CLONE_ALREADY_EXISTS,

        /**
         * An error indicating a connection already exists
         */
        CONNECTION_SERVICE_NAME_EXISTS,

        /**
         * A message indicating an unexpected error occurred during name validation.
         */
        CONNECTION_SERVICE_NAME_VALIDATION_ERROR,

        /**
         * Error transferring connections from server to repo
         */
        CONNECTION_TO_REPO_IMPORT_ERROR,

        /**
         * An error indicating a JSON document representing the Drivers in the workspace could not be retrieved.
         */
        DRIVER_SERVICE_GET_DRIVERS_ERROR,

        /**
         * A message indicating the VDB is invalid since a data source with the given name already exists.
         */
        VDB_DATA_SOURCE_NAME_EXISTS,

        /**
         * A message indicating that a VDB with the given name already exists.
         */
        VDB_NAME_EXISTS,

        /**
         * A message indicating an unexpected error occurred during name validation.
         */
        VDB_NAME_VALIDATION_ERROR,

        /**
         * An error indicating the VDB descriptor JSON representation could not be created.
         */
        VDB_DESCRIPTOR_BUILDER_ERROR,

        /**
         * An error indicating a VDB data role could not be created.
         */
        VDB_SERVICE_CREATE_DATA_ROLE_ERROR,
        
        /**
         * An error indicating create attempt was missing a VDB data role name.
         */
        VDB_SERVICE_CREATE_MISSING_DATA_ROLE_NAME,

        /**
         * An error indicating create attempt was missing a VDB name
         */
        VDB_SERVICE_CREATE_MISSING_VDB_NAME,
 
        /**
         * An error indicating create attempt was missing a Model name
         */
        VDB_SERVICE_CREATE_MISSING_MODEL_NAME,
 
        /**
         * An error indicating create attempt was missing a ModelSource name
         */
        VDB_SERVICE_CREATE_MISSING_MODEL_SOURCE_NAME,
 
        /**
         * An error indicating a VDB could not be created.
         */
        VDB_SERVICE_CREATE_VDB_ERROR,

        /**
         * An error indicating a VDB model could not be created.
         */
        VDB_SERVICE_CREATE_VDB_MODEL_ERROR,

        /**
         * An error indicating a VDB model source could not be created.
         */
        VDB_SERVICE_CREATE_VDB_MODEL_SOURCE_ERROR,

        /**
         * An error indicating a request to clone a VDB failed
         */
        VDB_SERVICE_CLONE_VDB_ERROR,
        
        /**
         * An error indicating clone attempt failed because same name already exists
         */
        VDB_SERVICE_CLONE_ALREADY_EXISTS,
        
        /**
         * An error indicating clone attempt was missing a name
         */
        VDB_SERVICE_CLONE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a new VDB name
         */
        VDB_SERVICE_CLONE_MISSING_NEW_NAME,
 
        /**
         * An error indicating the desired new clone name is same as VDB being cloned
         */
        VDB_SERVICE_CLONE_SAME_NAME_ERROR,
        
        /**
         * An error indicating a data role with the specified name already exists.
         */
        VDB_SERVICE_DATA_ROLE_ALREADY_EXISTS,
        
        /**
         * An error indicating the specified VDB data role could not be deleted.
         */
        VDB_SERVICE_DELETE_DATA_ROLE_ERROR,
        
        /**
         * An error indicating a VDB could not be deleted.
         */
        VDB_SERVICE_DELETE_VDB_ERROR,

        /**
         * An error indicating a VDB model could not be deleted.
         */
        VDB_SERVICE_DELETE_VDB_MODEL_ERROR,

        /**
         * An error indicating a VDB model source could not be deleted.
         */
        VDB_SERVICE_DELETE_VDB_MODEL_SOURCE_ERROR,

        /**
         * An error indicating a request to update a vdb failed
         */
        VDB_SERVICE_UPDATE_VDB_ERROR,

        /**
         * An error indicating a request to update a vdb failed
         */
        VDB_SERVICE_UPDATE_VDB_MODEL_ERROR,

        /**
         * An error indicating a request to update a vdb model source failed
         */
        VDB_SERVICE_UPDATE_VDB_MODEL_SOURCE_ERROR,

        /**
         * An error indicating a JSON document representing the VDBs in the workspace could not be retrieved.
         */
        VDB_SERVICE_GET_VDBS_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified VDB.
         */
        VDB_SERVICE_GET_VDB_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's models.
         */
        VDB_SERVICE_GET_MODELS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a specific model from a VDB
         */
        VDB_SERVICE_GET_MODEL_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB Model's sources
         */
        VDB_SERVICE_GET_SOURCES_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB Model's source
         */
        VDB_SERVICE_GET_SOURCE_ERROR,

        /**
         * An error indicating an error occurred trying to obain a Models tables
         */
        VDB_SERVICE_GET_TABLES_ERROR,

        /**
         * An error indicating an error occurred trying to obain a Tables columns
         */
        VDB_SERVICE_GET_COLUMNS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's translators
         */
        VDB_SERVICE_GET_TRANSLATORS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's translator
         */
        VDB_SERVICE_GET_TRANSLATOR_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's imports
         */
        VDB_SERVICE_GET_IMPORTS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's import
         */
        VDB_SERVICE_GET_IMPORT_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data roles
         */
        VDB_SERVICE_GET_DATA_ROLES_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data role
         */
        VDB_SERVICE_GET_DATA_ROLE_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data role permissions
         */
        VDB_SERVICE_GET_PERMISSIONS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a data role's permission
         */
        VDB_SERVICE_GET_PERMISSION_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's conditions
         */
        VDB_SERVICE_GET_CONDITIONS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's condition
         */
        VDB_SERVICE_GET_CONDITION_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's masks
         */
        VDB_SERVICE_GET_MASKS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's mask
         */
        VDB_SERVICE_GET_MASK_ERROR,

        /**
         * An error indicating the VDB data role name is missing from the input JSON document.
         */
        VDB_SERVICE_MISSING_JSON_DATA_ROLE_NAME,
        
        /**
         * An error indicating the parameter and JSON VDB data role name does not match.
         */
        VDB_SERVICE_DATA_ROLE_NAME_DONT_MATCH_ERROR,

        /**
         * An error when creating a VDB indicating the input VDB JSON document was missing.
         */
        VDB_SERVICE_MISSING_JSON_VDB,

        /**
         * An error indicating the VDB name is missing from the input JSON document.
         */
        VDB_SERVICE_MISSING_JSON_VDB_NAME,

        /**
         * An error indicating the Model name is missing from the input JSON document.
         */
        VDB_SERVICE_MISSING_JSON_MODEL_NAME,

        /**
         * An error indicating the ModelSource name is missing from the input JSON document.
         */
        VDB_SERVICE_MISSING_JSON_MODEL_SOURCE_NAME,

        /**
         * An error indicating update attempt was missing a VDB name
         */
        VDB_SERVICE_UPDATE_MISSING_VDB_NAME,
        
        /**
         * An error indicating update attempt was missing a Model name
         */
        VDB_SERVICE_UPDATE_MISSING_MODEL_NAME,
        
        /**
         * An error indicating update attempt was missing a ModelSource name
         */
        VDB_SERVICE_UPDATE_MISSING_MODEL_SOURCE_NAME,
        
        /**
         * An error indicating that the vdb does not exist
         */
        VDB_SERVICE_UPDATE_VDB_DNE,
        
        /**
         * An error indicating that the Vdb model does not exist
         */
        VDB_SERVICE_UPDATE_VDB_MODEL_DNE,
        
        /**
         * An error indicating a VDB with the specified name already exists and therefore cannot be created.
         */
        VDB_SERVICE_VDB_ALREADY_EXISTS,

        /**
         * An error indicating a VDB model with the specified name already exists and therefore cannot be created.
         */
        VDB_SERVICE_VDB_MODEL_ALREADY_EXISTS,

        /**
         * An error indicating a VDB ModelSource with the specified name already exists and therefore cannot be created.
         */
        VDB_SERVICE_VDB_MODEL_SOURCE_ALREADY_EXISTS,

        /**
         * An error indicating the parameter and JSON VDB name does not match.
         */
        VDB_SERVICE_VDB_NAME_DONT_MATCH_ERROR,

        /**
         * An error indicating the parameter and JSON Model name does not match.
         */
        VDB_SERVICE_MODEL_NAME_DONT_MATCH_ERROR,

        /**
         * An error indicating the parameter and JSON ModelSource name does not match.
         */
        VDB_SERVICE_MODEL_SOURCE_NAME_DONT_MATCH_ERROR,
        
        /**
         * An error indicating delete attempt was missing a VDB data role name.
         */
        VDB_SERVICE_DELETE_MISSING_DATA_ROLE_NAME,

        /**
         * An error indicating delete attempt was missing a VDB name
         */
        VDB_SERVICE_DELETE_MISSING_VDB_NAME,
        
        /**
         * An error indicating delete attempt was missing a Model name
         */
        VDB_SERVICE_DELETE_MISSING_MODEL_NAME,
        
        /**
         * An error indicating delete attempt was missing a ModelSource name
         */
        VDB_SERVICE_DELETE_MISSING_MODEL_SOURCE_NAME,
        
        /**
         * An error indicating an exception occurred while importing a sample vdb
         */
        VDB_SERVICE_LOAD_SAMPLE_ERROR,

        /**
         * An error indicating the content of a sample vdb could not be loaded into a stream
         */
        VDB_SAMPLE_CONTENT_FAILURE,

        /**
         * Success indicator that the content of a sample vdb was loaded into a stream
         */
        VDB_SAMPLE_CONTENT_SUCCESS,

        /**
         * An error indicating the transaction timeout while awaiting the import of a vdb
         */
        VDB_SAMPLE_IMPORT_TIMEOUT,

        /**
         * Success indicator that the import of a sample vdb was successful
         */
        VDB_SAMPLE_IMPORT_SUCCESS,

        /**
         * Import errors occurred during sample vdb import
         */
        VDB_SAMPLE_IMPORT_ERRORS,

        /**
         * Vdb already exists during sample import
         */
        VDB_SAMPLE_IMPORT_VDB_EXISTS,

        /**
         * Error transferring vdb from server to repo
         */
        VDB_TO_REPO_IMPORT_ERROR,
        
        /**
         * An error occurred while trying to obtain the teiid schema
         */
        SCHEMA_SERVICE_GET_SCHEMA_ERROR,

        /**
         * An unknown {@link KomodoType} was provided to the get schema operation
         */
        SCHEMA_SERVICE_GET_SCHEMA_UNKNOWN_KTYPE,

        /**
         * The schema was not found
         */
        SCHEMA_SERVICE_GET_SCHEMA_NOT_FOUND,

        /**
         * An error indicating a VDB search failed
         */
        SEARCH_SERVICE_GET_SEARCH_ERROR,

        /**
         * An error indicating a request for saved searched failed
         */
        SEARCH_SERVICE_WKSP_SEARCHES_ERROR,

        /**
         * An error indicating a request to save a search configuration failed
         */
        SEARCH_SERVICE_SAVE_SEARCH_ERROR,

        /**
         * An error indicating a request to delete a saved search configuration failed
         */
        SEARCH_SERVICE_DELETE_SEARCH_ERROR,

        /**
         * The search service lacks at least one parameter
         */
        SEARCH_SERVICE_NO_PARAMETERS_ERROR,

        /**
         * The search service has both parent and ancestor parameters
         */
        SEARCH_SERVICE_PARENT_ANCESTOR_EXCLUSIVE_ERROR,

        /**
         * The search service cannot parse the request body
         */
        SEARCH_SERVICE_REQUEST_PARSING_ERROR,

        /**
         * An error indicating a teiid status error
         */
        TEIID_SERVICE_STATUS_ERROR,

        /**
         * The teiid service cannot parse the request body
         */
        TEIID_SERVICE_REQUEST_PARSING_ERROR,

        /**
         * The teiid service requires all credentials to contain a value
         */
        TEIID_SERVICE_EMPTY_CREDENTIAL_ERROR,

        /**
         * An error indicating a teiid credentials failure
         */
        TEIID_SERVICE_SET_CREDENTIALS_ERROR,

        /**
         * An error determining data service deployable status
         */
        TEIID_SERVICE_GET_DATA_SERVICE_DEPLOYABLE_ERROR,

        /**
         * An error when getting vdbs
         */
        TEIID_SERVICE_GET_VDBS_ERROR,

        /**
         * An error when getting datasources
         */
        TEIID_SERVICE_GET_DATA_SOURCES_ERROR,

        /**
         * An error when getting datasource
         */
        TEIID_SERVICE_GET_DATA_SOURCE_ERROR,

        /**
         * An error when getting a Datasource translator.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_TRANSLATOR_ERROR,

        /**
         * An error indicating a teiid vdb status error
         */
        TEIID_SERVICE_VDBS_STATUS_ERROR,

        /**
         * An error when getting drivers
         */
        TEIID_SERVICE_GET_DRIVERS_ERROR,

        /**
         * An error indicating a timeout occurred whilst conducting an import
         */
        TEIID_SERVICE_IMPORT_TIMEOUT,

        /**
         * An error indicating an error occurred whilst fetching the teiid translators
         */
        TEIID_SERVICE_GET_TRANSLATORS_ERROR,

        /**
         * An error occurred locating the default translator mappings
         */
        TEIID_SERVICE_DEFAULT_TRANSLATOR_MAPPINGS_NOT_FOUND_ERROR,

        /**
         * An error occurred attempting to load the default translator mappings
         */
        TEIID_SERVICE_LOAD_DEFAULT_TRANSLATOR_MAPPINGS_ERROR,

        /**
         * An error indicating a teiid file attributes object has no parameters
         */
        TEIID_SERVICE_FILE_ATTRIB_NO_PARAMETERS,

        /**
         * An error indicating a teiid file attributes object has no name
         */
        TEIID_SERVICE_FILE_ATTRIB_NO_NAME,

        /**
         * An error indicating a teiid file attributes object has no file
         */
        TEIID_SERVICE_FILE_ATTRIB_NO_CONTENT,

        /**
         * An error indicating a teiid dataService deployment failure
         */
        TEIID_SERVICE_DEPLOY_DATA_SERVICE_ERROR,

        /**
         * An error indicating a teiid connection deployment failure
         */
        TEIID_SERVICE_DEPLOY_CONNECTION_ERROR,

        /**
         * An error indicating a workspace driver being deployed to teiid cannot be found due to
         * a missing path property
         */
        TEIID_SERVICE_DRIVER_MISSING_PATH,

        /**
         * A driver cannot be found at the given path in the workspace
         */
        TEIID_SERVICE_NO_DRIVER_FOUND_IN_WKSP,

        /**
         * Cannot deploy a driver since one of its attributes is missing
         */
        TEIID_SERVICE_DRIVER_ATTRIBUTES_MISSING,

        /**
         * An error indicating a teiid DataSource undeploy failure
         */
        TEIID_SERVICE_UNDEPLOY_DATA_SOURCE_ERROR,

        /**
         * An error indicating a teiid Vdb deployment failure
         */
        TEIID_SERVICE_DEPLOY_VDB_ERROR,

        /**
         * An error indicating a teiid Vdb undeploy failure
         */
        TEIID_SERVICE_UNDEPLOY_VDB_ERROR,
        
        /**
         * An error indicating a teiid driver deployment failure
         */
        TEIID_SERVICE_DEPLOY_DRIVER_ERROR,

        /**
         * An error indicating a teiid driver undeployment failure
         */
        TEIID_SERVICE_UNDEPLOY_DRIVER_ERROR,

        /**
         * An error indicating a name is missing while deploying a data service
         */
        TEIID_SERVICE_DATA_SERVICE_MISSING_PATH,

        /**
         * No data service could be found while trying to deploy
         */
        TEIID_SERVICE_NO_DATA_SERVICE_FOUND,

        /**
         * An error indicating a name is missing while deploying a data source
         */
        TEIID_SERVICE_CONNECTION_MISSING_PATH,

        /**
         * No data source could be found while trying to deploy
         */
        TEIID_SERVICE_NO_CONNECTION_FOUND,

        /**
         * No VDB could be found while trying to deploy
         */
        TEIID_SERVICE_NO_VDB_FOUND,

        /**
         * An error indicating a name is missing while deploying a VDB
         */
        TEIID_SERVICE_VDB_MISSING_PATH,

        /**
         * No query specified for the query operation
         */
        TEIID_SERVICE_QUERY_MISSING_QUERY,

        /**
         * No target specified for the query operation
         */
        TEIID_SERVICE_QUERY_MISSING_TARGET,

        /**
         * The query target does not appear to have been deployed
         */
        TEIID_SERVICE_QUERY_TARGET_NOT_DEPLOYED,

        /**
         * An error indicating a query failure
         */
        TEIID_SERVICE_QUERY_ERROR,

        /**
         * Error indicating a ping type is missing
         */
        TEIID_SERVICE_PING_MISSING_TYPE,

        /**
         * The Teiid service update is missing a parameter
         */
        TEIID_SERVICE_UPDATE_MISSING_PARAMETER_ERROR,

        /**
         * The service lacks at least one parameter
         */
        TEIID_SERVICE_UPDATE_REQUEST_PARSING_ERROR,

        /**
         * An error indicating update attempt failed because the VDB name was missing
         */
        TEIID_SERVICE_UPDATE_MISSING_VDBNAME,

        /**
         * An error indicating update attempt failed because the Model name was missing
         */
        TEIID_SERVICE_UPDATE_MISSING_MODELNAME,

        /**
         * An error indicating update attempt failed because the Teiid VDB name was missing
         */
        TEIID_SERVICE_UPDATE_MISSING_TEIID_VDBNAME,

        /**
         * An error indicating update attempt failed because the Teiid Model name was missing
         */
        TEIID_SERVICE_UPDATE_MISSING_TEIID_MODELNAME,

        /**
         * An error indicating update attempt failed because retrieval of the teiid DDL failed.
         */
        TEIID_SERVICE_UPDATE_DDL_FETCH_ERROR,
        
        /**
         * An error indicating update attempt failed because the Teiid Model DDL was empty
         */
        TEIID_SERVICE_UPDATE_DDL_DNE,
        
        /**
         * An error indicating data source isn not a JDBC source.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_NOT_JDBC_ERROR,

        /**
         * An error indicating data source cannot be instantiated from available data sources
         */
        TEIID_SERVICE_GET_DATA_SOURCE_INSTANTIATION_FAILURE,
        
        /**
         * An error indicating attempt to get source JDBC connection failed.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_CONNECTION_ERROR,
        
        /**
         * An error indicating attempt to fetch source JDBC tables failed.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_TABLE_FETCH_ERROR,
        
        /**
         * An error indicating attempt to get source tables failed.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_TABLES_ERROR,

        /**
         * An error indicating attempt to get source catalog and schema failed.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_CATALOG_SCHEMA_ERROR,

        /**
         * An error indicating jdbc info failed to be supplied from a data source.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_JDBC_INFO_FAILURE,

        /**
         * An error indicating the jdbc data source is not recognised.
         */
        TEIID_SERVICE_GET_DATA_SOURCE_UNRECOGNISED_JDBC_SOURCE,

        /**
         * An error indicating a connection could not be undeployed
         */
        TEIID_SERVICE_UNDEPLOY_CONNECTION_ERROR,

        /**
         * An error indicating update attempt failed
         */
        TEIID_SERVICE_UPDATE_ERROR,

        /**
         * An error indicating the instance failed to get a data source template.
         */
        TEIID_SERVICE_GET_TEMPLATE_ERROR,

        /**
         * An error indicating the instance failed to get any data source templates.
         */
        TEIID_SERVICE_GET_TEMPLATES_ERROR,

        /**
         * An error indicating the instance failed to get any data source templates.
         */
        TEIID_SERVICE_GET_TEMPLATE_ENTRIES_ERROR,

        /**
         * The importexport service lacks at least one storage attribute
         */
        IMPORT_EXPORT_SERVICE_NO_PARAMETERS_ERROR,

        /**
         * the importexport service encountered an unsupported storage type
         */
        IMPORT_EXPORT_SERVICE_UNSUPPORTED_TYPE_ERROR,

        /**
         * The import export service cannot parse the request body
         */
        IMPORT_EXPORT_SERVICE_REQUEST_PARSING_ERROR,

        /**
         * The import export service cannot find the artifact to be exported
         */
        IMPORT_EXPORT_SERVICE_NO_ARTIFACT_ERROR,

        /**
         * The import export service is trying to export an artifact that is not exportable
         */
        IMPORT_EXPORT_SERVICE_ARTIFACT_NOT_EXPORTABLE_ERROR,

        /**
         * The import export service has not found a relative file path to import from
         */
        IMPORT_EXPORT_SERVICE_NO_FILE_PATH_ERROR,

        /**
         * An import export service export error
         */
        IMPORT_EXPORT_SERVICE_EXPORT_ERROR,

        /**
         * An import export service import error
         */
        IMPORT_EXPORT_SERVICE_IMPORT_ERROR,

        /**
         * An import export service import artifact error
         */
        IMPORT_EXPORT_SERVICE_IMPORT_ARTIFACT_ERROR,
        
        /**
         * An import export service storage types retrieval error
         */
        IMPORT_EXPORT_SERVICE_STORAGE_TYPES_ERROR,

        /**
         * An import export service storage types missing parameter error
         */
        IMPORT_EXPORT_SERVICE_MISSING_PARAMETER_ERROR;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }

    }

    private static final String BUNDLE_NAME = RelationalMessages.class.getPackage().getName() + DOT
                                              + RelationalMessages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" ); //$NON-NLS-1$
        return components[ components.length - 1 ];
    }

    private static String getString( final Enum< ? > key ) {
        try {
            return RESOURCE_BUNDLE.getString( key.toString() );
        } catch ( final Exception e ) {
            String msg;

            if ( e instanceof NullPointerException ) {
                msg = "<No message key>"; //$NON-NLS-1$
            } else if ( e instanceof MissingResourceException ) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = e.getLocalizedMessage();
            }

            return msg;
        }
    }

    private static void expandParameters(Object parameter, List<Object> paramList) {
        if (parameter instanceof Object[]) {
            Object[] parameters = (Object[]) parameter;
            for (Object param : parameters) {
                expandParameters(param, paramList);
            }
            return;
        }

        paramList.add(parameter);
    }

    /**
     * @param key
     *        the message key (cannot be <code>null</code>)
     * @param parameters
     *        the substitution parameters (can be <code>null</code>)
     * @return the localized message (never empty)
     */
    public static String getString( final Enum< ? > key,
                                    final Object... parameters ) {
        final String text = getString( key );

        // return key if message not found
        if ( text == null ) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }

        // return if no parameters to format
        if ( ( parameters == null ) || ( parameters.length == 0 ) ) {
            return text;
        }

        List<Object> expandedParam = new ArrayList<>();
        expandParameters(parameters, expandedParam);

        // return formatted message
        return String.format( text, expandedParam.toArray() );
    }

    /**
     * Don't allow construction outside of this class.
     */
    private RelationalMessages() {
        // nothing to do
    }

}
