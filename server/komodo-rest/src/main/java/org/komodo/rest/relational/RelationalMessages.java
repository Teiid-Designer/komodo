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
         * VDB undeployment request sent but not yet undeployed
         */
        VDB_UNDEPLOYMENT_REQUEST_SENT,

        /**
         * Vdb successfully deployed
         */
        VDB_SUCCESSFULLY_UNDEPLOYED,

        /**
         * DataSource undeployment request sent but not yet undeployed
         */
        DATA_SOURCE_UNDEPLOYMENT_REQUEST_SENT,

        /**
         * DataSource successfully deployed
         */
        DATA_SOURCE_SUCCESSFULLY_UNDEPLOYED,
        
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
         * Data source status title
         */
        DATA_SOURCE_DEPLOYMENT_STATUS_TITLE,

        /**
         * Data source successfully deployed
         */
        DATA_SOURCE_SUCCESSFULLY_DEPLOYED,

        /**
         * Data source deployed with errors
         */
        DATA_SOURCE_DEPLOYED_WITH_ERRORS,

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
        VDB_TO_REPO_SUCCESS;

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
         * An error indicating update attempt was missing a name
         */
        DATASERVICE_SERVICE_UPDATE_MISSING_NAME,
        
        /**
         * An error indicating that the service does not exist
         */
        DATASERVICE_SERVICE_UPDATE_SERVICE_DNE,
        
        /**
         * An error indicating update attempt was missing json arg
         */
        DATASERVICE_SERVICE_UPDATE_MISSING_JSON,
 
        /**
         * An error indicating the Dataservice name is missing from the input JSON document.
         */
        DATASERVICE_SERVICE_JSON_MISSING_NAME,

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
         * An error indicating a JSON document representing the Datasources in the workspace could not be retrieved.
         */
        DATASOURCE_SERVICE_GET_DATASOURCES_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified Datasource.
         */
        DATASOURCE_SERVICE_GET_DATASOURCE_ERROR,

        /**
         * An error indicating a request to create a datasource failed
         */
        DATASOURCE_SERVICE_CREATE_DATASOURCE_ERROR,

        /**
         * An error indicating a request to clone a datasource failed
         */
        DATASOURCE_SERVICE_CLONE_DATASOURCE_ERROR,

        /**
         * An error indicating a request to delete a datasource failed
         */
        DATASOURCE_SERVICE_DELETE_DATASOURCE_ERROR,
        
        /**
         * An error indicating a request to update a datasource failed
         */
        DATASOURCE_SERVICE_UPDATE_DATASOURCE_ERROR,

        /**
         * An error indicating create attempt was missing a name
         */
        DATASOURCE_SERVICE_CREATE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a name
         */
        DATASOURCE_SERVICE_CLONE_MISSING_NAME,
 
        /**
         * An error indicating clone attempt was missing a new datasource name
         */
        DATASOURCE_SERVICE_CLONE_MISSING_NEW_NAME,
 
        /**
         * An error indicating the desired new clone name is same as datasource being cloned
         */
        DATASOURCE_SERVICE_CLONE_SAME_NAME_ERROR,
        
        /**
         * An error indicating update attempt was missing a name
         */
        DATASOURCE_SERVICE_UPDATE_MISSING_NAME,
        
        /**
         * An error indicating that the datasource does not exist
         */
        DATASOURCE_SERVICE_UPDATE_SOURCE_DNE,
        
        /**
         * An error indicating update attempt was missing json arg
         */
        DATASOURCE_SERVICE_UPDATE_MISSING_JSON,
 
        /**
         * An error indicating the datasource name is missing from the input JSON document.
         */
        DATASOURCE_SERVICE_JSON_MISSING_NAME,

        /**
         * An error indicating the parameter and JSON datasource name does not match for a datasource being created.
         */
        DATASOURCE_SERVICE_SOURCE_NAME_ERROR,

        /**
         * An error indicating create attempt failed because same name already exists
         */
        DATASOURCE_SERVICE_CREATE_ALREADY_EXISTS,
        
        /**
         * An error indicating clone attempt failed because same name already exists
         */
        DATASOURCE_SERVICE_CLONE_ALREADY_EXISTS,
        
        /**
         * An error indicating a JSON document representing the Drivers in the workspace could not be retrieved.
         */
        DRIVER_SERVICE_GET_DRIVERS_ERROR,

        /**
         * An error indicating the VDB descriptor JSON representation could not be created.
         */
        VDB_DESCRIPTOR_BUILDER_ERROR,

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
         * An error indicating a teiid dataSource deployment failure
         */
        TEIID_SERVICE_DEPLOY_DATA_SOURCE_ERROR,

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
        TEIID_SERVICE_DATA_SOURCE_MISSING_PATH,

        /**
         * No data source could be found while trying to deploy
         */
        TEIID_SERVICE_NO_DATA_SOURCE_FOUND,

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
