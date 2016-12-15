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
package org.komodo.teiid;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class Messages implements StringConstants {

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName()
    																					+ DOT
    																					+ Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    @SuppressWarnings( "javadoc" )
    public enum Error {

        UnsupportedTeiid,
        UnsupportedStorageType;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum TeiidInstance {
        versionFailure,
        parentNotStartedMessage,
        reconnectErrorMsg,
        noSuchField,
        buildOperationFailure,
        requestDriverFailure,
        requestTeiidVersionFailure;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ExecutionAdmin {
        mergeVdbUnsupported,
        dynamicVdbInvalidName,
        jarDeploymentJarNotFound,
        jarDeploymentFailed,
        jarDeploymentJarNotReadable,
        jdbcSourceForClassNameNotFound,
        dataSourceTypeDoesNotExist,
        errorCreatingDataSource,
        invalidPropertyValue,
        cannotConnectToServer,
        instanceDeployUndeployProblemPingingTeiidJdbc,
        invalidPropertyEditorConstrainedValue,
        invalidPropertyEditorValue,
        invalidNullPropertyValue,
        missingPropertyDefinition,
        unknownPropertyType,Property,
        connectorDetailedName,
        failedToGetDriverMappings,
        cannotLoadDriverClass,
        admin_conn_closed,
        invalid_parameter,
        properties_describe,
        import_vdbs_describe,
        models_describe,
        override_translators_describe,
        data_policies_describe,
        source_mappings_describe,
        validity_errors_describe,
        data_permissions_describe,
        mapped_role_names_describe,
        noParentServer,
        refreshVdbException;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum TeiidVdb {
        onlySupportingDynamicVdbs;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum VDBMetadata {
        description_describe,
        vdb_name_describe,
        model_name_describe,
        model_path_describe,
        translator_name_describe,
        connection_type_describe,
        status_describe,
        vdb_version_describe,
        url_describe,
        xml_deployment_describe,
        property_name_describe,
        property_value_describe,
        visible_describe,
        model_type_describe,
        source_name_describe,
        jndi_name_describe,
        translator_description_describe,
        error_path_describe,
        severity_describe,
        message_describe,
        base_type_describe,
        module_name_describe,
        allow_create_temp_tables_describe,
        any_authenticated_describe,
        policy_name_describe,
        properties_describe,
        source_mappings_describe,
        validity_errors_describe,
        models_describe,
        import_vdbs_describe,
        import_vdb_name_describe,
        import_vdb_version_describe,
        import_policies_describe,
        override_translators_describe,
        data_policies_describe,
        data_permissions_describe,
        mapped_role_names_describe,
        policy_description_describe,
        vdb_description_describe,
        resource_name_describe,
        metadata_status_describe,
        execution_id_describe,

        session_id_describe,
        start_time_describe,
        command_describe,
        source_request_describe,
        node_id_describe,
        transaction_id_describe,
        processing_state_describe,
        thread_state_describe,

        application_name_describe,
        created_time_describe,
        client_host_address_describe,
        ip_address_describe,
        last_ping_time_describe,
        user_name_describe,
        security_domain_describe,

        txn_created_time_describe,
        txn_scope_describe,
        txn_id_describe,

        max_threads_describe,
        highest_queued_describe,
        queued_describe,
        queue_name_describe,
        total_submitted_describe,
        total_completed_describe,
        highest_active_threads_describe,
        active_threads_describe,

        allow_create_describe,
        allow_read_describe,
        allow_update_describe,
        allow_delete_describe,
        allow_execute_describe,
        allow_alter_describe,
        allow_language_describe;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum VDBMetadataParser {

        unexpected_element1,
        unexpected_element2,
        unexpected_element3,
        unexpected_element4,
        unexpected_element5,
        unexpected_element6,
        unexpected_element7;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum TeiidURL {
        invalid_format,
        invalid_ipv6_hostport,
        invalid_hostport,
        non_numeric_port,
        port_out_of_range;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum Socket {
        keystore_not_found,
        alias_no_key_entry,
        anon_not_available;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum GSS {
        client_prop_missing,
        ambigious_gss_selection,
        no_gss_selection,
        system_prop_missing,
        gss_auth_failed,
        no_krb_ticket;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum JDBC {
        Connection_success,
        continuous,
        DataTypeTransformer_blob_too_big,
        DataTypeTransformer_Err_converting,
        DeferredMetadataProvider_Invalid_data,
        Err_registering,
        forward_only_resultset,
        Method_not_supported,
        MMCallableStatement_Param_not_found,
        MMConnection_Cant_use_closed_connection,
        MMConnection_Commit_success,
        MMConnection_Concurrency_type_not_supported,
        MMConnection_Connection_close_success,
        MMConnection_Err_closing_stmts,
        MMConnection_Err_connection_close,
        MMConnection_Rollback_success,
        MMConnection_Scrollable_type_not_supported,
        MMConnection_Session_success,
        MMConnection_SQL_cannot_be_null,
        MMDatabaseMetadata_Best_row_sucess,
        MMDatabaseMetadata_Catalog_success,
        MMDatabaseMetadata_Err_getting_primary_keys,
        MMDatabaseMetadata_getCols_error,
        MMDatabaseMetadata_getCols_success,
        MMDatabaseMetadata_getCrossRef_error,
        MMDatabaseMetadata_getCrossRef_success,
        MMDatabaseMetadata_getExpKey_error,
        MMDatabaseMetadata_getExpKey_success,
        MMDatabaseMetadata_getImpKey_error,
        MMDatabaseMetadata_getImpKey_success,
        MMDatabaseMetadata_getIndex_error,
        MMDatabaseMetadata_getIndex_success,
        MMDatabaseMetadata_getPrimaryKey_error,
        MMDatabaseMetadata_getPrimaryKey_success,
        MMDatabaseMetadata_getProcCol_error,
        MMDatabaseMetadata_getProcCol_success,
        MMDatabaseMetadata_getProc_error,
        MMDatabaseMetadata_getProc_success,
        MMDatabaseMetadata_getRefKey_success,
        MMDatabaseMetadata_getschema_error,
        MMDatabaseMetadata_getschema_success,
        MMDatabaseMetadata_getTable_error,
        MMDatabaseMetadata_getTable_success,
        MMDatabaseMetadata_getTableType_success,
        MMDatabaseMetadata_getTypes_success,
        MMDatabaseMetadata_getVersionCols_success,
        MMPreparedStatement_Err_prep_sql,
        MMPreparedStatement_Invalid_param_index,
        MMResultSet_cannot_convert_to_binary_stream,
        MMResultSet_Cant_call_closed_resultset,
        MMResultsImpl_Col_doesnt_exist,
        MMStatement_Bad_timeout_value,
        MMStatement_Close_stmt_success,
        MMStatement_Error_timing_out,
        MMStatement_Invalid_During_Transaction,
        MMStatement_Invalid_fetch_size,
        MMStatement_Invalid_field_size,
        MMStatement_Stmt_closed,
        MMStatement_Success_query,
        MMStatement_Timeout_before_complete,
        MMStatement_Timeout_ocurred_in_Statement,
        MMXAConnection_rolling_back,
        MMXAConnection_rolling_back_error,
        PlanNode_unexpected_element,
        ResultsImpl_Invalid_col_index,
        ResultsImpl_Op_invalid_fwd_only,
        ResultsImpl_The_cursor_is_not_on_a_valid_row_1,
        StatementImpl_set_result_set,
        StatementImpl_show_update_count,
        StaticMetadataProvider_Invalid_column,
        StoredProcedureResultsImpl_Invalid_parameter_index__0_2,
        StoredProcedureResultsImpl_ResultSet_cursor_is_after_the_last_row_1,
        stream_closed,
        Unable_to_read_data_from_stream,
        urlFormat,
        WarningUtil_Failures_occurred,
        wrong_class;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ProcedureService {
        procedureServiceTextTableSqlTemplate,
        procedureServiceTextInvokeHttpTableSqlTemplate,
        procedureServiceXmlGetTextFilesTableSqlTemplate,
        procedureServiceXmlInvokeHttpTableSqlTemplate;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }

    public enum TeiidService {
        NotAJcrNode;

        @Override
        public String toString() {
            return getEnumName(this) + DOT +  name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

    private Messages() {
    }

    /**
     * Get message string
     *
     * @param key
     *
     * @return i18n string
     */
    private static String getString(Enum<?> key) {
        try {
            return RESOURCE_BUNDLE.getString(key.toString());
        } catch (final Exception err) {
            String msg;

            if (err instanceof NullPointerException) {
                msg = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = err.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * Get message string with parameters
     *
     * @param key the message key (cannot be empty)
     * @param parameters the message parameters (can be empty)
     *
     * @return i18n string
     */
    public static String getString(Enum<?> key, Object... parameters) {
        String text = getString(key);

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}
