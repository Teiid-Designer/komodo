/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for the WorkspaceCommandMessages
 */
@SuppressWarnings( "javadoc" )
public class WorkspaceCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( WorkspaceCommandMessages.class.getPackage().getName() + DOT + WorkspaceCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    public enum General {
        ERROR_DDL_EMPTY,
        ERROR_WRITING_FILE,
        INVALID_BOOLEAN_PROPERTY_VALUE,
        INVALID_INTEGER_PROPERTY_VALUE,
        INVALID_OBJECT_TYPE,
        INVALID_PROPERTY_NAME,
        MISSING_OUTPUT_FILE_NAME,
        MISSING_INPUT_FILE_NAME,
        MISSING_PROPERTY_NAME_VALUE,
        MISSING_VDB_NAME,
        MISSING_TEIID_NAME,
        MISSING_SCHEMA_NAME,
        NO_PROPERTIES,
        OUTPUT_FILE_ERROR,
        INPUT_FILE_ERROR,
        PROPERTIES_HEADER,
        PRINT_RELATIONAL_OBJECT,
        PROPERTY_NOT_SET,
        SET_PROPERTY_SUCCESS,
        UNSET_MISSING_PROPERTY_NAME,
        UNSET_PROPERTY_SUCCESS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateVdbCommand {
        MISSING_VDB_EXTERNAL_PATH,
        VDB_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum ImportVdbCommand {
        VdbImportInProgressMsg,
        VdbImportSuccessMsg,
        InvalidSubCommand,
        ImportFailedMsg,
        childTypeNotAllowed,
        ErrorCreatingTempNode,
        DeleteTempVdbFailedMsg,
        cannotImport_wouldCreateDuplicate,
        VDB_IMPORTED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum DeleteVdbCommand {
        DELETE_VDB_ERROR,
        VDB_NOT_FOUND,
        VDB_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateTeiidCommand {
        CREATE_TEIID_ERROR,
        TEIID_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum DeleteTeiidCommand {
        DELETE_TEIID_ERROR,
        TEIID_NOT_FOUND,
        TEIID_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateSchemaCommand {
        CREATE_SCHEMA_ERROR,
        SCHEMA_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum DeleteSchemaCommand {
        DELETE_SCHEMA_ERROR,
        SCHEMA_NOT_FOUND,
        SCHEMA_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum FindCommand {
        MISSING_TYPE_NAME,
        HELP_TYPES_HEADING,
        FAILURE,
        INVALID_TYPE,
        NO_OBJECTS_FOUND,
        TYPE_HEADER;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum UploadVdbCommand {

        INVALID_OVERWRITE_ARG,
        MISSING_INPUT_VDB_FILE_PATH,
        MISSING_VDB_NAME,
        VDB_INPUT_FILE_IS_EMPTY,
        VDB_OVERWRITE_DISABLED,
        VDB_UPLOADED;

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

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}
