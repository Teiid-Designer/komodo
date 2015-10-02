/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import java.util.ResourceBundle;
import org.komodo.relational.model.Model;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link Model}-related shell commands.
 */
public class ModelCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( ModelCommandMessages.class.getPackage().getName() + DOT + ModelCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    @SuppressWarnings( "javadoc" )
    public enum General {
        INVALID_MODEL_TYPE_PROPERTY_VALUE,
        MISSING_PUSHDOWN_FUNCTION_NAME,
        MISSING_SOURCE_NAME,
        MISSING_STORED_PROCEDURE_NAME,
        MISSING_TABLE_NAME,
        MISSING_USER_DEFINED_FUNCTION_NAME,
        MISSING_VIEW_NAME,
        MISSING_VIRTUAL_PROCEDURE_NAME,
        PUSHDOWN_FUNCTION_NAME,
        SOURCE_NAME,
        STORED_PROCEDURE_NAME,
        TABLE_NAME,
        USER_DEFINED_FUNCTION_NAME,
        VIEW_NAME,
        VIRTUAL_PROCEDURE_NAME;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddPushdownFunctionCommand {
        ADD_PUSHDOWN_FUNCTION_ERROR,
        PUSHDOWN_FUNCTION_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddSourceCommand {
        ADD_SOURCE_ERROR,
        SOURCE_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddStoredProcedureCommand {
        ADD_STORED_PROCEDURE_ERROR,
        STORED_PROCEDURE_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddTableCommand {
        ADD_TABLE_ERROR,
        TABLE_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddUserDefinedFunctionCommand {
        ADD_USER_DEFINED_FUNCTION_ERROR,
        USER_DEFINED_FUNCTION_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddViewCommand {
        ADD_VIEW_ERROR,
        VIEW_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddVirtualProcedureCommand {
        ADD_VIRTUAL_PROCEDURE_ERROR,
        VIRTUAL_PROCEDURE_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeletePushdownFunctionCommand {
        DELETE_PUSHDOWN_FUNCTION_ERROR,
        PUSHDOWN_FUNCTION_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteSourceCommand {
        DELETE_SOURCE_ERROR,
        SOURCE_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteStoredProcedureCommand {
        DELETE_STORED_PROCEDURE_ERROR,
        STORED_PROCEDURE_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteTableCommand {
        DELETE_TABLE_ERROR,
        TABLE_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteUserDefinedFunctionCommand {
        DELETE_USER_DEFINED_FUNCTION_ERROR,
        USER_DEFINED_FUNCTION_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteViewCommand {
        DELETE_VIEW_ERROR,
        VIEW_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteVirtualProcedureCommand {
        DELETE_VIRTUAL_PROCEDURE_ERROR,
        VIRTUAL_PROCEDURE_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ExportCommand {
        DDL_EXPORTED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}
