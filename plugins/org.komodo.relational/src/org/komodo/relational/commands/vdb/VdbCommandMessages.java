/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import java.util.ResourceBundle;
import org.komodo.relational.vdb.DataRole;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link DataRole}-related shell commands.
 */
public class VdbCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( VdbCommandMessages.class.getPackage().getName() + DOT + VdbCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    @SuppressWarnings( "javadoc" )
    public enum General {
        MISSING_DATA_ROLE_NAME,
        MISSING_ENTRY_NAME,
        MISSING_ENTRY_PATH,
        MISSING_IMPORT_NAME,
        MISSING_MODEL_NAME,
        MISSING_TRANSLATOR_NAME,
        MISSING_TRANSLATOR_TYPE,
        MISSING_VDB_VERSION,
        NAME_TYPE_DISPLAY;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ExportCommand {
        VDB_EXPORTED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum ShowDataRolesCommand {
        NO_DATA_ROLES,
        DATA_ROLES_HEADER,
        SHOW_DATA_ROLES_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowEntriesCommand {
        NO_ENTRIES,
        ENTRIES_HEADER,
        SHOW_ENTRIES_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowImportsCommand {
        NO_IMPORTS,
        IMPORTS_HEADER,
        SHOW_IMPORTS_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowModelsCommand {
        NO_MODELS,
        MODELS_HEADER,
        SHOW_MODELS_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowTranslatorsCommand {
        NO_TRANSLATORS,
        SHOW_TRANSLATORS_ERROR,
        TRANSLATORS_HEADER;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowVdbCommand {

        SHOW_VDB_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }

    }

    @SuppressWarnings( "javadoc" )
    public enum AddDataRoleCommand {
        ADD_DATA_ROLE_ERROR,
        DATA_ROLE_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddEntryCommand {
        ADD_ENTRY_ERROR,
        ENTRY_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddImportCommand {
        ADD_IMPORT_ERROR,
        IMPORT_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddModelCommand {
        ADD_MODEL_ERROR,
        MODEL_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddTranslatorCommand {
        ADD_TRANSLATOR_ERROR,
        TRANSLATOR_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteDataRoleCommand {
        DATA_ROLE_DELETED,
        DELETE_DATA_ROLE_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteEntryCommand {
        DELETE_ENTRY_ERROR,
        ENTRY_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteImportCommand {
        DELETE_IMPORT_ERROR,
        IMPORT_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteModelCommand {
        DELETE_MODEL_ERROR,
        MODEL_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteTranslatorCommand {
        DELETE_TRANSLATOR_ERROR,
        TRANSLATOR_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum SetVdbPropertyCommand {
        PLACEHOLDER;

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