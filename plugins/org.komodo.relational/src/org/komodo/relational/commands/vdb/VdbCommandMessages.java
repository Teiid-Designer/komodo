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
        MISSING_VDB_VERSION;

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
        NO_DATA_ROLES;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowEntriesCommand {
        NO_ENTRIES;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum ShowImportsCommand {
        NO_IMPORTS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum ShowModelsCommand {
        NO_MODELS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowTranslatorsCommand {
        NO_TRANSLATORS;

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