/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import java.util.ResourceBundle;
import org.komodo.relational.model.Table;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link Table}-related shell commands.
 */
public class TableCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( TableCommandMessages.class.getPackage().getName() + DOT + TableCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    @SuppressWarnings( "javadoc" )
    public enum General {
        MISSING_ACCESS_PATTERN_NAME,
        MISSING_COLUMN_NAME,
        MISSING_FOREIGN_KEY_NAME,
        MISSING_PRIMARY_KEY_NAME,
        MISSING_INDEX_NAME,
        MISSING_UNIQUE_CONSTRAINT_NAME,
        INVALID_ON_COMMIT_PROPERTY_VALUE,
        INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE,
        INVALID_TEMPORARY_TABLE_TYPE_PROPERTY_VALUE;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddAccessPatternCommand {
        ACCESS_PATTERN_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddColumnCommand {
        COLUMN_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddForeignKeyCommand {
        FOREIGN_KEY_ADDED,
        INVALID_TABLE_PATH,
        MISSING_FOREIGN_KEY_TABLE_REF_PATH;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddIndexCommand {
        INDEX_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddUniqueConstraintCommand {
        UNIQUE_CONSTRAINT_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum AddPrimaryKeyCommand {
        PK_EXISTS_CANT_ADD,
        PRIMARY_KEY_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteAccessPatternCommand {
        ACCESS_PATTERN_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteColumnCommand {
        COLUMN_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteForeignKeyCommand {
        FOREIGN_KEY_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteIndexCommand {
        INDEX_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteUniqueConstraintCommand {
        UNIQUE_CONSTRAINT_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeletePrimaryKeyCommand {
        NO_PK_TO_REMOVE,
        PRIMARY_KEY_DELETED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowAccessPatternsCommand {

        ACCESS_PATTERNS_HEADER,
        NO_ACCESS_PATTERNS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowColumnsCommand {

        COLUMNS_HEADER,
        NO_COLUMNS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowIndexesCommand {

        INDEXES_HEADER,
        NO_INDEXES;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ShowUniqueConstraintsCommand {

        NO_UNIQUE_CONSTRAINTS,
        UNIQUE_CONSTRAINTS_HEADER;

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
