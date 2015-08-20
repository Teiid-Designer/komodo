/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.spi.constants.StringConstants.DOT;
import java.util.ResourceBundle;
import org.komodo.relational.Messages;
import org.komodo.relational.vdb.DataRole;

/**
 * Localized messages for {@link DataRole}-related shell commands.
 */
enum VdbCommandMessages {

    /**
     * An error message indicating a VDB data role is missing a name.
     */
    MISSING_DATA_ROLE_NAME,

    /**
     * An error message indicating a VDB entry is missing a name.
     */
    MISSING_ENTRY_NAME,

    /**
     * An error message indicating a VDB entry is missing a path.
     */
    MISSING_ENTRY_PATH,

    /**
     * An error message indicating an import VDB is missing a name.
     */
    MISSING_IMPORT_NAME,

    /**
     * An error message indicating a VDB model is missing a name.
     */
    MISSING_MODEL_NAME,

    /**
     * An error message indicating a VDB translator is missing a name.
     */
    MISSING_TRANSLATOR_NAME,

    /**
     * An error message indicating a VDB translator is missing a type.
     */
    MISSING_TRANSLATOR_TYPE,

    /**
     * An error message indicating the VDB version is missing.
     */
    MISSING_VDB_VERSION,

    /**
     * A message indicating the VDB has no data roles defined.
     */
    NO_DATA_ROLES,

    /**
     * A message indicating the VDB has no entries defined.
     */
    NO_ENTRIES,

    /**
     * A message indicating the VDB has no VDB imports defined.
     */
    NO_IMPORTS,

    /**
     * A message indicating the VDB has no models defined.
     */
    NO_MODELS,

    /**
     * A message indicating the VDB has no translators defined.
     */
    NO_TRANSLATORS,

    /**
     * A message indicating the VDB was successfully exported.
     */
    VDB_EXPORTED;

    private static final String BUNDLE_NAME = ( VdbCommandMessages.class.getPackage().getName() + DOT + VdbCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    String getMessage( final Object... params ) {
        return Messages.getString( RESOURCE_BUNDLE, toString(), params );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
        return Messages.getEnumName( this ) + DOT + name();
    }

}
