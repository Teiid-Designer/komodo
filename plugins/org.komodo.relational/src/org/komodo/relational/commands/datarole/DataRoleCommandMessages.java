/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.spi.constants.StringConstants.DOT;
import java.util.ResourceBundle;
import org.komodo.relational.Messages;
import org.komodo.relational.vdb.DataRole;

/**
 * Localized messages for {@link DataRole}-related shell commands.
 */
enum DataRoleCommandMessages {

    MISSING_MAPPED_ROLE_NAME,

    MISSING_PERMISSION_NAME;

    private static final String BUNDLE_NAME = ( DataRoleCommandMessages.class.getPackage().getName() + DOT + DataRoleCommandMessages.class.getSimpleName().toLowerCase() );

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
