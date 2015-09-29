/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.importer.commands;

import static org.komodo.spi.constants.StringConstants.CLOSE_ANGLE_BRACKET;
import static org.komodo.spi.constants.StringConstants.DOT;
import static org.komodo.spi.constants.StringConstants.OPEN_ANGLE_BRACKET;
import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Localized messages for importer shell commands.
 */
@SuppressWarnings( "javadoc" )
public final class ImportCommandMessages {

    public enum ImportCommand {

        ImportError,
        InvalidArgMsg_SubCommand,
        InvalidArgMsg_FileName,
        InvalidArgMsg_ModelName,
        InvalidTargetPath,
        DdlImportInProgressMsg,
        VdbImportInProgressMsg,
        DdlImportSuccessMsg,
        VdbImportSuccessMsg,
        InvalidSubCommand,
        ImportFailedMsg,
        childTypeNotAllowed,
        InvalidDDLParentType,
        ErrorCreatingTempNode,
        DeleteTempContextFailedMsg,
        cannotImport_wouldCreateDuplicate;

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

    private static final String BUNDLE_NAME = ( ImportCommandMessages.class.getPackage().getName() + DOT
                                                + ImportCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

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
                msg = "<No message available>"; //$NON-NLS-1$
            } else if ( e instanceof MissingResourceException ) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = e.getLocalizedMessage();
            }

            return msg;
        }
    }

    public static String getString( final Enum< ? > key,
                                    final Object... parameters ) {
        final String text = getString( key );

        if ( text == null ) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }

        if ( ( parameters == null ) || ( parameters.length == 0 ) ) {
            return text;
        }

        return MessageFormat.format( text, parameters );
    }

    public static String getString( final ResourceBundle bundle,
                                    final String key,
                                    final Object... parameters ) {
        String text = null;

        try {
            text = bundle.getString( key );
        } catch ( final Exception err ) {
            if ( err instanceof NullPointerException ) {
                text = "<No message available>"; //$NON-NLS-1$
            } else if ( err instanceof MissingResourceException ) {
                text = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                text = err.getLocalizedMessage();
            }
        }

        // Check the trivial cases ...
        if ( text == null ) {
            return OPEN_ANGLE_BRACKET + key + CLOSE_ANGLE_BRACKET;
        }
        if ( ( parameters == null ) || ( parameters.length == 0 ) ) {
            return text;
        }

        return MessageFormat.format( text, parameters );
    }

}
