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
package org.komodo.shell;

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
    public enum SHELL {
        COMPONENT_STARTED,
        COMPONENT_FAILED,
        ENGINE_STARTING,
        LOCAL_REPOSITORY_STARTING,
        LOCAL_REPOSITORY_TIMEOUT_ERROR,
    	COMMAND_NOT_FOUND,
    	GOOD_BYE,
    	Help_COMMAND_LIST_MSG,
    	Help_INVALID_COMMAND,
    	Help_USAGE,
    	Help_GET_HELP_1,
    	Help_GET_HELP_2,
    	EXITING,
    	INVALID_ARG,
    	USAGE,
    	SHUTTING_DOWN,
    	DONE,
    	InvalidArgMsg_EntryPath,
    	ENTRY_PATH,
    	ENTRY_LIST_SUMMARY,
    	ENTRY,
    	InvalidArgMsg_ArtifactId,
    	Property_InvalidArgMsg_SubCommand,
    	Property_InvalidArgMsg_PropertyName,
    	Property_InvalidArgMsg_PropertyValue,
    	Property_PropertySet,
    	Property_PropertyUnset,
    	Property_InvalidSubCommand,
    	Property_Failure,
    	InvalidArgMsg_propertiesFile_not_exist,
    	InvalidArgMsg_property_not_correct_format,
    	InvalidArgMsg_propertiesFile_error_reading,
    	InvalidArgMsg_propertiesFile_error_reading_line,
    	FileShellCommandReader_NoConsole;

    	@Override
    	public String toString() {
    		return getEnumName(this) + DOT + name();
    	}
    }

    @SuppressWarnings( "javadoc" )
    public enum StatusCommand {
        Separator,
        Connected,
        NotConnected,
        PingOk,
        PingFail;

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
     * @param key the enum key
     * @param parameters parameters
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
    
    /**
     * Look up a message in the i18n resource message bundle by key, then format the
     * message with the given params and return the result.
     * @param key the message key
     * @param parameters the parameters
     * @return the message
     */
    public static String getString(String key, Object ... parameters) {
    	String text = null;
        try {
        	text = RESOURCE_BUNDLE.getString(key);
        } catch (final Exception err) {
            if (err instanceof NullPointerException) {
            	text = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
            	text = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
            	text = err.getLocalizedMessage();
            }
        }
        
        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}
