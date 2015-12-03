package org.komodo.shell.api;

import static org.komodo.spi.constants.StringConstants.DOT;

/**
 *Tab completion modifier through which an user is able to customize tab completion
 *
 */
public enum TabCompletionModifier {

	/**
	 * Let engine to select a proper value
	 */
	AUTO, 
	/**
	 * Do not add a space after autocompleted text
	 */
	NO_APPEND_SEPARATOR, 
	/**
	 * Add a space after autocompleted text
	 */
	APPEND_SEPARATOR, 
	/**
	 * Do not perform autocompletion
	 */
	NO_AUTOCOMPLETION;

    @Override
    public String toString() {
        return getEnumName( this ) + DOT + name();
    }

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" ); //$NON-NLS-1$
        return components[ components.length - 1 ];
    }
}
