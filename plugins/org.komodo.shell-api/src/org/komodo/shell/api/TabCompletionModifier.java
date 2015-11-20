package org.komodo.shell.api;

import static org.komodo.spi.constants.StringConstants.DOT;

public enum TabCompletionModifier {

	AUTO, // Let engine to select proper value
	NO_APPEND_SEPARATOR, // Do not add a space after autocompleted text
	APPEND_SEPARATOR, // Add a space after autocompleted text
	NO_AUTOCOMPLETION;// Do not perform autocompletion

    @Override
    public String toString() {
        return getEnumName( this ) + DOT + name();
    }

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" );
        return components[ components.length - 1 ];
    }
}
