/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.util;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.ShellCommand;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 *  Utilities for formatted printing
 */
public class PrintUtils implements StringConstants {

    private static final int DEFAULT_WIDTH = 25;
    private static final int MAX_PROPERTY_VALUE_WIDTH = 100;  // Limit on the value column width
    
    /**
     * @param command the ShellCommand
     * @param items the item list
     * @param headerTitle name for header
     */
    public static void printList( ShellCommand command, List<String> items, String headerTitle ) {

        int maxNameWidth = DEFAULT_WIDTH;
        for(String item : items) {
            updateMaxNameWidth(maxNameWidth,item);
        }

        // Sort alphabetically
        Collections.sort(items);

        // Print Header
        final String format = getFormat( maxNameWidth );
        command.print( MESSAGE_INDENT, String.format( format, headerTitle ) );
        command.print( MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ) ) );

        // Print each name
        for ( final String name : items ) {
            command.print( MESSAGE_INDENT, name );
        }

    }
    
    /**
     * @param command the ShellCommand
     * @param props the properties
     * @param nameTitle the name title
     * @param valueTitle the value title
     */
    public static void printProperties(ShellCommand command, Properties props, String nameTitle, String valueTitle) {

        final Map< String, String > sorted = new TreeMap<>();
        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;
        
        for(String name : props.stringPropertyNames()) {
            String value = props.getProperty(name);
            if(StringUtils.isEmpty(value)) {
                value = Messages.getString( SHELL.NO_PROPERTY_VALUE );
            }
            
            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            if ( maxValueWidth < value.length() ) {
                maxValueWidth = value.length();
            }

            sorted.put( name, value );
        }
        
        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
            maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }
        
        final String format = getFormat( maxNameWidth, maxValueWidth );
        
        command.print( MESSAGE_INDENT, String.format( format, nameTitle, valueTitle ) );
        command.print( MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ), getHeaderDelimiter( maxValueWidth ) ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
            String propName = entry.getKey();
            String propValue = entry.getValue();
            // propValue less than maximum width
            if(propValue.length() <= maxValueWidth) {
                command.print( MESSAGE_INDENT, String.format( format, propName, propValue ) );
                // propValue exceeds maximum width - splits it up onto separate lines
            } else {
                printPropWithLongValue(command,format,propName,propValue,maxValueWidth);
            }
        }
    }
    
    private static void printPropWithLongValue(ShellCommand command, String format, String propName, String propValue, int maxValueWidth) {
        // splits long strings into equal length lines of 'maxValueWidth' length.
        List<String> lines = splitEqually(propValue,maxValueWidth);
        boolean first = true;
        for(String line : lines) {
            // First line includes the propName
            if(first) {
                command.print( MESSAGE_INDENT, String.format( format, propName, line ) );
                first = false;
            // Subsequent lines the 'name' is just a spacer
            } else {
                command.print( MESSAGE_INDENT, String.format( format, EMPTY_STRING, line ) );
            }
        }
    }

    private static List<String> splitEqually(String text, int size) {
        // Remove Control chars from the incoming string
        String noCtrlText = text.replaceAll("\\p{Cntrl}", EMPTY_STRING); //$NON-NLS-1$

        // Give the list the right capacity to start with. You could use an array
        // instead if you wanted.
        List<String> result = new ArrayList<String>((noCtrlText.length() + size - 1) / size);

        for (int start = 0; start < noCtrlText.length(); start += size) {
            result.add(noCtrlText.substring(start, Math.min(noCtrlText.length(), start + size)));
        }

        return result;
    }

    private static String getFormat( final int column1Width ) {
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( column1Width + 5 ).append( 's' ); //$NON-NLS-1$
        return result.toString();
    }
    
    private static String getFormat( final int column1Width,
                              final int column2Width ) {
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( column1Width + 5 ).append( "s%-" ).append( column2Width + 5 ).append( 's' ); //$NON-NLS-1$ //$NON-NLS-2$
        return result.toString();
    }
    
    private static void updateMaxNameWidth(int maxNameWidth, String name) {
        if ( maxNameWidth < name.length() ) {
            maxNameWidth = name.length();
        }
    }

    private static String getHeaderDelimiter( final int width ) {
        final StringBuilder dashes = new StringBuilder();

        for ( int i = 0; i < ( width ); ++i ) {
            dashes.append( HYPHEN );
        }

        return dashes.toString();
    }

}
