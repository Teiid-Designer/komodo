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
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.WorkspaceContextImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
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
    
    /**
     * Prints the properties for a context
     * @param command the shell command
     * @param showHiddenProps 'true' to show hidden properties
     * @param showPropPrefixes 'true' to show property prefixes
     * @param context the workspace context
     * @throws Exception the exception
     */
    public static void printProperties( ShellCommand command, boolean showHiddenProps, boolean showPropPrefixes, final WorkspaceContext context ) throws Exception {
        // show unfiltered properties if necessary
        List< String > props = null;

        if ( showHiddenProps ) {
            props = ( ( WorkspaceContextImpl )context ).getUnfilteredProperties();
        } else {
            props = context.getProperties();
        }

        if ( props.isEmpty() ) {
            final String noPropsMsg = Messages.getString( SHELL.NoPropertiesMsg, context.getType(), context.getFullName() ); 
            command.print( MESSAGE_INDENT, noPropsMsg );
            return;
        }

        final Map< String, String > sorted = new TreeMap<>();
        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;

        // loop through properties getting value, removing namespace prefix if necessary, finding widest property name
        for ( int i = 0, size = props.size(); i < size; ++i ) {
            String name = props.get( i );
            String value = null;

            if ( showHiddenProps ) {
                value = ( ( WorkspaceContextImpl )context ).getUnfilteredPropertyValue( name );
            } else {
                value = context.getPropertyValue( name );

                if ( StringUtils.isBlank( value ) ) {
                    value = Messages.getString( SHELL.NO_PROPERTY_VALUE );
                }
            }

            if ( !showPropPrefixes ) {
                name = removePrefix( props.get( i ) );
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

        // Print properties header
        final String objType = context.getType(); // current object type
        final String objFullName = context.getFullName(); // current object name
        final String propListHeader = Messages.getString( SHELL.PropertiesHeader, objType, objFullName );
        command.print( MESSAGE_INDENT, propListHeader );

        final String format = PrintUtils.getFormat( maxNameWidth, maxValueWidth );
        command.print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        command.print( MESSAGE_INDENT, String.format( format, PrintUtils.getHeaderDelimiter( maxNameWidth ), PrintUtils.getHeaderDelimiter( maxValueWidth ) ) );

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
    
    /**
     * Shows the komodo object property with the specified name
     * @param command the shell command
     * @param context the workspace context
     * @param name the property name
     * @throws Exception the exception
     */
    public static void printProperty( ShellCommand command, WorkspaceContext context, String name ) throws Exception {
        String propertyName = attachPrefix( context, name );

        // Get the value for the supplied property
        String propValue = context.getPropertyValue( propertyName );

        if ( StringUtils.isBlank( propValue ) ) {
            propValue = Messages.getString( SHELL.NO_PROPERTY_VALUE );
        }
        
        if ( !context.getWorkspaceStatus().isShowingPropertyNamePrefixes() ) {
            propertyName = removePrefix( propertyName );
        }
        final int maxNameWidth = Math.max( DEFAULT_WIDTH, propertyName.length() );

        int maxValueWidth = Math.max( DEFAULT_WIDTH, propValue.length() );

        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
            maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }

        final String format = PrintUtils.getFormat( maxNameWidth, maxValueWidth );

        // Print properties header
        String propListHeader = Messages.getString( SHELL.PropertyHeader, context.getType(), context.getFullName() ); 
        command.print( MESSAGE_INDENT, propListHeader );
        command.print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        command.print( MESSAGE_INDENT, String.format( format, PrintUtils.getHeaderDelimiter( maxNameWidth ), PrintUtils.getHeaderDelimiter( maxValueWidth ) ) );

        // propValue less than maximum width
        if(propValue.length() <= maxValueWidth) {
            command.print( MESSAGE_INDENT, String.format( format, propertyName, propValue ) );
        // propValue exceeds maximum width - splits it up onto separate lines
        } else {
            printPropWithLongValue(command,format,propertyName,propValue,maxValueWidth);
        }

    }
    
    /**
     * @param command the shell command
     * @param context the workspace context
     * @throws Exception the exception
     */
    public static void printChildren( final ShellCommand command, final WorkspaceContext context ) throws Exception {
        final List< WorkspaceContext > children = context.getChildren();

        if ( children.isEmpty() ) {
            String noChildrenMsg = Messages.getString( SHELL.noChildrenMsg, context.getType(), context.getFullName() );
            command.print( MESSAGE_INDENT, noChildrenMsg );
            return;
        }

        int maxNameWidth = DEFAULT_WIDTH;
        int maxTypeWidth = DEFAULT_WIDTH;

        // loop through children getting name, type, and finding widest child name
        for ( int i = 0, size = children.size(); i < size; ++i ) {
            final String name = children.get( i ).getName();

            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            final String type = children.get( i ).getType();

            if ( maxTypeWidth < type.length() ) {
                maxTypeWidth = type.length();
            }
        }

        // sort
        final Comparator< WorkspaceContext > sorter = new Comparator< WorkspaceContext >() {

            /**
             * {@inheritDoc}
             *
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare( final WorkspaceContext thisContext,
                                final WorkspaceContext thatContext ) {
                try {
                    final String thisType = thisContext.getType();
                    int result = thisType.compareTo( thatContext.getType() );

                    if ( result == 0 ) {
                        return thisContext.getName().compareTo( thatContext.getName() );
                    }

                    return result;
                } catch ( final Exception e ) {
                    KLog.getLogger().error( "Error comparing WorkspaceContext objects", e ); //$NON-NLS-1$
                    return 0;
                }
            }

        };
        Collections.sort( children, sorter );

        // Print children header
        final String childrenHeader = Messages.getString( SHELL.ChildrenHeader, context.getType(), context.getFullName() );
        command.print( MESSAGE_INDENT, childrenHeader );

        final String format = PrintUtils.getFormat( maxNameWidth, maxTypeWidth );
        command.print( MESSAGE_INDENT,
               String.format( format, Messages.getString( SHELL.CHILD_NAME_HEADER ), Messages.getString( SHELL.CHILD_TYPE_HEADER ) ) );
        command.print( MESSAGE_INDENT, String.format( format, PrintUtils.getHeaderDelimiter( maxNameWidth ), PrintUtils.getHeaderDelimiter( maxTypeWidth ) ) );

        // Print each child
        for ( final WorkspaceContext childContext : children ) {
            final String childName = childContext.getName();
            final String childType = childContext.getType();
            command.print( MESSAGE_INDENT, String.format( format, childName, childType ) );
        }
    }
    
    /**
     * @param context
     *        the associated context (cannot be null)
     * @param propertyName
     *        the name whose namespace prefix is being attached (cannot be empty)
     * @return the property name with the namespace prefix attached (never empty)
     * @throws Exception
     *         if an error occurs
     */
    protected static String attachPrefix( final WorkspaceContext context,
                                          final String propertyName ) throws Exception {
        ArgCheck.isNotNull( context, "context" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$

        for ( final String name : context.getProperties() ) {
            if ( propertyName.equals( removePrefix( name ) ) ) {
                return name;
            }
        }

        return propertyName;
    }
    
    /**
     * @param propertyName
     *        the property name whose namespace prefix is being removed (cannot be empty)
     * @return the name without the namespace prefix (never empty)
     */
    private static String removePrefix( final String propertyName ) {
        ArgCheck.isNotEmpty( propertyName, "qname" ); //$NON-NLS-1$
        final int index = propertyName.indexOf( ':' );

        if ( index == -1 ) {
            return propertyName;
        }

        if ( index < propertyName.length() ) {
            return propertyName.substring( index + 1 );
        }

        return propertyName;
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

    /**
     * gets a format string for 1 column
     * @param column1Width the width of column1
     * @return the format string
     */
    private static String getFormat( final int column1Width ) {
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( column1Width + 5 ).append( 's' ); //$NON-NLS-1$
        return result.toString();
    }
    
    /**
     * gets a format string for 2 columns
     * @param column1Width the width of column1
     * @param column2Width the width of column2
     * @return the format string
     */
    public static String getFormat( final int column1Width,
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

    /**
     * Get header delimiter string of specified width
     * @param width the string width
     * @return the string delimiter
     */
    public static String getHeaderDelimiter( final int width ) {
        final StringBuilder dashes = new StringBuilder();

        for ( int i = 0; i < ( width ); ++i ) {
            dashes.append( HYPHEN );
        }

        return dashes.toString();
    }

}
