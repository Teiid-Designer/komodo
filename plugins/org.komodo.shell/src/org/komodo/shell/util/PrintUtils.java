/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.util;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;

import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.ShellApiI18n;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 *  Utilities for formatted printing
 */
public class PrintUtils implements StringConstants {

    private static final int DEFAULT_WIDTH = 25;
    private static final int MAX_PROPERTY_VALUE_WIDTH = 100;  // Limit on the value column width
    private static final int MAX_DEFAULT_VALUE_WIDTH = ( MAX_PROPERTY_VALUE_WIDTH / 3 );

    /**
     * Print the message to the writer.  A newLine is added after the message.
     * @param writer the Writer
     * @param indent number of indent spaces
     * @param formattedMessage the message
     * @param params message params
     */
    public static void print(Writer writer, int indent, String formattedMessage, Object... params) {
        print(writer,true,indent,formattedMessage,params);
    }

    /**
     * Print to writer with supplied info
     * @param writer the Writer
     * @param addNewLine 'true' to add new line after message.
     * @param indent number of indent spaces
     * @param formattedMessage the message
     * @param params message params
     */
    public static void print(Writer writer, boolean addNewLine, int indent,String formattedMessage, Object... params) {
        if(writer==null) return;

        ArgCheck.isNonNegative(indent, I18n.bind(ShellApiI18n.negativeIndentSupplied));
        StringBuffer sb = new StringBuffer();
        for(int i=0; i<indent; i++) {
            sb.append(StringConstants.SPACE);
        }
        String msg = String.format(formattedMessage, params);
        try {
            writer.write(sb.toString()+msg);
            if(addNewLine) writer.write(NEW_LINE);
            writer.flush();
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println(msg);
        }
    }

    /**
     * @param writer the Writer
     * @param items the item list
     * @param headerTitle name for header
     */
    public static void printList( final Writer writer, List<String> items, String headerTitle ) {
        int maxNameWidth = DEFAULT_WIDTH;
        for(String item : items) {
            maxNameWidth = updateMaxNameWidth(maxNameWidth,item);
        }

        // Sort alphabetically
        Collections.sort(items);

        // Print Header
        final String format = getFormat( maxNameWidth );
        print( writer, MESSAGE_INDENT, String.format( format, headerTitle ) );
        print( writer, MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ) ) );

        // Print each name
        for ( final String name : items ) {
            print( writer, MESSAGE_INDENT, name );
        }
    }

    /**
     * Prints a list with multiple items per line
     * @param indent number of spaces to indent
     * @param writer the Writer
     * @param items the item list
     * @param itemsPerLine the number of items on each line
     * @param headerTitle name for header
     */
    public static void printMultiLineItemList( int indent, final Writer writer, List<String> items, int itemsPerLine, String headerTitle ) {
        StringBuffer indentBuffer = new StringBuffer();
        for(int i=0; i<indent; i++) {
            indentBuffer.append(StringConstants.SPACE);
        }

        int maxNameWidth = DEFAULT_WIDTH;
        for(String item : items) {
            maxNameWidth = updateMaxNameWidth(maxNameWidth,item);
        }

        // Print Header first if supplied
        if(!StringUtils.isBlank(headerTitle)) {
            final String format = getFormat( maxNameWidth );
            print( writer, MESSAGE_INDENT, String.format( format, headerTitle ) );
            print( writer, MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ) ) );
        }

        // Print appropriate commands per line
        int colCount = 0;
        StringBuilder builder = new StringBuilder();
        for (String item : items) {
            builder.append(String.format("%-"+(maxNameWidth+5)+"s", item)); //$NON-NLS-1$ //$NON-NLS-2$
            colCount++;

            if (colCount == itemsPerLine) {
                builder.append("\n"+indentBuffer.toString()); //$NON-NLS-1$
                colCount = 0;
            }
        }
        print(writer, MESSAGE_INDENT, builder.toString());
    }

    /**
     * Print the supplied properties
     * @param writer the output writer
     * @param props the properties
     * @param nameTitle the name title
     * @param valueTitle the value title
     */
    public static void printProperties(final Writer writer, Properties props, String nameTitle, String valueTitle) {
        final Map< String, String > sorted = new TreeMap<>();
        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;

        for(String name : props.stringPropertyNames()) {
            String value = props.getProperty(name);
            if(StringUtils.isEmpty(value)) {
                value = I18n.bind( ShellI18n.noPropertyValue );
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

        print( writer, MESSAGE_INDENT, String.format( format, nameTitle, valueTitle ) );
        print( writer,
               MESSAGE_INDENT,
               String.format( format, getHeaderDelimiter( maxNameWidth ), getHeaderDelimiter( maxValueWidth ) ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
            String propName = entry.getKey();
            String propValue = entry.getValue();
            // propValue less than maximum width
            if(propValue.length() <= maxValueWidth) {
                print( writer, MESSAGE_INDENT, String.format( format, propName, propValue ) );
                // propValue exceeds maximum width - splits it up onto separate lines
            } else {
                printPropWithLongValue(writer,format,propName,propValue,null,maxValueWidth);
            }
        }
    }

    /**
     * Prints the properties for a context
     * @param wsStatus the WorkspaceStatus
     * @param writer the Writer
     * @param showHiddenProps 'true' to show hidden properties
     * @param showPropPrefixes 'true' to show property prefixes
     * @param context the workspace context
     * @throws Exception the exception
     */
    public static void printProperties( final WorkspaceStatus wsStatus, final Writer writer, boolean showHiddenProps, boolean showPropPrefixes, final KomodoObject context ) throws Exception {
        // show unfiltered properties if necessary
        List< String > props = null;

        if ( showHiddenProps ) {
            props = KomodoObjectUtils.getUnfilteredProperties(wsStatus,context);
        } else {
            props = KomodoObjectUtils.getProperties(wsStatus,context);
        }

        if ( props.isEmpty() ) {
            final String path = wsStatus.getDisplayPath(context);
            final String noPropsMsg = I18n.bind( ShellI18n.noPropertiesMsg, wsStatus.getTypeDisplay(context), path );
            print( writer, MESSAGE_INDENT, noPropsMsg );
            return;
        }

        // key is the property name, value is a 2 element string array holding the current value and the default value
        final Map< String, String[] > sorted = new TreeMap<>();

        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;

        // loop through properties getting value and default value, removing namespace prefix if necessary, finding widest property name
        for ( int i = 0, size = props.size(); i < size; ++i ) {
            String name = props.get( i );
            String value = null;
            final String defaultValue = getPropertyDefaultValue( wsStatus, context, name );

            if ( showHiddenProps ) {
                value = KomodoObjectUtils.getUnfilteredPropertyValue(wsStatus, context, name );
            } else {
                value = KomodoObjectUtils.getPropertyValue(wsStatus, context, name );

                if ( StringUtils.isBlank( value ) ) {
                    value = I18n.bind( ShellI18n.noPropertyValue );
                }
            }

            if ( !showPropPrefixes ) {
                name = KomodoObjectUtils.removePrefix( props.get( i ) );
            }

            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            if ( maxValueWidth < value.length() ) {
                maxValueWidth = value.length();
            }

            sorted.put( name, new String[] { value, defaultValue } );
        }

        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
            maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }

        // Print properties header
        final String objType = wsStatus.getTypeDisplay(context); // current object type
        final String path = wsStatus.getDisplayPath(context);
        final String propListHeader = I18n.bind( ShellI18n.propertiesHeader, objType, path );
        print( writer, MESSAGE_INDENT, propListHeader );

        final int maxDefaultValueWidth = Math.min( maxValueWidth, MAX_DEFAULT_VALUE_WIDTH );
        final String format = getFormat( new int[] { maxNameWidth,
                                                     maxValueWidth,
                                                     maxDefaultValueWidth } );

        print( writer,
               MESSAGE_INDENT,
               String.format( format,
                              I18n.bind( ShellI18n.propertyNameHeader ),
                              I18n.bind( ShellI18n.propertyValueHeader ),
                              I18n.bind( ShellI18n.propertyDefaultValueHeader ) ) );
        print( writer,
               MESSAGE_INDENT,
               String.format( format,
                              PrintUtils.getHeaderDelimiter( maxNameWidth ),
                              PrintUtils.getHeaderDelimiter( maxValueWidth ),
                              PrintUtils.getHeaderDelimiter( maxDefaultValueWidth ) ) );

        // print property name, value, and default value
        for ( final Entry< String, String[] > entry : sorted.entrySet() ) {
            String propName = entry.getKey();
            String propValue = entry.getValue()[ 0 ];
            final String defaultValue = entry.getValue()[ 1 ];
            // propValue less than maximum width
            if(propValue.length() <= maxValueWidth) {
                print( writer, MESSAGE_INDENT, String.format( format, propName, propValue, defaultValue ) );
            // propValue exceeds maximum width - splits it up onto separate lines
            } else {
                printPropWithLongValue(writer,format,propName,propValue,defaultValue,maxValueWidth);
            }
        }
    }

    /**
     * Shows the komodo object property with the specified name
     * @param wsStatus the WorkspaceStatus
     * @param writer the Writer
     * @param context the workspace context
     * @param name the property name
     * @throws Exception the exception
     */
    public static void printProperty( final WorkspaceStatus wsStatus, final Writer writer, KomodoObject context, String name ) throws Exception {
        String propertyName = KomodoObjectUtils.attachPrefix( wsStatus, context, name );

        // Get the value for the supplied property
        String propValue = KomodoObjectUtils.getPropertyValue(wsStatus, context, propertyName );

        // default value
        final String defaultValue = getPropertyDefaultValue( wsStatus, context, name );

        if ( StringUtils.isBlank( propValue ) ) {
            propValue = I18n.bind( ShellI18n.noPropertyValue );
        }

        if ( !wsStatus.isShowingPropertyNamePrefixes() ) {
            propertyName = KomodoObjectUtils.removePrefix( propertyName );
        }
        final int maxNameWidth = Math.max( DEFAULT_WIDTH, propertyName.length() );

        int maxValueWidth = Math.max( DEFAULT_WIDTH, propValue.length() );

        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
            maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }

        final int maxDefaultValueWidth = Math.min( maxValueWidth, MAX_DEFAULT_VALUE_WIDTH );
        final String format = PrintUtils.getFormat( maxNameWidth, maxValueWidth, maxDefaultValueWidth );

        // Print property header
        final String path = wsStatus.getDisplayPath(context);
        String propListHeader = I18n.bind( ShellI18n.propertyHeader, wsStatus.getTypeDisplay(context), path );
        print( writer, MESSAGE_INDENT, propListHeader );
        print( writer,
               MESSAGE_INDENT,
               String.format( format,
                              I18n.bind( ShellI18n.propertyNameHeader ),
                              I18n.bind( ShellI18n.propertyValueHeader ),
                              I18n.bind( ShellI18n.propertyDefaultValueHeader ) ) );
        print( writer,
               MESSAGE_INDENT,
               String.format( format,
                              PrintUtils.getHeaderDelimiter( maxNameWidth ),
                              PrintUtils.getHeaderDelimiter( maxValueWidth ),
                              PrintUtils.getHeaderDelimiter( maxDefaultValueWidth ) ) );

        // propValue less than maximum width
        if(propValue.length() <= maxValueWidth) {
            print( writer, MESSAGE_INDENT, String.format( format, propertyName, propValue, defaultValue ) );
        // propValue exceeds maximum width - splits it up onto separate lines
        } else {
            printPropWithLongValue(writer,format,propertyName,propValue,defaultValue,maxValueWidth);
        }

    }

    /**
     * @param wsStatus the Workspace Status
     * @param writer the writer
     * @param context the workspace context
     * @throws Exception the exception
     */
    public static void printChildren( final WorkspaceStatus wsStatus, final Writer writer, final KomodoObject context ) throws Exception {
        final KomodoObject[] children = context.getChildren(wsStatus.getTransaction());
        List<KomodoObject> childList = Arrays.asList(children);

        if ( childList.isEmpty() ) {
            final String path = wsStatus.getDisplayPath(context);
            String noChildrenMsg = I18n.bind( ShellI18n.noChildrenMsg, wsStatus.getTypeDisplay(context), path );
            print(writer, MESSAGE_INDENT, noChildrenMsg );
            return;
        }

        int maxNameWidth = DEFAULT_WIDTH;
        int maxTypeWidth = DEFAULT_WIDTH;

        // loop through children getting name, type, and finding widest child name
        for ( int i = 0, size = childList.size(); i < size; ++i ) {
            final String name = wsStatus.getLabelProvider().getDisplayName( childList.get( i ) );

            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            final String type = wsStatus.getTypeDisplay(childList.get(i));

            if ( maxTypeWidth < type.length() ) {
                maxTypeWidth = type.length();
            }
        }

        // sort
        final Comparator< KomodoObject > sorter = new Comparator< KomodoObject >() {

            /**
             * {@inheritDoc}
             *
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare( final KomodoObject thisContext,
                                final KomodoObject thatContext ) {
                try {
                    final String thisType = wsStatus.getTypeDisplay(thisContext);
                    int result = thisType.compareTo( wsStatus.getTypeDisplay(thatContext) );

                    if ( result == 0 ) {
                        final String thisName = wsStatus.getLabelProvider().getDisplayName( thisContext );
                        final String thatName = wsStatus.getLabelProvider().getDisplayName( thatContext );
                        return thisName.compareTo( thatName );
                    }

                    return result;
                } catch ( final Exception e ) {
                    KLog.getLogger().error( "Error comparing WorkspaceContext objects", e ); //$NON-NLS-1$
                    return 0;
                }
            }

        };
        Collections.sort( childList, sorter );

        // Print children header
        final String path = wsStatus.getDisplayPath(context);
        final String childrenHeader = I18n.bind( ShellI18n.childrenHeader, wsStatus.getTypeDisplay(context), path );
        print( writer, MESSAGE_INDENT, childrenHeader );

        final String format = PrintUtils.getFormat( maxNameWidth, maxTypeWidth );
        print( writer, MESSAGE_INDENT,
               String.format( format, I18n.bind( ShellI18n.childNameHeader ), I18n.bind( ShellI18n.childTypeHeader ) ) );
        print( writer, MESSAGE_INDENT, String.format( format, PrintUtils.getHeaderDelimiter( maxNameWidth ), PrintUtils.getHeaderDelimiter( maxTypeWidth ) ) );

        // Print each child
        for ( final KomodoObject childContext : childList ) {
            final String childName = wsStatus.getLabelProvider().getDisplayName( childContext );
            final String childType = wsStatus.getTypeDisplay(childContext);
            print( writer, MESSAGE_INDENT, String.format( format, childName, childType ) );
        }
    }

    /**
     * Prints properties with property values longer than maxValueWidth, splitting them into separate lines
     * @param writer the output writer
     * @param format the format
     * @param propName the property name
     * @param propValue the property value
     * @param defaultValue the default value of the property (can be empty)
     * @param maxValueWidth maximum width of the value
     */
    public static void printPropWithLongValue( Writer writer,
                                               String format,
                                               String propName,
                                               String propValue,
                                               String defaultValue,
                                               int maxValueWidth ) {
        // splits long strings into equal length lines of 'maxValueWidth' length.
        List<String> lines = splitEqually(propValue,maxValueWidth);
        defaultValue = ( StringUtils.isBlank( defaultValue ) ? EMPTY_STRING : defaultValue );
        boolean first = true;
        for(String line : lines) {
            // First line includes the propName
            if(first) {
                print( writer, MESSAGE_INDENT, String.format( format, propName, line, defaultValue ) );
                first = false;
            // Subsequent lines the 'name' is just a spacer
            } else {
                print( writer, MESSAGE_INDENT, String.format( format, EMPTY_STRING, line, EMPTY_STRING ) );
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
     * Gets a format string for as many columns as there are widths passed in.
     *
     * @param widths
     *        the column widths (cannot be <code>null</code>)
     * @return the format string (never <code>null</code> but can be empty if no widths are passed in)
     */
    public static String getFormat( final int... widths ) {
        ArgCheck.isNotNull( widths, "widths" ); //$NON-NLS-1$
        final StringBuilder result = new StringBuilder();

        for ( final int width : widths ) {
            result.append( "%-" ).append( width + 5 ).append( 's' ); //$NON-NLS-1$
        }

        return result.toString();
    }

    private static int updateMaxNameWidth(int maxNameWidth, String name) {
        if ( maxNameWidth < name.length() ) {
            maxNameWidth = name.length();
        }
        return maxNameWidth;
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

    private static String getPropertyDefaultValue( final WorkspaceStatus wsStatus,
                                                   final KomodoObject kobject,
                                                   final String propName ) throws KException {
        final PropertyDescriptor descriptor = kobject.getPropertyDescriptor( wsStatus.getTransaction(), propName );

        // no property descriptor so no way to find default value
        if ( descriptor == null ) {
            return StringConstants.EMPTY_STRING;
        }

        final Object[] defaultValues = descriptor.getDefaultValues();

        // there are no default values in the descriptor
        if ( ( defaultValues == null ) || ( defaultValues.length == 0 ) ) {
            return StringConstants.EMPTY_STRING;
        }

        // delimit default values by a comma
        final StringBuilder builder = new StringBuilder();
        boolean firstTime = true;

        for ( final Object obj : defaultValues ) {
            if ( firstTime ) {
                firstTime = false;
            } else {
                builder.append( ',' );
            }

            builder.append( obj );
        }

        return builder.toString();
    }

}
