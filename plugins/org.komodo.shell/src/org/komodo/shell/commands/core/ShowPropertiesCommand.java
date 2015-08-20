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
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.WorkspaceContextImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * Show Command.  Has various acceptable args.
 * show < properties | children | status | global | property | summary >
 *
 */
public class ShowPropertiesCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "show-properties"; //$NON-NLS-1$

    private static final int DEFAULT_WIDTH = 25;
    private static final int MAX_PROPERTY_VALUE_WIDTH = 100;  // Limit on the value column width

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowPropertiesCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        // Validates arguments
        if (!validate(getArguments())) {
            return false;
        }
        
        WorkspaceStatus wsStatus = getWorkspaceStatus();

		try {
		    String pathArg = optionalArgument(0);
		    WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

		    // Print properties for the context
		    printProperties(theContext);
		} catch (InvalidCommandArgumentException e) {
		    throw e;
		} catch (Exception e) {
		    print( MESSAGE_INDENT, Messages.getString( Messages.ShowCommand.Failure, e.getLocalizedMessage() ) ); 
		    return false;
		}
        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }

	/**
	 * Shows the status at the current workspace context
	 * @throws Exception
	 */
    private void printProperties( final WorkspaceContext context ) throws Exception {
        // show unfiltered properties if necessary
        List< String > props = null;

        if ( getWorkspaceStatus().isShowingHiddenProperties() ) {
            props = ( ( WorkspaceContextImpl )context ).getUnfilteredProperties();
        } else {
            props = context.getProperties();
        }

        if ( props.isEmpty() ) {
            final String noPropsMsg = Messages.getString( Messages.ShowCommand.NoPropertiesMsg, context.getType(), context.getFullName() ); 
            print( MESSAGE_INDENT, noPropsMsg );
            return;
        }

        final Map< String, String > sorted = new TreeMap<>();
        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;

        // loop through properties getting value, removing namespace prefix if necessary, finding widest property name
        for ( int i = 0, size = props.size(); i < size; ++i ) {
            String name = props.get( i );
            String value = null;

            if ( getWorkspaceStatus().isShowingHiddenProperties() ) {
                value = ( ( WorkspaceContextImpl )context ).getUnfilteredPropertyValue( name );
            } else {
                value = context.getPropertyValue( name );

                if ( StringUtils.isBlank( value ) ) {
                    value = Messages.getString( SHELL.NO_PROPERTY_VALUE );
                }
            }

            if ( !isShowingPropertyNamePrefixes() ) {
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
        final String propListHeader = Messages.getString( Messages.ShowCommand.PropertiesHeader, objType, objFullName );
        print( MESSAGE_INDENT, propListHeader );

        final String format = getFormat( maxNameWidth, maxValueWidth );
        print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ), getHeaderDelimiter( maxValueWidth ) ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
        	String propName = entry.getKey();
        	String propValue = entry.getValue();
        	// propValue less than maximum width
        	if(propValue.length() <= maxValueWidth) {
        		print( MESSAGE_INDENT, String.format( format, propName, propValue ) );
        	// propValue exceeds maximum width - splits it up onto separate lines
        	} else {
        		printPropWithLongValue(format,propName,propValue,maxValueWidth);
        	}
        }
    }

    private void printPropWithLongValue(String format, String propName, String propValue, int maxValueWidth) {
		// splits long strings into equal length lines of 'maxValueWidth' length.
		List<String> lines = splitEqually(propValue,maxValueWidth);
		boolean first = true;
		for(String line : lines) {
			// First line includes the propName
			if(first) {
        		print( MESSAGE_INDENT, String.format( format, propName, line ) );
				first = false;
			// Subsequent lines the 'name' is just a spacer
			} else {
        		print( MESSAGE_INDENT, String.format( format, EMPTY_STRING, line ) );
			}
		}
    }

    private List<String> splitEqually(String text, int size) {
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

    private String getFormat( final int column1Width,
                              final int column2Width ) {
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( column1Width + 5 ).append( "s%-" ).append( column2Width + 5 ).append( 's' ); //$NON-NLS-1$ //$NON-NLS-2$
        return result.toString();
    }

    private String getHeaderDelimiter( final int width ) {
        final StringBuilder dashes = new StringBuilder();

        for ( int i = 0; i < ( width ); ++i ) {
            dashes.append( HYPHEN );
        }

        return dashes.toString();
    }

	protected boolean validate(Arguments allArgs) throws Exception {
		// optional path arg
	    if(!allArgs.isEmpty()) {
	        // Optional path arg
	        String pathArg = optionalArgument(0);
	        if(!validatePath(pathArg)) {
	            return false;
	        }
	    }

		return true;
	}

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {

        if (getArguments().isEmpty()) {
            // The arg is expected to be a path
            updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

            // Do not put space after it - may want to append more to the path
            return CompletionConstants.NO_APPEND_SEPARATOR;
            // Tab completion for "property" - expects a valid property for the current context.
        }

    	return -1;
    }

}
