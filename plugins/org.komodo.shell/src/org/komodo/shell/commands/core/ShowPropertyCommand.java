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
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * Show Command.  Has various acceptable args.
 * show < properties | children | status | global | property | summary >
 *
 */
public class ShowPropertyCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "show-property"; //$NON-NLS-1$

    private static final int DEFAULT_WIDTH = 25;
    private static final int MAX_PROPERTY_VALUE_WIDTH = 100;  // Limit on the value column width

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowPropertyCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        // Validates the sub-command arg
        if (!validate(getArguments())) {
			return false;
		}

        WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext context = wsStatus.getCurrentContext();

		try {
		    String propName = requiredArgument(1, Messages.getString(Messages.ShowCommand.InvalidArgMsg_PropertyName)); 

		    printProperty(context,propName);
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

	/**
	 * Shows the komodo object property with the specified name
	 * @throws Exception
	 */
    private void printProperty( WorkspaceContext context,
                                String name ) throws Exception {
        final String propertyName = attachPrefix( context, name );
        final int maxNameWidth = Math.max( DEFAULT_WIDTH, propertyName.length() );

        // Get the value for the supplied property
        String propValue = context.getPropertyValue( propertyName );

        if ( StringUtils.isBlank( propValue ) ) {
            propValue = Messages.getString( SHELL.NO_PROPERTY_VALUE );
        }

        int maxValueWidth = Math.max( DEFAULT_WIDTH, propValue.length() );

        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
        	maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }

        final String format = getFormat( maxNameWidth, maxValueWidth );

        // Print properties header
        String propListHeader = Messages.getString( Messages.ShowCommand.PropertyHeader, context.getType(), context.getFullName() ); 
        print( MESSAGE_INDENT, propListHeader );
        print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ), getHeaderDelimiter( maxValueWidth ) ) );

    	// propValue less than maximum width
    	if(propValue.length() <= maxValueWidth) {
    		print( MESSAGE_INDENT, String.format( format, name, propValue ) );
    	// propValue exceeds maximum width - splits it up onto separate lines
    	} else {
    		printPropWithLongValue(format,name,propValue,maxValueWidth);
    	}

        print();
    }

	protected boolean validate(Arguments allArgs) throws Exception {
//		// Validate the sub-command
//		String subCmd = allArgs.get(0).trim().toLowerCase();
//		if(!validateSubCmd(subCmd)) {
//			return false;
//		}
//
//		// SubCommand ok, continue with second arg for commands with second args
//		if(allArgs.size()>1) {
//			// For "properties", "children" and "summary" commands - a second argument (path) is optional.
//			if(subCmd.equals(SUBCMD_PROPERTIES) || subCmd.equals(SUBCMD_CHILDREN) || subCmd.equals(SUBCMD_SUMMARY)) {
//				// Optional path arg
//		        String secondArg = optionalArgument(1);
//				if(!validatePath(secondArg)) {
//					return false;
//				}
//			// For "property", the second arg (propName) is required.
//			} else if(subCmd.equals(SUBCMD_PROPERTY)) {
//				// Second required arg is the property name.  Verify that it is valid for the current object
//		        String propName = requiredArgument(1, Messages.getString(Messages.ShowCommand.InvalidArgMsg_PropertyName));
//				WorkspaceContext context = getContext();
//				if(!validatePropertyName(context, propName)) {
//					return false;
//				}
//			}
//		}

		return true;
	}

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
//        // TODO needs to work with properties without their prefix
//
//    	if (getArguments().isEmpty()) {
//    		// --------------------------------------------------------------
//    		// No arg - offer subcommands
//    		// --------------------------------------------------------------
//    		if(lastArgument==null) {
//    			candidates.addAll(SUBCMDS);
//                return 0;
//    		}
//
//            for ( final String item : SUBCMDS ) {
//                if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
//                    candidates.add( item );
//                }
//            }
//
//            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
//    	}
//
//    	if (getArguments().size()==1) {
//    		String cmdArgLower = getArguments().get(0).toLowerCase();
//    		// Tab completion for "properties", "children", "summary" - expects a path arg
//    		if(SUBCMD_PROPERTIES.equals(cmdArgLower) || SUBCMD_CHILDREN.equals(cmdArgLower) || SUBCMD_SUMMARY.equals(cmdArgLower)) {
//    			// The arg is expected to be a path
//    			updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);
//
//    			// Do not put space after it - may want to append more to the path
//    			return CompletionConstants.NO_APPEND_SEPARATOR;
//    			// Tab completion for "property" - expects a valid property for the current context.
//    		}
//
//    		if(SUBCMD_PROPERTY.equals(cmdArgLower)) {
//    		    updateTabCompleteCandidatesForProperty(candidates, getContext(), lastArgument);
//
//                if ( StringUtils.isBlank( lastArgument ) ) {
//                    return 0;
//                }
//
//                return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
//            }
//    	}
//
    	return -1;
    }

}
