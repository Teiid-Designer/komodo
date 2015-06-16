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
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.KLog;

/**
 * Show Command.  Has various acceptable args.
 * show < properties | children | status | global | property | summary >
 *
 */
public class ShowCommand extends BuiltInShellCommand implements StringConstants {

    private static final String SHOW = "show"; //$NON-NLS-1$
    private static final int DEFAULT_WIDTH = 25;

    private static final String SUBCMD_PROPERTIES = "properties"; //$NON-NLS-1$

    /**
     * The sub-command for showing children of the current context.
     */
    public static final String SUBCMD_CHILDREN = "children"; //$NON-NLS-1$

    private static final String SUBCMD_STATUS = "status"; //$NON-NLS-1$
    private static final String SUBCMD_GLOBAL = "global"; //$NON-NLS-1$
    private static final String SUBCMD_PROPERTY = "property"; //$NON-NLS-1$
    private static final String SUBCMD_SUMMARY = "summary"; //$NON-NLS-1$
    private static final List< String > SUBCMDS = Arrays.asList( SUBCMD_PROPERTIES,
                                                                 SUBCMD_CHILDREN,
                                                                 SUBCMD_STATUS,
                                                                 SUBCMD_GLOBAL,
                                                                 SUBCMD_PROPERTY,
                                                                 SUBCMD_SUMMARY );

	/**
	 * Constructor.
	 * @param wsStatus the workspace status
	 */
	public ShowCommand(WorkspaceStatus wsStatus) {
		super(SHOW,wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
    public boolean execute() throws Exception {
		// Make sure the correct number of args supplied
        String subcmdArg = requiredArgument(0, Messages.getString("ShowCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$

        // Validates the sub-command arg
        if (!validate(getArguments())) {
			return false;
		}

        WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext context = wsStatus.getCurrentContext();

        try {
        	// Sub-command which shows all properties
        	if (SUBCMD_PROPERTIES.equalsIgnoreCase(subcmdArg)) {
		        String pathArg = optionalArgument(1);
		        WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

        		// Print properties for the context
        		printProperties(theContext);

                // Echo command if recording on
        		recordCommand();
        	} else if (SUBCMD_CHILDREN.equalsIgnoreCase(subcmdArg)) {
		        String pathArg = optionalArgument(1);
		        WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

        		printChildren(theContext);

                // Echo command if recording on
                recordCommand();
            } else if (SUBCMD_STATUS.equalsIgnoreCase(subcmdArg)) {
            	printStatus( );

                // Echo command if recording on
                recordCommand();
            } else if (SUBCMD_GLOBAL.equalsIgnoreCase(subcmdArg)) {
        		printGlobal( );

                // Echo command if recording on
                recordCommand();
            } else if (SUBCMD_PROPERTY.equalsIgnoreCase(subcmdArg)) {
                String propName = requiredArgument(1, Messages.getString("ShowCommand.InvalidArgMsg_PropertyName")); //$NON-NLS-1$

            	printProperty(context,propName);

                // Echo command if recording on
                recordCommand();
            } else if (SUBCMD_SUMMARY.equalsIgnoreCase(subcmdArg)) {
		        String pathArg = optionalArgument(1);
		        WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

            	printSummary(theContext);

            	// Echo command if recording on
                recordCommand();
            } else {
                throw new InvalidCommandArgumentException(0, Messages.getString("ShowCommand.InvalidSubCommand")); //$NON-NLS-1$
            }
        } catch (InvalidCommandArgumentException e) {
            throw e;
        } catch (Exception e) {
            print(MESSAGE_INDENT, Messages.getString("ShowCommand.Failure")); //$NON-NLS-1$
            print(MESSAGE_INDENT, "\t" + e.getMessage()); //$NON-NLS-1$
            return false;
        }
        return true;
    }

	/**
	 * Shows the status at the current workspace context
	 * @throws Exception
	 */
    private void printProperties( WorkspaceContext context ) throws Exception {
        final List< String > props = context.getProperties(); // All properties

        if ( props.isEmpty() ) {
            final String noPropsMsg = Messages.getString( "ShowCommand.NoPropertiesMsg", context.getType(), context.getFullName() ); //$NON-NLS-1$
            print( MESSAGE_INDENT, noPropsMsg );
            return;
        }

        final Map< String, String > sorted = new TreeMap<>();
        int maxWidth = DEFAULT_WIDTH;

        // loop through properties getting value, removing namespace prefix if necessary, finding widest property name
        for ( int i = 0, size = props.size(); i < size; ++i ) {
            String name = props.get( i );
            final String value = context.getPropertyValue( name );

            if ( !isShowingPropertyNamePrefixes() ) {
                name = removePrefix( props.get( i ) );

                if ( maxWidth < name.length() ) {
                    maxWidth = name.length();
                }
            }

            sorted.put( name, value );
        }

        // Print properties header
        final String objType = context.getType(); // current object type
        final String objFullName = context.getFullName(); // current object name
        final String propListHeader = Messages.getString( "ShowCommand.PropertiesHeader", objType, objFullName ); //$NON-NLS-1$
        print( MESSAGE_INDENT, propListHeader );

        final String format = getFormat( maxWidth );
        final String headerDelimiter = getHeaderDelimiter( maxWidth );
        print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, headerDelimiter, headerDelimiter ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
            print( MESSAGE_INDENT, String.format( format, entry.getKey(), entry.getValue() ) );
        }
    }

    private String getFormat( final int width ) {
        final int columnWidth = ( width + 5 );
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( columnWidth ).append( "s%-" ).append( columnWidth ).append( 's' ); //$NON-NLS-1$ //$NON-NLS-2$
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
	 * Shows the status at the current workspace context
	 * @throws Exception
	 */
	private void printStatus( ) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        WorkspaceContext context = wsStatus.getCurrentContext();

        // Repo info
        final Repository.Id repoId = context.getRepository().getId();
		print(MESSAGE_INDENT,
		      Messages.getString("ShowCommand.CurrentRepo", repoId.getUrl(), repoId.getWorkspaceName())); //$NON-NLS-1$

		// Teiid Instance info
		Teiid teiid = wsStatus.getTeiid();
		if (teiid == null)
			print(MESSAGE_INDENT, Messages.getString("ShowCommand.NoCurrentTeiid")); //$NON-NLS-1$
		else {
			TeiidInstance teiidInstance = teiid.getTeiidInstance();
			String teiidUrl = teiidInstance.getUrl();
			String teiidConnected = teiidInstance.isConnected() ? Messages.getString(Messages.ShowCommand.Connected) : Messages.getString(Messages.ShowCommand.NotConnected);

			String teiidJdbcUrl = teiidInstance.getTeiidJdbcInfo().getUrl();
			/* Only test jdbc connection if teiid instance has been connected to */
			String teiidJdbcConnected = teiidConnected;
			if (teiidInstance.isConnected()) {
				teiidJdbcConnected = teiidInstance.ping(ConnectivityType.JDBC).isOK() ? Messages.getString(Messages.ShowCommand.PingOk) : Messages.getString(Messages.ShowCommand.PingFail);
			}

			String currentTeiidInst = OPEN_SQUARE_BRACKET + teiidUrl + " : " + teiidConnected + CLOSE_SQUARE_BRACKET; //$NON-NLS-1$
			String currentTeiidJdbc = OPEN_SQUARE_BRACKET + teiidJdbcUrl + " : " + teiidJdbcConnected + CLOSE_SQUARE_BRACKET; //$NON-NLS-1$
			print(MESSAGE_INDENT, Messages.getString("ShowCommand.CurrentTeiid", currentTeiidInst)); //$NON-NLS-1$
			print(MESSAGE_INDENT, Messages.getString("ShowCommand.CurrentTeiidJdbc", currentTeiidJdbc)); //$NON-NLS-1$
		}

		// Current Context
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		print(MESSAGE_INDENT, Messages.getString("ShowCommand.CurrentContext", currentContext.getFullName())); //$NON-NLS-1$
	}

    private void printChildren( final WorkspaceContext context ) throws Exception {
        final List< WorkspaceContext > children = context.getChildren();

        if ( children.isEmpty() ) {
            String noChildrenMsg = Messages.getString( "ShowCommand.noChildrenMsg", context.getType(), context.getFullName() ); //$NON-NLS-1$
            print( MESSAGE_INDENT, noChildrenMsg );
            return;
        }

        int maxWidth = DEFAULT_WIDTH;

        // loop through children getting name, type, and finding widest child name
        for ( int i = 0, size = children.size(); i < size; ++i ) {
            final String name = children.get( i ).getName();

            if ( maxWidth < name.length() ) {
                maxWidth = name.length();
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
        final String childrenHeader = Messages.getString( "ShowCommand.ChildrenHeader", context.getType(), context.getFullName() ); //$NON-NLS-1$
        print( MESSAGE_INDENT, childrenHeader );

        final String format = getFormat( maxWidth );
        final String headerDelimiter = getHeaderDelimiter( maxWidth );
        print( MESSAGE_INDENT,
               String.format( format, Messages.getString( SHELL.CHILD_NAME_HEADER ), Messages.getString( SHELL.CHILD_TYPE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, headerDelimiter, headerDelimiter ) );

        // Print each child
        for ( final WorkspaceContext childContext : children ) {
            final String childName = childContext.getName();
            final String childType = childContext.getType();
            print( MESSAGE_INDENT, String.format( format, childName, childType ) );
        }
    }

	/**
	 * Shows the komodo shell global properties
	 * @throws Exception
	 */
    private void printGlobal() throws Exception {
        final Properties globalProps = getWorkspaceStatus().getProperties(); // All properties

        // make sure the standard global properties are shown
        for ( final String prop : WorkspaceStatus.GLOBAL_PROP_KEYS ) {
            if ( !globalProps.containsKey( prop ) ) {
                globalProps.put( prop, Messages.getString( SHELL.NO_PROPERTY_VALUE ) );
            }
        }

        final Map< String, String > sorted = new TreeMap<>();
        int maxWidth = DEFAULT_WIDTH;

        for ( Entry< ?, ? > entry : globalProps.entrySet() ) {
            final String propName = ( String )entry.getKey();

            if ( maxWidth < propName.length() ) {
                maxWidth = propName.length();
            }

            sorted.put( propName, ( String )entry.getValue() );
        }

        // Print properties header
        final String globalPropsHeader = Messages.getString( "ShowCommand.GlobalPropertiesHeader" ); //$NON-NLS-1$
        print( MESSAGE_INDENT, globalPropsHeader );

        final String format = getFormat( maxWidth );
        final String headerDelimiter = getHeaderDelimiter( maxWidth );
        print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, headerDelimiter, headerDelimiter ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
            print( MESSAGE_INDENT, String.format( format, entry.getKey(), entry.getValue() ) );
        }
    }

	/**
	 * Shows the komodo object property with the specified name
	 * @throws Exception
	 */
    private void printProperty( WorkspaceContext context,
                                String name ) throws Exception {
        final String propertyName = attachPrefix( context, name );

        // Get the value for the supplied property
        String propValue = context.getPropertyValue( propertyName );
        int maxWidth = Math.max( DEFAULT_WIDTH, propValue.length() );
        final String format = getFormat( maxWidth );
        final String headerDelimiter = getHeaderDelimiter( maxWidth );

        // Print properties header
        String propListHeader = Messages.getString( "ShowCommand.PropertyHeader", context.getType(), context.getFullName() ); //$NON-NLS-1$
        print( MESSAGE_INDENT, propListHeader );
        print( MESSAGE_INDENT,
               String.format( format,
                              Messages.getString( SHELL.PROPERTY_NAME_HEADER ),
                              Messages.getString( SHELL.PROPERTY_VALUE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, headerDelimiter, headerDelimiter ) );

        print( MESSAGE_INDENT, String.format( format, name, propValue ) );
        print();
    }

	/**
	 * Print a summary of the specified komodo object
	 * @throws Exception
	 */
	private void printSummary(WorkspaceContext context) throws Exception {
        printProperties(context);
        printChildren(context);
	}

	/**
	 * Records the command if recording status is 'on'
	 */
	private void recordCommand() {
        if (getWorkspaceStatus().getRecordingStatus()) recordCommand(getArguments());
	}

	protected boolean validate(Arguments allArgs) throws Exception {
		// Validate the sub-command
		String subCmd = allArgs.get(0).trim().toLowerCase();
		if(!validateSubCmd(subCmd)) {
			return false;
		}

		// SubCommand ok, continue with second arg for commands with second args
		if(allArgs.size()>1) {
			// For "properties", "children" and "summary" commands - a second argument (path) is optional.
			if(subCmd.equals(SUBCMD_PROPERTIES) || subCmd.equals(SUBCMD_CHILDREN) || subCmd.equals(SUBCMD_SUMMARY)) {
				// Optional path arg
		        String secondArg = optionalArgument(1);
				if(!validatePath(secondArg)) {
					return false;
				}
			// For "property", the second arg (propName) is required.
			} else if(subCmd.equals(SUBCMD_PROPERTY)) {
				// Second required arg is the property name.  Verify that it is valid for the current object
		        String propName = requiredArgument(1, Messages.getString("ShowCommand.InvalidArgMsg_PropertyName")); //$NON-NLS-1$
				WorkspaceContext context = getContext();
				if(!validatePropertyName(context, propName)) {
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Validate the sub-command
	 * @param subCmd
	 */
	private boolean validateSubCmd(String subCmd) {
		// Validate the sub-command
		if (subCmd.length()==0) {
			print(MESSAGE_INDENT,Messages.getString("ShowCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
			return false;
		}
		if(!SUBCMDS.contains(subCmd)) {
			print(MESSAGE_INDENT,Messages.getString("ShowCommand.InvalidSubCommand")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        // TODO needs to work with properties without their prefix

    	if (getArguments().isEmpty()) {
    		// --------------------------------------------------------------
    		// No arg - offer subcommands
    		// --------------------------------------------------------------
    		if(lastArgument==null) {
    			candidates.addAll(SUBCMDS);
    			// --------------------------------------------------------------
    			// One arg - determine the completion options for it.
    			// --------------------------------------------------------------
    		} else {
    			for (String item : SUBCMDS) {
    				if (item.toUpperCase().startsWith(lastArgument.toUpperCase())) {
    					candidates.add(item);
    				}
    			}
    		}
    		return 0;
    	} else if (getArguments().size()==1) {
    		String cmdArgLower = getArguments().get(0).toLowerCase();
    		// Tab completion for "properties", "children", "summary" - expects a path arg
    		if(SUBCMD_PROPERTIES.equals(cmdArgLower) || SUBCMD_CHILDREN.equals(cmdArgLower) || SUBCMD_SUMMARY.equals(cmdArgLower)) {
    			// The arg is expected to be a path
    			updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

    			// Do not put space after it - may want to append more to the path
    			return CompletionConstants.NO_APPEND_SEPARATOR;
    			// Tab completion for "property" - expects a valid property for the current context.
    		} else if(SUBCMD_PROPERTY.equals(cmdArgLower)) {
    		    updateTabCompleteCandidatesForProperty(candidates, getContext(), lastArgument);

    			return 0;
            }
    	}
    	return -1;
    }

}
