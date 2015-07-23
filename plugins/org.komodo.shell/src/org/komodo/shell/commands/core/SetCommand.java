/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.shell.CompletionConstants.NO_APPEND_SEPARATOR;
import static org.komodo.shell.Messages.SetCommand.TOO_MANY_ARGS;

import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.utils.StringUtils;

/**
 * A command to set a property value, a global property value, or to turn on recording.
 */
public class SetCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set"; //$NON-NLS-1$

    private static final String SUBCMD_PROPERTY = "property"; //$NON-NLS-1$
    private static final String SUBCMD_GLOBAL = "global"; //$NON-NLS-1$
    private static final String SUBCMD_RECORD = "record"; //$NON-NLS-1$
    private static final List<String> SUBCMDS = Arrays.asList(SUBCMD_PROPERTY, SUBCMD_GLOBAL, SUBCMD_RECORD);

    private static final String ON = "on"; //$NON-NLS-1$
	private static final String OFF = "off"; //$NON-NLS-1$
    private static final List<String> RECORD_CMDS = Arrays.asList(ON, OFF);

    private static final String RESET_GLOBAL = "--reset"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String subcmdArg = requiredArgument(0, Messages.getString("SetCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$

        try {
        	// Set property
        	if (SUBCMD_PROPERTY.equalsIgnoreCase(subcmdArg)) {
        		// property name and value are required
        		String propNameArg = requiredArgument(1, Messages.getString("SetCommand.InvalidArgMsg_PropertyName")); //$NON-NLS-1$
        		String propValueArg = requiredArgument(2, Messages.getString("SetCommand.InvalidArgMsg_PropertyValue")); //$NON-NLS-1$
        		// path is optional.  if path is not included, current context is assumed.
        		String pathArg = optionalArgument(3);

                // check for too many args
                if ( getArguments().size() > 4 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, subcmdArg ) );
                }
                
                // Validates SET PROPERTY args
                if (!validateSetProperty(propNameArg,propValueArg,pathArg)) {
        			return false;
        		}

                final WorkspaceContext context = StringUtils.isEmpty( pathArg ) ? getContext()
                                                                               : ContextUtils.getContextForPath( getWorkspaceStatus(),
                                                                                                                 pathArg );

        		// Set the property
        		setProperty(context,propNameArg, propValueArg);

                // Commit transaction
                getWorkspaceStatus().commit("SetCommand"); //$NON-NLS-1$

                // Print message
        		print(MESSAGE_INDENT, Messages.getString("SetCommand.PropertySet", propNameArg)); //$NON-NLS-1$
        	} else if (SUBCMD_GLOBAL.equalsIgnoreCase(subcmdArg)) {
        		// property name and value are required
        		String propNameArg = requiredArgument(1, Messages.getString("SetCommand.InvalidArgMsg_GlobalPropertyName")); //$NON-NLS-1$
        		String propValueArg = null;

                if ( RESET_GLOBAL.equals( propNameArg.toLowerCase() ) ) {
                    getWorkspaceStatus().resetProperties();
                    print( MESSAGE_INDENT, Messages.getString( "SetCommand.resetGlobalProperties" ) ); //$NON-NLS-1$
                } else {
                    propValueArg = requiredArgument( 2, Messages.getString( "SetCommand.InvalidArgMsg_PropertyValue" ) ); //$NON-NLS-1$

                    // validate global property name and value
                    final String errorMsg = getWorkspaceStatus().validateGlobalPropertyValue( propNameArg, propValueArg );

                    if ( !StringUtils.isEmpty( errorMsg ) ) {
                        print( MESSAGE_INDENT, Messages.getString( "SetCommand.InvalidGlobalProperty", errorMsg ) ); //$NON-NLS-1$
                        return false;
                    }

                    // Set the property
                    setGlobalProperty( propNameArg, propValueArg );
                    print( MESSAGE_INDENT, Messages.getString( "SetCommand.GlobalPropertySet", propNameArg ) ); //$NON-NLS-1$
                }
        	} else if (SUBCMD_RECORD.equalsIgnoreCase(subcmdArg)) {
        		String onOffArg = requiredArgument(1, Messages.getString("SetCommand.onOffArg_empty")); //$NON-NLS-1$

        		if (!this.validateRecord(onOffArg)) {
        			return false;
        		}

        		WorkspaceStatus wsStatus = getWorkspaceStatus();
        		if(onOffArg.equalsIgnoreCase(ON)) {
        			wsStatus.setRecordingStatus(true);
        		} else if(onOffArg.equalsIgnoreCase(OFF)) {
        			wsStatus.setRecordingStatus(false);
        		}

        		Date d = new Date();
        		String rState = wsStatus.getRecordingStatus() ? ON : OFF;
        		String rFile = wsStatus.getRecordingOutputFile().getCanonicalPath();
        		String stateChangedMsg = Messages.getString("SetCommand.setRecordingStateMsg",rState,d.toString(),rFile); //$NON-NLS-1$

                print(MESSAGE_INDENT,stateChangedMsg);

                recordComment("====== "+stateChangedMsg+" ======"); //$NON-NLS-1$ //$NON-NLS-2$

                return true;
        	} else {
        		throw new InvalidCommandArgumentException(0, Messages.getString("SetCommand.InvalidSubCommand")); //$NON-NLS-1$
        	}
        } catch (InvalidCommandArgumentException e) {
            throw e;
        } catch (Exception e) {
            print( MESSAGE_INDENT, Messages.getString( "SetCommand.Failure", e.getLocalizedMessage() ) ); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#validatePropertyValue(java.lang.String, java.lang.String,
     *      org.komodo.shell.api.WorkspaceContext)
     */
    @Override
    public boolean validatePropertyValue( final String propName,
                                          final String propValue,
                                          final WorkspaceContext context ) {
        if ( !StringUtils.isBlank( propValue ) ) {
            try {
                // make sure multi-valued property value parses
                if ( isMultiValuedProperty( context, propName ) ) {
                    final String[] multiValues = parseMultiValues( propValue );
                    return ( multiValues.length != 0 );
                }
            } catch ( final Exception e ) {
                return false;
            }
        }

        // let super validate
        return super.validatePropertyValue( propName, propValue, context );
    }

    /**
     * Validate the SET PROPERTY args
     * @param propName the property name
     * @param propValue the property value
     * @param contextPath the optional context path
     * @return 'true' if valid, 'false' if not.
     * @throws Exception
     */
	protected boolean validateSetProperty(String propName, String propValue, String contextPath) throws Exception {
		// Validate the path first if supplied
		if(!StringUtils.isEmpty(contextPath)) {
			if (!validatePath(contextPath)) {
				return false;
			}
		}
		
		// Get the context for object.  otherwise use current context
        final WorkspaceContext context = StringUtils.isEmpty( contextPath ) ? getContext()
                                                                           : ContextUtils.getContextForPath( getWorkspaceStatus(),
                                                                                                             contextPath );

		// Validate the type is valid for the context
		if (!validateProperty(propName,context,true)) {
			return false;
		}

		// Validate the property value
		if (!validatePropertyValue(propName,propValue,context)) {
			return false;
		}

		return true;
	}

    /**
     * Validate the SET RECORD args
     * @param onOffArg the on / off arg
     * @return 'true' if valid, 'false' if not.
     */
	protected boolean validateRecord(String onOffArg) {
		// Check for empty arg
		if(StringUtils.isEmpty(onOffArg)) {
            print(MESSAGE_INDENT,Messages.getString("SetCommand.onOffArg_empty")); //$NON-NLS-1$
			return false;
		}

		// Check for invalid arg
		if(!onOffArg.equalsIgnoreCase(ON) && !onOffArg.equalsIgnoreCase(OFF)) {
            print(MESSAGE_INDENT,Messages.getString("SetCommand.onOffArg_invalid")); //$NON-NLS-1$
			return false;
		}

		// If verify that global file var was set.
		String recordingFileStr = getWorkspaceStatus().getProperties().getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
		if(StringUtils.isEmpty(recordingFileStr)) {
			print(MESSAGE_INDENT,Messages.getString("SetCommand.recordingFileNotSet")); //$NON-NLS-1$
			return false;
		} else {
			File recordingFile = getWorkspaceStatus().getRecordingOutputFile();
			if(recordingFile!=null && recordingFile.exists()) {
				if(!recordingFile.canWrite()) {
					print(MESSAGE_INDENT,Messages.getString("SetCommand.recordingFileNotWriteable",recordingFile)); //$NON-NLS-1$
					return false;
				}
			}
		}

		return true;
	}

    private Object[] getPossibleValues( final WorkspaceContext context,
                                        final String name ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );
        final PropertyDescriptor descriptor = context.getKomodoObj().getPropertyDescriptor( getWorkspaceStatus().getTransaction(),
                                                                                            propertyName );
        if ( descriptor == null ) {
            return StringConstants.EMPTY_ARRAY;
        }

        if ( Type.BOOLEAN == descriptor.getType() ) {
            return new String[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() };
        }

        return descriptor.getDefaultValues();
    }

    private boolean isMultiValuedProperty( final WorkspaceContext context,
                                           final String name ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );
        final PropertyDescriptor descriptor = context.getKomodoObj().getPropertyDescriptor( getWorkspaceStatus().getTransaction(),
                                                                                            propertyName );
        return ( ( descriptor == null ) ? false : descriptor.isMultiple() );
    }

    private String concatMultiValues( final WorkspaceContext context,
                                      final String name ) throws Exception {
        // TODO need to account for escaped values
        assert isMultiValuedProperty( context, name );

        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );
        final Property property = context.getKomodoObj().getProperty( getWorkspaceStatus().getTransaction(), propertyName );
        final StringBuilder result = new StringBuilder();
        boolean quoted = false;
        boolean firstTime = true;

        for ( final Object value : property.getValues( getWorkspaceStatus().getTransaction() ) ) {
            if ( !firstTime ) {
                result.append( ',' );
            } else {
                firstTime = false;
            }

            final String valueAsText = value.toString();

            if ( ( valueAsText.indexOf( ' ' ) != -1 ) && !quoted ) {
                quoted = true;
                result.insert( 0, '"' );
            }

            result.append( valueAsText );
        }

        if ( quoted ) {
            result.append( '"' );
        }

        return result.toString();
    }

    private String[] parseMultiValues( final String valuesString ) {
        // TODO need to account for escaped values
        assert !StringUtils.isBlank( valuesString );
        String multiValues = null;

        // strip off leading and trailing quotes if necessary
        if ( valuesString.startsWith( "\"" ) && valuesString.endsWith( "\"" ) ) { //$NON-NLS-1$ //$NON-NLS-2$
            if ( valuesString.length() == 2 ) {
                return StringConstants.EMPTY_ARRAY;
            }

            multiValues = valuesString.substring( 1, valuesString.length() - 1 );
        } else {
            multiValues = valuesString;
        }

        final StringTokenizer parser = new StringTokenizer( multiValues, "," ); //$NON-NLS-1$
        final String[] result = new String[ parser.countTokens() ];
        int i = 0;

        while ( parser.hasMoreTokens() ) {
            final String value = parser.nextToken();
            result[i++] = value;
        }

        return result;
    }

    private void setProperty( final WorkspaceContext context,
                              final String name,
                              final String propValue ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );

        if ( !StringUtils.isBlank( propValue ) && isMultiValuedProperty( context, propertyName ) ) {
            final String[] values = parseMultiValues( propValue );
            context.getKomodoObj().setProperty( getWorkspaceStatus().getTransaction(), propertyName, ( Object[] )values );
        } else {
            context.setPropertyValue( propertyName, propValue );
        }
    }

    /**
     * Sets a global workspace property
     * @param propName the global property name
     * @param propValue the property value
     * @throws Exception the exception
     */
    private void setGlobalProperty(String propName, String propValue) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        wsStatus.setProperty(propName, propValue);
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
    	if (getArguments().isEmpty()) {
    		// SubCommand completion options
    		if(lastArgument==null) {
    			candidates.addAll(SUBCMDS);
    		} else {
    			for (String subCmd : SUBCMDS) {
    				if (subCmd.toUpperCase().startsWith(lastArgument.toUpperCase())) {
    					candidates.add(subCmd);
    				}
    			}
    		}
    		return 0;
    	} else if (getArguments().size()==1) {
    		// Record subcommand options
    		if(getArguments().get(0).toLowerCase().equals(SUBCMD_RECORD)) {
    			if(lastArgument==null) {
    				candidates.addAll(RECORD_CMDS);
    			} else {
    				for (String cmdName : RECORD_CMDS) {
    					if (cmdName.startsWith(lastArgument.toLowerCase())) {
    						candidates.add(cmdName);
    					}
    				}
    			}
    		// Set property and global options
            } else {
                final String subCmd = getArguments().get( 0 ).toLowerCase();

                if ( subCmd.equals( SUBCMD_PROPERTY ) ) {
                    updateTabCompleteCandidatesForProperty( candidates, getContext(), lastArgument );
                } else if ( subCmd.equals( SUBCMD_GLOBAL ) ) {
                    // Global property completion options
                    final Set< String > potentials = WorkspaceStatus.GLOBAL_PROPS.keySet();

                    if ( lastArgument == null ) {
                        candidates.addAll( potentials );
                        candidates.add( RESET_GLOBAL );
                    } else {
                        for ( final String name : potentials ) {
                            if ( name.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                                candidates.add( name );
                            }
                        }

                        if ( RESET_GLOBAL.startsWith( lastArgument.toLowerCase() ) ) {
                            candidates.add( RESET_GLOBAL );
                        }
                    }
                } else {
                    return -1; // invalid subcmd
                }
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        } else if ( getArguments().size() == 2 ) {
            if ( getArguments().get( 0 ).toLowerCase().equals( SUBCMD_RECORD ) ) return 0;

            if ( getArguments().get( 0 ).toLowerCase().equals( SUBCMD_PROPERTY ) ) {
                final WorkspaceContext context = getContext();

                final String propName = getArguments().get( 1 );

                if ( isMultiValuedProperty( context, propName ) ) {
                    // concat current multi-values as a string
                    final String value = concatMultiValues( context, propName );
                    candidates.add( value );
                } else {
                    final Object[] possibleValues = getPossibleValues( context, propName );

                    if ( possibleValues.length != 0 ) {
                        final boolean hasLastArgument = !StringUtils.isBlank( lastArgument );

                        for ( final Object value : possibleValues ) {
                            if ( ( hasLastArgument && value.toString().startsWith( lastArgument ) ) || !hasLastArgument ) {
                                candidates.add( value.toString() );
                            }
                        }
                    } else if ( lastArgument == null ) {
                        candidates.add( "propertyValue" ); //$NON-NLS-1$
                    }
                }

                return 0;
            }

            if ( getArguments().get( 0 ).toLowerCase().equals( SUBCMD_GLOBAL ) ) {
                final String propName = getArguments().get( 1 );

                if ( getWorkspaceStatus().isBooleanProperty( propName ) ) {
                    if ( StringUtils.isBlank( lastArgument ) ) {
                        candidates.add( Boolean.TRUE.toString() );
                        candidates.add( Boolean.FALSE.toString() );
                    } else if ( Boolean.TRUE.toString().startsWith( lastArgument ) ) {
                        candidates.add( Boolean.TRUE.toString() );
                    } else if ( Boolean.FALSE.toString().startsWith( lastArgument ) ) {
                        candidates.add( Boolean.FALSE.toString() );
                    }
                }

                return 0;
            }
        } else if (getArguments().size()==3) {
    		if(getArguments().get(0).toLowerCase().equals(SUBCMD_RECORD)) return 0;

    		// The arg is expected to be a path
    		updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

    		// Do not put space after it - may want to append more to the path
    		return NO_APPEND_SEPARATOR;
    	}

    	return -1;
    }

}
