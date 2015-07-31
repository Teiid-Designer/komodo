/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import static org.komodo.relational.Messages.Relational.INVALID_TABLE_REF_COLUMN;
import static org.komodo.relational.Messages.Relational.INVALID_TABLE_REF_PATH;
import static org.komodo.relational.Messages.Relational.TABLE_REF_NOT_SET;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.shell.CompletionConstants.NO_APPEND_SEPARATOR;
import static org.komodo.shell.Messages.SetCommand.ADD_TABLE_CONSTRAINT_COLUMN_FAILED;
import static org.komodo.shell.Messages.SetCommand.COLUMN_PATH_NOT_FOUND;
import static org.komodo.shell.Messages.SetCommand.INVALID_TABLE_REF_COLUMN_PATH;
import static org.komodo.shell.Messages.SetCommand.TABLE_COLUMNS_CANNOT_BE_SET;
import static org.komodo.shell.Messages.SetCommand.TABLE_COLUMNS_SET;
import static org.komodo.shell.Messages.SetCommand.TABLE_PATH_NOT_FOUND;
import static org.komodo.shell.Messages.SetCommand.TABLE_REF_CANNOT_BE_SET;
import static org.komodo.shell.Messages.SetCommand.TABLE_REF_COLUMNS_SET;
import static org.komodo.shell.Messages.SetCommand.TABLE_REF_REFS_CANNOT_BE_SET;
import static org.komodo.shell.Messages.SetCommand.TABLE_REF_SET;
import static org.komodo.shell.Messages.SetCommand.TOO_MANY_ARGS;
import static org.komodo.shell.Messages.SetCommand.UNSET_TABLE_CONSTRAINT_COLUMN_FAILED;
import static org.komodo.shell.Messages.SetCommand.UNSET_TABLE_REF_COLUMN_FAILED;
import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

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
                if ( isAutoCommit() ) {
                    getWorkspaceStatus().commit( SetCommand.class.getSimpleName() );
                }

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

    private void provideCandidates( final WorkspaceContext context,
                                    final String name,
                                    final String lastArgument,
                                    final List< CharSequence > candidates ) throws Exception {
        final WorkspaceStatus wsStatus = getWorkspaceStatus();
        final Repository.UnitOfWork uow = wsStatus.getTransaction();
        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );
        final PropertyDescriptor descriptor = context.getKomodoObj().getPropertyDescriptor( uow, propertyName );

        if ( descriptor == null ) {
            return;
        }

        String[] possibleValues = StringConstants.EMPTY_ARRAY;

        if ( Type.BOOLEAN == descriptor.getType() ) {
            possibleValues = new String[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() };
        } else if ( Constraint.REFERENCES.equals( propertyName ) ) {
            // show columns of parent table
            final KomodoObject parent = context.getKomodoObj().getParent( uow );

            if ( parent instanceof Table ) {
                final Column[] columns = ( ( Table )parent ).getColumns( uow );

                if ( columns.length != 0 ) {
                    possibleValues = new String[ columns.length ];
                    int i = 0;

                    for ( final Column column : columns ) {
                        possibleValues[ i++ ] = ContextUtils.convertPathToDisplayPath( column.getAbsolutePath() );
                    }
                }
            }
        } else {
            final KomodoObject kobject = context.getKomodoObj();

            if ( kobject instanceof ForeignKey ) {
                // show columns of referenced table
                if ( Constraint.TABLE_REFERENCE_REFERENCES.equals( propertyName ) ) {
                    final Table refTable = ( ( ForeignKey )kobject ).getReferencesTable( uow );

                    // if no table reference than cannot provide columns
                    if ( refTable != null ) {
                        // provide the paths of the table reference columns
                        final Column[] columns = refTable.getColumns( uow );
                        possibleValues = new String[ columns.length ];
                        int i = 0;

                        for ( final Column column : columns ) {
                            possibleValues[ i++ ] = ContextUtils.convertPathToDisplayPath( column.getAbsolutePath() );
                        }
                    }
                } else if ( Constraint.TABLE_REFERENCE.equals( propertyName ) ) {
                    // provide paths for all tables except the parent table
                    final FindCommand findCmd = new FindCommand( getWorkspaceStatus() );
                    findCmd.setOutput( getWriter() );

                    try {
                        findCmd.setArguments( new Arguments( StringConstants.EMPTY_STRING ) );
                    } catch ( final Exception e ) {
                        // only occurs on parsing error of arguments and we have no arguments
                    }

                    final String[] tablePaths = findCmd.query( KomodoType.TABLE, null, null );

                    if ( tablePaths.length != 0 ) {
                        // do not include the parent table of the foreign key
                        final String currentTablePath = ContextUtils.convertPathToDisplayPath( kobject.getParent( uow ).getAbsolutePath() );
                        possibleValues = new String[ tablePaths.length - 1 ];
                        int i = 0;
                        boolean found = false;

                        for ( final String path : tablePaths ) {
                            final String displayPath = ContextUtils.convertPathToDisplayPath( path );

                            if ( found ) {
                                possibleValues[ i++ ] = displayPath;
                            } else if ( currentTablePath.equals( displayPath ) ) {
                                found = true;
                            }
                        }
                    }
                }
            } else {
                final Object[] defaultValues = descriptor.getDefaultValues();

                if ( defaultValues.length != 0 ) {
                    possibleValues = new String[ defaultValues.length ];
                    int i = 0;

                    for ( final Object defaultValue : defaultValues ) {
                        possibleValues[ i++ ] = defaultValue.toString();
                    }
                }
            }
        }

        if ( possibleValues.length != 0 ) {
            final boolean hasLastArgument = !StringUtils.isBlank( lastArgument );

            for ( final String value : possibleValues ) {
                if ( ( hasLastArgument && value.startsWith( lastArgument ) ) || !hasLastArgument ) {
                    candidates.add( value );
                }
            }
        }
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
        final Repository.UnitOfWork uow = getWorkspaceStatus().getTransaction();
        final StringBuilder result = new StringBuilder();
        boolean quoted = false;
        boolean firstTime = true;

        for ( final Object value : property.getValues( getWorkspaceStatus().getTransaction() ) ) {
            if ( !firstTime ) {
                result.append( ',' );
            } else {
                firstTime = false;
            }

            final Type type = property.getDescriptor( uow ).getType();
            final boolean propIsReference = ( ( Type.REFERENCE == type ) || ( Type.WEAKREFERENCE == type ) );
            String valueAsText = null;

            if ( propIsReference ) {
                final String path = RepositoryTools.findPathOfReference( uow, property.getRepository(), value.toString() );
                valueAsText = ( StringUtils.isBlank( path ) ? value.toString() : ContextUtils.convertPathToDisplayPath( path ) );
            } else {
                valueAsText = value.toString();

                if ( ( valueAsText.indexOf( ' ' ) != -1 ) && !quoted ) {
                    quoted = true;
                    result.insert( 0, '"' );
                }
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

        return multiValues.split( "," ); //$NON-NLS-1$
    }

    private void setForeignKeyTableReference( final String tableRefPath ) throws Exception {
        // make sure current object is a foreign key
        final KomodoObject kobject = getContext().getKomodoObj();

        if ( !( kobject instanceof ForeignKey ) ) {
            throw new Exception( Messages.getString( TABLE_REF_CANNOT_BE_SET,
                                                     ContextUtils.convertPathToDisplayPath( kobject.getAbsolutePath() ) ) );
        }

        final ForeignKey foreignKey = ( ForeignKey )kobject;

        // get reference of the table at the specified path
        final WorkspaceContext tableContext = ContextUtils.getContextForPath( getWorkspaceStatus(), tableRefPath );

        if ( tableContext == null ) {
            throw new Exception( Messages.getString( TABLE_PATH_NOT_FOUND, tableRefPath ) );
        }

        final KomodoObject reference = tableContext.getKomodoObj();
        final Repository.UnitOfWork transaction = getWorkspaceStatus().getTransaction();

        if ( reference instanceof Table ) {
            if ( foreignKey.getParent( transaction ).equals( reference ) ) {
                throw new KException( Messages.getString( INVALID_TABLE_REF_PATH,
                                                          ( ( Table )reference ).getAbsolutePath(),
                                                          ( ( Table )reference ).getTypeIdentifier( transaction ).getType() ) );
            }

            foreignKey.setReferencesTable( transaction, ( Table )reference );
        } else {
            throw new KException( Messages.getString( INVALID_TABLE_REF_PATH,
                                                      reference.getAbsolutePath(),
                                                      reference.getTypeIdentifier( transaction ).getType() ) );
        }

        print( MESSAGE_INDENT, Messages.getString( TABLE_REF_SET, tableRefPath ) );
    }

    private void setForeignKeyTableReferenceColumns( final String propValue ) throws Exception {
        final KomodoObject kobject = getContext().getKomodoObj();

        if ( !( kobject instanceof ForeignKey ) ) {
            throw new Exception( Messages.getString( TABLE_REF_REFS_CANNOT_BE_SET,
                                                     ContextUtils.convertPathToDisplayPath( kobject.getAbsolutePath() ) ) );
        }

        final ForeignKey foreignKey = ( ForeignKey )kobject;

        { // first clear value if necessary
            if ( getContext().getPropertyValue( Constraint.TABLE_REFERENCE_REFERENCES ) != null ) {
                final UnsetPropertyCommand cmd = new UnsetPropertyCommand( getWorkspaceStatus() );
                cmd.setOutput( getWriter() );
                cmd.setArguments( new Arguments( Constraint.TABLE_REFERENCE_REFERENCES ) );
                cmd.setAutoCommit( false );

                if ( !cmd.execute() ) {
                    throw new Exception( Messages.getString( UNSET_TABLE_REF_COLUMN_FAILED, cmd ) );
                }
            }
        }

        final Repository.UnitOfWork transaction = getWorkspaceStatus().getTransaction();
        final Table referencedTable = foreignKey.getReferencesTable( transaction );

        // must have a referenced table
        if ( referencedTable == null ) {
            throw new KException( Messages.getString( TABLE_REF_NOT_SET, foreignKey.getName( transaction ) ) );
        }

        // set new value
        final String[] columnPaths = parseMultiValues( propValue );
        final Object[] columnIds = new String[ columnPaths.length ];
        int i = 0;

        for ( final String columnPath : columnPaths ) {
            // get column being referenced
            final WorkspaceContext columnContext = ContextUtils.getContextForPath( getWorkspaceStatus(), columnPath );

            if ( columnContext == null ) {
                throw new Exception( Messages.getString( COLUMN_PATH_NOT_FOUND, columnContext ) );
            }

            final KomodoObject referencedColumn = columnContext.getKomodoObj();

            if ( referencedColumn instanceof Column ) {
                if ( referencedColumn.getParent( transaction ).equals( referencedTable ) ) {
                    final String columnId = referencedColumn.getRawProperty( transaction,
                                                                             JcrLexicon.UUID.getString() ).getStringValue( transaction );
                    columnIds[ i++ ] = columnId;
                } else {
                    throw new KException( Messages.getString( INVALID_TABLE_REF_COLUMN, referencedColumn.getAbsolutePath() ) );
                }
            } else {
                throw new Exception( Messages.getString( INVALID_TABLE_REF_COLUMN_PATH, columnPath ) );
            }
        }

        foreignKey.setProperty( getWorkspaceStatus().getTransaction(), Constraint.TABLE_REFERENCE_REFERENCES, columnIds );
        print( MESSAGE_INDENT, Messages.getString( TABLE_REF_COLUMNS_SET, propValue ) );
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

    private void setProperty( final WorkspaceContext context,
                              final String name,
                              final String propValue ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : attachPrefix( context, name );

        if ( !StringUtils.isBlank( propValue ) && isMultiValuedProperty( context, propertyName ) ) {
            if ( Constraint.REFERENCES.equals( propertyName ) ) {
                setTableConstraintColumns( propValue );
            } else if ( Constraint.TABLE_REFERENCE_REFERENCES.equals( propertyName ) ) {
                setForeignKeyTableReferenceColumns( propValue );
            } else {
                final String[] values = parseMultiValues( propValue );
                context.getKomodoObj().setProperty( getWorkspaceStatus().getTransaction(), propertyName, ( Object[] )values );
            }
        } else {
            if ( Constraint.TABLE_REFERENCE.equals( propertyName ) ) {
                setForeignKeyTableReference( propValue );
            } else {
                context.setPropertyValue( propertyName, propValue );
            }
        }
    }

    private void setTableConstraintColumns( final String propValue ) throws Exception {
        final KomodoObject kobject = getContext().getKomodoObj();

        if ( !( kobject instanceof TableConstraint ) ) {
            throw new Exception( Messages.getString( TABLE_COLUMNS_CANNOT_BE_SET,
                                                     ContextUtils.convertPathToDisplayPath( kobject.getAbsolutePath() ) ) );
        }

        // first clear value if necessary
        if ( getContext().getPropertyValue( Constraint.REFERENCES ) != null ) {
            final UnsetPropertyCommand cmd = new UnsetPropertyCommand( getWorkspaceStatus() );
            cmd.setOutput( getWriter() );
            cmd.setArguments( new Arguments( Constraint.REFERENCES ) );
            cmd.setAutoCommit( false );

            if ( !cmd.execute() ) {
                throw new Exception( Messages.getString( UNSET_TABLE_CONSTRAINT_COLUMN_FAILED, cmd ) );
            }
        }

        // set new value by adding in one at a time using the AddConstraintColumnCommand
        for ( final String columnPath : parseMultiValues( propValue ) ) {
            final AddConstraintColumnCommand cmd = new AddConstraintColumnCommand( getWorkspaceStatus() );
            cmd.setOutput( getWriter() );
            cmd.setArguments( new Arguments( columnPath ) );
            cmd.setAutoCommit( false );

            if ( !cmd.execute() ) {
                throw new Exception( Messages.getString( ADD_TABLE_CONSTRAINT_COLUMN_FAILED, cmd ) );
            }
        }

        print( MESSAGE_INDENT, Messages.getString( TABLE_COLUMNS_SET, propValue ) );
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
                final String propName = isShowingPropertyNamePrefixes() ? getArguments().get( 1 )
                                                                        : attachPrefix( context, getArguments().get( 1 ) );

                if ( isMultiValuedProperty( context, propName ) ) {
                    if ( context.getKomodoObj().hasProperty( getWorkspaceStatus().getTransaction(), propName ) ) {
                        // concat current multi-values as a string
                        final String value = concatMultiValues( context, propName );
                        candidates.add( value );
                    } else if ( Constraint.REFERENCES.equals( propName )
                                || Constraint.TABLE_REFERENCE_REFERENCES.equals( propName ) ) {
                        provideCandidates( context, propName, lastArgument, candidates );
                    }
                } else {
                    provideCandidates( context, propName, lastArgument, candidates );
                }

                return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
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
