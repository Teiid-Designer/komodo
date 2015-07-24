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
import static org.komodo.shell.Messages.CreateCommand.DEFAULT_VDB_FILE_PATH;
import static org.komodo.shell.Messages.CreateCommand.FAILURE;
import static org.komodo.shell.Messages.CreateCommand.MISSING_ENTRY_PATH;
import static org.komodo.shell.Messages.CreateCommand.MISSING_FOREIGN_KEY_TABLE_REF;
import static org.komodo.shell.Messages.CreateCommand.MISSING_OBJ_NAME;
import static org.komodo.shell.Messages.CreateCommand.MISSING_OBJ_TYPE;
import static org.komodo.shell.Messages.CreateCommand.MISSING_OPTION_VALUE;
import static org.komodo.shell.Messages.CreateCommand.MISSING_PROPERTY_NAME;
import static org.komodo.shell.Messages.CreateCommand.MISSING_PROPERTY_VALUE;
import static org.komodo.shell.Messages.CreateCommand.MISSING_TRANSLATOR_TYPE;
import static org.komodo.shell.Messages.CreateCommand.NO_DUPLICATES_ALLOWED;
import static org.komodo.shell.Messages.CreateCommand.OBJECT_CREATED;
import static org.komodo.shell.Messages.CreateCommand.PATH_NOT_FOUND;
import static org.komodo.shell.Messages.CreateCommand.PROPERTY_ALREADY_EXISTS;
import static org.komodo.shell.Messages.CreateCommand.PROPERTY_CREATED;
import static org.komodo.shell.Messages.CreateCommand.TOO_MANY_ARGS;
import static org.komodo.shell.Messages.CreateCommand.TYPE_NOT_VALID;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.FindWorkspaceNodeVisitor;
import org.komodo.shell.Messages;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Creates various Vdb and relational objects based on command line inputs.
 */
public class CreateCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "create"; //$NON-NLS-1$

    private static final String PROPERTY = "property";  //$NON-NLS-1$

    private static final Set< KomodoType > NOT_SUPPORTED;

    static {
        final Set< KomodoType > temp = new HashSet<>();
        temp.add( KomodoType.TEIID );
        temp.add( KomodoType.UNKNOWN );
        temp.add( KomodoType.WORKSPACE );
        NOT_SUPPORTED = Collections.unmodifiableSet( temp );
    }

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public CreateCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        // type is only arg required by all commands
        final String typeArg = requiredArgument( 0, Messages.getString( MISSING_OBJ_TYPE ) );
        final int numOptionalArgs = ( getArguments().size() - 1 );
        final String[] optionalArgs = new String[ numOptionalArgs ];

        if ( numOptionalArgs > 0 ) {
            for ( int i = 0; i < numOptionalArgs; ++i ) {
                optionalArgs[ i ] = optionalArgument( i + 1 );
            }
        }

        try {
            final KomodoObject kobject = create( typeArg, optionalArgs );

            // Commit transaction
            if ( isAutoCommit() ) {
                getWorkspaceStatus().commit( CreateCommand.class.getSimpleName() );
            }

            if( PROPERTY.equals(typeArg.toLowerCase()) ) {
                final String typeName = requiredArgument( 1, Messages.getString( MISSING_OBJ_TYPE ) );
                // Print message
                print( MESSAGE_INDENT,
            			Messages.getString( PROPERTY_CREATED, typeName ) );
            } else {
            	// Print message
            	print( MESSAGE_INDENT,
            			Messages.getString( OBJECT_CREATED, typeArg, kobject.getName( getWorkspaceStatus().getTransaction() ) ) );
            }
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, Messages.getString( FAILURE, typeArg, e.getLocalizedMessage() ) );
            return false;
        }

        return true;
    }

    private KomodoObject doCreate( final KomodoObject parent,
                                   final String objName,
                                   final KomodoType type,
                                   final RelationalProperty... properties ) throws Exception {
        final WorkspaceStatus wsStatus = getWorkspaceStatus();
        final UnitOfWork uow = wsStatus.getTransaction();
        final WorkspaceManager wkspManager = getWorkspaceStatus().getCurrentContext().getWorkspaceManager();

        // make sure child type is valid for this parent
        if ( !validateChildType( type.getType(), ContextUtils.getContextForPath( wsStatus, parent.getAbsolutePath() ) ) ) {
            throw new Exception( Messages.getString( "BuiltInShellCommand.typeArg_childTypeNotAllowed", type, parent.getAbsolutePath() ) ); //$NON-NLS-1$
        }

        // make sure name is valid
        if ( !validateObjectName( objName, type ) ) {
            throw new Exception( Messages.getString( "BuiltInShellCommand.objectNameNotValid", objName ) ); //$NON-NLS-1$
        }

        // don't allow objects with same name and type
        final WorkspaceContext parentContext = ContextUtils.getContextForPath( wsStatus, parent.getAbsolutePath() );
        final WorkspaceContext childContext = parentContext.getChild( objName, type.getType() );

        if ( childContext != null ) {
            throw new Exception( Messages.getString( NO_DUPLICATES_ALLOWED, objName, type.getType() ) );
        }

        return wkspManager.create( uow, parent, objName, type, properties );
    }

    private List< String > findTables() throws Exception {
        final FindCommand findCmd = new FindCommand( getWorkspaceStatus() );
        findCmd.setOutput( getWriter() );

        try {
            findCmd.setArguments( new Arguments( StringConstants.EMPTY_STRING ) );
        } catch ( final Exception e ) {
            // only occurs on parsing error of arguments and we have no arguments
        }

        final String[] tablePaths = findCmd.query( KomodoType.TABLE, null, null );

        if ( tablePaths.length == 0 ) {
            return Collections.emptyList();
        }

        final String currentTablePath = ContextUtils.convertPathToDisplayPath( getContext().getKomodoObj().getAbsolutePath() );
        final List< String > result = new ArrayList< >( tablePaths.length );
        boolean found = false;

        for ( final String path : tablePaths ) {
            final String displayPath = ContextUtils.convertPathToDisplayPath( path );

            if ( !found && currentTablePath.equals( displayPath ) ) {
                found = true;
                continue;
            }

            result.add( displayPath );
        }

        return result;
    }

    private KomodoObject create( final String objType,
                                 final String... args ) throws Exception {
        assert !StringUtils.isBlank( objType );
        KomodoObject result = null;

        final KomodoType kType = KomodoType.getKomodoType( objType );

        switch ( kType ) {
            case ACCESS_PATTERN:
            case COLUMN:
            case VDB_CONDITION:
            case VDB_DATA_ROLE:
            case PUSHDOWN_FUNCTION:
            case INDEX:
            case VDB_MASK:
            case MODEL:
            case VDB_MODEL_SOURCE:
            case PARAMETER:
            case VDB_PERMISSION:
            case PRIMARY_KEY:
            case STORED_PROCEDURE:
            case RESULT_SET_COLUMN:
            case SCHEMA:
            case TABLE:
            case TEIID:
            case UNIQUE_CONSTRAINT:
            case USER_DEFINED_FUNCTION:
            case VDB_IMPORT:
            case VIRTUAL_PROCEDURE:
            case VIEW: {
                // must have an object name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                // may have a parent path
                if ( args.length > 2 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final String objName = args[ 0 ];
                final KomodoObject parent = getParent( 1, args );
                result = doCreate( parent, objName, kType );

                break;
            }
            case DATA_TYPE_RESULT_SET:
            case TABULAR_RESULT_SET: {
                // may have a parent path
                if ( args.length > 1 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final KomodoObject parent = getParent( 0, args );
                result = doCreate( parent, CreateProcedure.RESULT_SET, kType );

                break;
            }
            case VDB_ENTRY: {
                // must have an entry name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                // must have an entry path
                if ( args.length == 1 ) {
                    throw new Exception( Messages.getString( MISSING_ENTRY_PATH ) );
                }

                // may have a parent path
                if ( args.length > 3 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final String entryName = args[ 0 ];
                final String entryPath = args[ 1 ];
                final KomodoObject parent = getParent( 2, args );
                result = doCreate( parent, entryName, kType, new RelationalProperty( VdbLexicon.Entry.PATH, entryPath ) );

                break;
            }
            case FOREIGN_KEY: {
                // must have a foreign key name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                // must have a path of the table reference
                if ( args.length == 1 ) {
                    throw new Exception( Messages.getString( MISSING_FOREIGN_KEY_TABLE_REF ) );
                }

                // may have a parent path
                if ( args.length > 3 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final String fkName = args[ 0 ];
                final String tableRefPath = args[ 1 ];
                final KomodoObject parent = getParent( 2, args );

                // find object at path
                final FindWorkspaceNodeVisitor visitor = new FindWorkspaceNodeVisitor( tableRefPath );
                visitor.visit( getWorkspaceStatus().getWorkspaceContext() );
                final WorkspaceContext otherTableContext = visitor.getNodeContext();

                if ( otherTableContext == null ) {
                    throw new Exception( Messages.getString( PATH_NOT_FOUND, tableRefPath ) );
                }

                result = doCreate( parent,
                                   fkName,
                                   kType,
                                   new RelationalProperty( TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT,
                                                           otherTableContext.getKomodoObj() ) );
                break;
            }
            case STATEMENT_OPTION: {
                // must have an option name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                // must have an option value
                if ( args.length == 1 ) {
                    throw new Exception( Messages.getString( MISSING_OPTION_VALUE ) );
                }

                // may have a parent path
                if ( args.length > 3 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final String optionName = args[ 0 ];
                final String optionValue = args[ 1 ];
                final KomodoObject parent = getParent( 2, args );
                result = doCreate( parent, optionName, kType, new RelationalProperty( StandardDdlLexicon.VALUE, optionValue ) );

                break;
            }
            case VDB_TRANSLATOR: {
                // must have a translator name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                // must have a translator type
                if ( args.length == 1 ) {
                    throw new Exception( Messages.getString( MISSING_TRANSLATOR_TYPE ) );
                }

                // may have a parent path
                if ( args.length > 3 ) {
                    throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                }

                final String translatorName = args[ 0 ];
                final String transType = args[ 1 ];
                final KomodoObject parent = getParent( 2, args );
                result = doCreate( parent, translatorName, kType, new RelationalProperty( VdbLexicon.Translator.TYPE, transType ) );

                break;
            }
            case VDB: {
                // must have a VDB name
                if ( args.length == 0 ) {
                    throw new Exception( Messages.getString( MISSING_OBJ_NAME, objType ) );
                }

                final String vdbName = args[ 0 ];
                String filePath = Messages.getString( DEFAULT_VDB_FILE_PATH );

                // may have an external file path
                if ( args.length > 1 ) {
                    filePath = args[ 1 ];

                    // may have parent path
                    if ( args.length > 3 ) {
                        throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                    }
                }

                final KomodoObject parent = getParent( 2, args );
                result = doCreate( parent, vdbName, kType, new RelationalProperty( VdbLexicon.Vdb.ORIGINAL_FILE, filePath ) );

                break;
            }
            case UNKNOWN:
            default:
            	// Allows Custom properties to be created
            	if( objType!=null && PROPERTY.equals(objType.toLowerCase()) ) {
                    // must have a property name
                    if ( args.length == 0 ) {
                        throw new Exception( Messages.getString( MISSING_PROPERTY_NAME ) );
                    }

                    // must have a property value
                    if ( args.length == 1 ) {
                        throw new Exception( Messages.getString( MISSING_PROPERTY_VALUE ) );
                    }

                    // may have a parent path
                    if ( args.length > 2 ) {
                        throw new Exception( Messages.getString( TOO_MANY_ARGS, objType ) );
                    }

                    final String propName = args[ 0 ];
                    final String propValue = args[ 1 ];
                    WorkspaceContext context = getWorkspaceStatus().getCurrentContext();
            		// Can create if it does not already exist
            		if (!validateProperty(propName,context,false)) {
                        context.setPropertyValue( propName, propValue );
                	// Already exists - cannot create
            		} else {
                        throw new Exception( Messages.getString( PROPERTY_ALREADY_EXISTS, propName ) );
            		}
            	} else {
            		throw new Exception( Messages.getString( TYPE_NOT_VALID, objType ) );
            	}
        }

        return result;
    }

    private KomodoObject getParent( final int index,
                                    final String[] args ) throws Exception {
        assert ( args != null );

        // if no path argument return current KomodoObject
        if ( index >= args.length ) {
            final WorkspaceContext context = getWorkspaceStatus().getCurrentContext();
            return context.getKomodoObj();
        }

        // there is a path arg
        final WorkspaceContext context = ContextUtils.getContextForPath( getWorkspaceStatus(), args[ index ] );

        // path is invalid
        if ( context == null ) {
            throw new Exception( Messages.getString( "BuiltInShellCommand.locationArg_noContextWithThisName", args[ index ] ) ); //$NON-NLS-1$
        }

        // return object at designated path
        return context.getKomodoObj();
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( String lastArgument,
                              List< CharSequence > candidates ) throws Exception {
        if ( getArguments().isEmpty() ) {
            // Get list of valid child types for current context
            final KomodoObject kObj = getWorkspaceStatus().getCurrentContext().getKomodoObj();

            for ( final KomodoType kType : kObj.getChildTypes() ) {
                if ( NOT_SUPPORTED.contains( kType ) ) {
                    continue;
                }

                final String type = kType.getType();

                if ( ( lastArgument == null ) || ( type.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) ) {
                    candidates.add( type );
                }
            }

            // Allow creation of custom properties everywhere except workspace node
            String contextType = getWorkspaceStatus().getCurrentContext().getType();
           	if( !WorkspaceStatus.WORKSPACE_TYPE.equals(contextType.toUpperCase()) ) {
                if ( ( lastArgument == null ) || ( PROPERTY.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) ) {
                    candidates.add( PROPERTY );
                }
        	}

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        if ( getArguments().size() == 1 ) {
            return -1;
        }

        if ( getArguments().size() == 2 ) {
            final String typeArg = getArguments().get( 0 ).toUpperCase();

            // foreign key requires referenced table path
            if ( typeArg.equals( KomodoType.FOREIGN_KEY.getType().toUpperCase() ) ) {
                // find all tables not including the current parent object
                final List< String > tablePaths = findTables();

                if ( !tablePaths.isEmpty() ) {
                    for ( final String path : tablePaths ) {
                        if ( StringUtils.isBlank( lastArgument ) || ( path.startsWith( lastArgument ) ) ) {
                            candidates.add( path );
                        }
                    }
                }

                return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
            }

            // The arg is expected to be a path if not one of these types. These types require an additional arg.
            if ( !typeArg.equals( KomodoType.VDB_ENTRY.getType().toUpperCase() )
                 && !typeArg.equals( KomodoType.STATEMENT_OPTION.getType().toUpperCase() )
                 && !typeArg.equals( KomodoType.VDB_TRANSLATOR.getType().toUpperCase() )
                 && !typeArg.equals( KomodoType.VDB.getType().toUpperCase() ) ) {
                updateTabCompleteCandidatesForPath( candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument );

                // Do not put space after it - may want to append more to the path
                return NO_APPEND_SEPARATOR;
            }

            return -1;
        }

        if ( getArguments().size() == 3 ) {
            final String typeArg = getArguments().get( 0 ).toUpperCase();

            // these types can have a path as last arg
            if ( typeArg.equals( KomodoType.VDB_ENTRY.getType().toUpperCase() )
                 || typeArg.equals( KomodoType.STATEMENT_OPTION.getType().toUpperCase() )
                 || typeArg.equals( KomodoType.VDB_TRANSLATOR.getType().toUpperCase() )
                 || typeArg.equals( KomodoType.VDB.getType().toUpperCase() ) ) {
                updateTabCompleteCandidatesForPath( candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument );

                // Do not put space after it - may want to append more to the path
                return NO_APPEND_SEPARATOR;
            }
        }

        return -1;
    }

}
