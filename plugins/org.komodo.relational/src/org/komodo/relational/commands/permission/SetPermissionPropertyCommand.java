/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Permission properties
 */
public final class SetPermissionPropertyCommand extends PermissionShellCommand {

    static final String NAME = "set-permission-property"; //$NON-NLS-1$

    private static final String ALLOW_ALTER = "allow-alter"; //$NON-NLS-1$
    private static final String ALLOW_CREATE = "allow-create"; //$NON-NLS-1$
    private static final String ALLOW_DELETE = "allow-delete"; //$NON-NLS-1$
    private static final String ALLOW_EXECUTE = "allow-execute"; //$NON-NLS-1$
    private static final String ALLOW_LANGUAGE = "allow-language"; //$NON-NLS-1$
    private static final String ALLOW_READ = "allow-read"; //$NON-NLS-1$
    private static final String ALLOW_UPDATE = "allow-update"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOW_ALTER, ALLOW_CREATE, ALLOW_DELETE, ALLOW_EXECUTE,
                                                                                  ALLOW_LANGUAGE, ALLOW_READ, ALLOW_UPDATE } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetPermissionPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final Permission permission = getPermission();
        
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case ALLOW_ALTER:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_ALTER ) );
                    success = false;
                }

                break;
            case ALLOW_CREATE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_CREATE ) );
                    success = false;
                }

                break;
            case ALLOW_DELETE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_DELETE ) );
                    success = false;
                }

                break;
            case ALLOW_EXECUTE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_EXECUTE ) );
                    success = false;
                }

                break;
            case ALLOW_LANGUAGE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_LANGUAGE ) );
                    success = false;
                }

                break;
            case ALLOW_READ:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_READ ) );
                    success = false;
                }

                break;
            case ALLOW_UPDATE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOW_UPDATE ) );
                    success = false;
                }

                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

        return success;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }
        
        if ( ( args.size() == 1 ) ) {
            String theArg = getArguments().get(0);
            if( ALLOW_ALTER.equals(theArg) || ALLOW_CREATE.equals(theArg) || ALLOW_DELETE.equals(theArg) || ALLOW_EXECUTE.equals(theArg) || 
                ALLOW_LANGUAGE.equals(theArg) || ALLOW_READ.equals(theArg) || ALLOW_UPDATE.equals(theArg) ) {
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
            }

            return 0;
        }
        
        // no tab completion
        return -1;
    }

}
