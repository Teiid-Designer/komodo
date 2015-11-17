/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set Permission properties
 */
public final class SetPermissionPropertyCommand extends PermissionShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetPermissionPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final String name = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingPropertyNameValue ) );
            final String value = requiredArgument( 1, I18n.bind( WorkspaceCommandsI18n.missingPropertyNameValue ) );

            final Permission permission = getPermission();

            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case ALLOW_ALTER:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_ALTER );
                    }

                    break;
                case ALLOW_CREATE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_CREATE );
                    }

                    break;
                case ALLOW_DELETE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_DELETE );
                    }

                    break;
                case ALLOW_EXECUTE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_EXECUTE );
                    }

                    break;
                case ALLOW_LANGUAGE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_LANGUAGE );
                    }

                    break;
                case ALLOW_READ:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_READ );
                    }

                    break;
                case ALLOW_UPDATE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_UPDATE );
                    }

                    break;
                default:
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, Permission.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.setPropertySuccess, name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyUsage ) );
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
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        // no tab completion
        return -1;
    }

}
