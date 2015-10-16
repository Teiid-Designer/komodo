/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_MISSING_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_PROPERTY_SUCCESS;
import java.util.List;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to unset {@link Permission permission} properties.
 */
public final class UnsetPermissionPropertyCommand extends PermissionShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetPermissionPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, getWorkspaceMessage( UNSET_MISSING_PROPERTY_NAME ) );
            final Permission permission = getPermission();

            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case ALLOW_ALTER:
                    permission.setAllowAlter( transaction, Permission.DEFAULT_ALLOW_ALTER );
                    break;
                case ALLOW_CREATE:
                    permission.setAllowCreate( transaction, Permission.DEFAULT_ALLOW_CREATE );
                    break;
                case ALLOW_DELETE:
                    permission.setAllowDelete( transaction, Permission.DEFAULT_ALLOW_DELETE );
                    break;
                case ALLOW_EXECUTE:
                    permission.setAllowExecute( transaction, Permission.DEFAULT_ALLOW_EXECUTE );
                    break;
                case ALLOW_LANGUAGE:
                    permission.setAllowLanguage( transaction, Permission.DEFAULT_ALLOW_LANGUAGE );
                    break;
                case ALLOW_READ:
                    permission.setAllowRead( transaction, Permission.DEFAULT_ALLOW_READ );
                    break;
                case ALLOW_UPDATE:
                    permission.setAllowUpdate( transaction, Permission.DEFAULT_ALLOW_UPDATE );
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Permission.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getWorkspaceMessage( UNSET_PROPERTY_SUCCESS, name ) );
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
        return 1;
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

        // no tab completion
        return -1;
    }

}
