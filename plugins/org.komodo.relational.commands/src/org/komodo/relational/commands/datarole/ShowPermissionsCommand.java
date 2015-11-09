/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.ShowPermissionsCommand.NO_PERMISSIONS;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.ShowPermissionsCommand.PERMISSIONS_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all {@link Permission permissions} of a {@link DataRole}.
 */
public final class ShowPermissionsCommand extends DataRoleShellCommand {

    static final String NAME = "show-permissions"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowPermissionsCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final DataRole dataRole = getDataRole();
            final Permission[] permissions = dataRole.getPermissions( getTransaction() );

            if ( permissions.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_PERMISSIONS, dataRole.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( PERMISSIONS_HEADER, dataRole.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final Permission permission : permissions ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                permission.getName( getTransaction() ),
                                                permission.getTypeDisplayName() ) );
                }
            }

            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
    }

}
