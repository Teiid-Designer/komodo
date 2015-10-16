/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.AddPermissionCommand.PERMISSION_ADDED;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.General.MISSING_PERMISSION_NAME;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a permission to a {@link DataRole}.
 */
public final class AddPermissionCommand extends DataRoleShellCommand {

    static final String NAME = "add-permission"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddPermissionCommand( final WorkspaceStatus status ) {
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
            final String permissionName = requiredArgument( 0, getMessage( MISSING_PERMISSION_NAME ) );

            final DataRole dataRole = getDataRole();
            dataRole.addPermission( getTransaction(), permissionName );

            result = new CommandResultImpl( getMessage( PERMISSION_ADDED, permissionName ) );
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

}
