/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb.datarole;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.relational.commands.vdb.datarole.DataRoleCommandMessages.General.MISSING_PERMISSION_NAME;
import static org.komodo.relational.commands.vdb.datarole.DataRoleCommandMessages.AddPermissionCommand.PERMISSION_ADDED;
import org.komodo.relational.vdb.DataRole;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String permissionName = requiredArgument( 0, getMessage(MISSING_PERMISSION_NAME) );

        final DataRole dataRole = getDataRole();
        dataRole.addPermission( getTransaction(), permissionName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(PERMISSION_ADDED,permissionName));
        
        return true;
    }

}
