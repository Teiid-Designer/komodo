/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.MISSING_PERMISSION_NAME;
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
        final String permissionName = requiredArgument( 0, MISSING_PERMISSION_NAME.getMessage() );

        final DataRole dataRole = getDataRole();
        dataRole.addPermission( getTransaction(), permissionName );

        return true;
    }

}
