/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.permission.PermissionCommandMessages.AddMaskCommand.MASK_ADDED;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.General.MISSING_MASK_NAME;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Mask to a Permission.
 */
public final class AddMaskCommand extends PermissionShellCommand {

    static final String NAME = "add-mask"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddMaskCommand( final WorkspaceStatus status ) {
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
            final String maskName = requiredArgument( 0, getMessage( MISSING_MASK_NAME ) );

            final Permission permission = getPermission();
            permission.addMask( getTransaction(), maskName );

            result = new CommandResultImpl( getMessage( MASK_ADDED, maskName ) );
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
